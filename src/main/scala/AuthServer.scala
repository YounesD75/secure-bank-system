package securebank

import akka.actor.typed.{Behavior, ActorRef}
import akka.actor.typed.scaladsl.Behaviors
import Protocols._
import java.util.UUID
import scala.concurrent.duration._

import securebank.analytics.SecurityEvent

object AuthServer {

  private val TOKEN_TTL_MS = 30000L  

  private val validUsers = Map(
    "admin" -> "password123",
    "alice" -> "pwd123",
    "bob"   -> "secret"
  )

  def apply(tokenStore: ActorRef[TokenCommand], eventWriter: SecurityEvent => Unit): Behavior[AuthCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        case Authenticate(user, pass, replyTo) =>
          ctx.log.info(s"[AuthServer] Connexion demandée — $user")

          val lockChecker = ctx.spawnAnonymous(
            Behaviors.receive[Boolean] { (innerCtx, isLocked) =>
              if (isLocked) {
                innerCtx.log.warn(s"[AuthServer] Compte bloqué — $user")
                replyTo ! AccountLocked
              } else if (validUsers.get(user).contains(pass)) {
                eventWriter(SecurityEvent("AUTH_SUCCESS", user, System.currentTimeMillis()))
                
                val now   = System.currentTimeMillis()
                val token = JwtToken(
                  value     = s"${UUID.randomUUID().toString.replace("-", "")}",
                  username  = user,
                  issuedAt  = now,
                  expiresAt = now + TOKEN_TTL_MS
                )
                tokenStore ! StoreToken(token)
                innerCtx.log.info(s"[AuthServer] Auth réussie — token émis : $token")
                replyTo ! AuthSuccess(token)
              } else {
                innerCtx.log.warn(s"[AuthServer] Credentials invalides — $user")
                tokenStore ! ReportFailure(user, replyTo)
              }
              Behaviors.stopped
            }
          )
          tokenStore ! IsAccountLocked(user, lockChecker)
          Behaviors.same

        case CheckToken(token, replyTo) =>
          ctx.log.info(s"[AuthServer] Validation token — $token")
          tokenStore ! ValidateToken(token, replyTo)
          Behaviors.same

        case RevokeTokenAuth(token, replyTo) =>
          eventWriter(SecurityEvent("TOKEN_REVOKED", token.username, System.currentTimeMillis()))
          
          ctx.log.warn(s"[AuthServer] Révocation — $token")
          tokenStore ! RevokeToken(token)
          replyTo ! TokenRevoked
          Behaviors.same
      }
    }
}