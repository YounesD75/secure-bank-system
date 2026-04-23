package securebank

import akka.actor.typed.{Behavior, ActorRef}
import akka.actor.typed.scaladsl.Behaviors
import Protocols._
import java.util.UUID
import scala.concurrent.duration._

object AuthServer {

  private val TOKEN_TTL_MS = 30000L  // 30 secondes

  private val validUsers = Map(
    "admin" -> "password123",
    "alice" -> "pwd123",
    "bob"   -> "secret"
  )

  def apply(tokenStore: ActorRef[TokenCommand]): Behavior[AuthCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        // ─ T0/T1/T2 : Authenticate ────────────────────────────────────────
        case Authenticate(user, pass, replyTo) =>
          ctx.log.info(s"[AuthServer] Connexion demandée — $user")

          // FIX: l'acteur anonyme a son propre ctx (innerCtx), pas celui du parent
          val lockChecker = ctx.spawnAnonymous(
            Behaviors.receive[Boolean] { (innerCtx, isLocked) =>
              if (isLocked) {
                innerCtx.log.warn(s"[AuthServer] Compte bloqué — $user")
                replyTo ! AccountLocked
              } else if (validUsers.get(user).contains(pass)) {
                // ─ T1 : Auth OK → JWT émis ─
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
                // ─ T2 : Auth KO → TokenStore incrémente P3 ─
                innerCtx.log.warn(s"[AuthServer] Credentials invalides — $user")
                tokenStore ! ReportFailure(user, replyTo)
              }
              Behaviors.stopped
            }
          )
          tokenStore ! IsAccountLocked(user, lockChecker)
          Behaviors.same

        // ─ T4 check : CheckToken → délégué au TokenStore ──────────────────
        case CheckToken(token, replyTo) =>
          ctx.log.info(s"[AuthServer] Validation token — $token")
          tokenStore ! ValidateToken(token, replyTo)
          Behaviors.same

        // ─ T6 : RevokeTokenAuth → P6 (Token Révoqué) ─────────────────────
        case RevokeTokenAuth(token, replyTo) =>
          ctx.log.warn(s"[AuthServer] Révocation — $token")
          tokenStore ! RevokeToken(token)
          replyTo ! TokenRevoked
          Behaviors.same
      }
    }
}
