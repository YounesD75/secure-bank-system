import akka.actor.typed.{Behavior, ActorRef}
import akka.actor.typed.scaladsl.Behaviors
import Protocols._
import java.util.UUID

// ─────────────────────────────────────────────────────────────────────────────
// AuthServer.scala
//
// Rôle : vérifier les credentials et émettre des tokens.
// BUGFIX vs Rayan : vérifie le lock AVANT auth, envoie AccountLocked
// ─────────────────────────────────────────────────────────────────────────────

object AuthServer {

  private val validUsers = Map(
    "admin" -> "password123",
    "alice" -> "pwd123",
    "bob"   -> "secret"
  )

  def apply(tokenStore: ActorRef[TokenCommand]): Behavior[AuthCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        case Authenticate(user, pass, replyTo) =>
          ctx.log.info(s"[AuthServer] Connexion demandée — $user")

          // Acteur anonyme qui reçoit la réponse IsAccountLocked
          val lockChecker = ctx.spawnAnonymous(
            Behaviors.receiveMessage[Boolean] { isLocked =>
              if (isLocked) {
                ctx.log.warn(s"[AuthServer] Compte bloqué — accès refusé : $user")
                replyTo ! AccountLocked
              } else if (validUsers.get(user).contains(pass)) {
                // ─ T1 : Auth OK → token émis ─
                val token = UUID.randomUUID().toString
                tokenStore ! StoreToken(user, token)
                ctx.log.info(s"[AuthServer] Auth réussie — token émis pour $user")
                replyTo ! AuthSuccess(token)
              } else {
                // ─ T2 : Auth KO → TokenStore incrémente P3, renvoie la réponse ─
                ctx.log.warn(s"[AuthServer] Credentials invalides pour $user")
                tokenStore ! ReportFailure(user, replyTo)
              }
              Behaviors.stopped
            }
          )
          tokenStore ! IsAccountLocked(user, lockChecker)
          Behaviors.same

        case ValidateToken(token, replyTo) =>
          ctx.log.info(s"[AuthServer] Validation token ${token.take(8)}...")
          tokenStore ! Protocols.ValidateToken(token, replyTo)
          Behaviors.same

        case RevokeTokenAuth(token, replyTo) =>
          ctx.log.warn(s"[AuthServer] Révocation token ${token.take(8)}...")
          tokenStore ! RevokeToken(token)
          replyTo ! TokenRevoked
          Behaviors.same
      }
    }
}
