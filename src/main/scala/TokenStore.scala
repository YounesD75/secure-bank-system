package securebank
import securebank.analytics.SecurityEvent

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import Protocols._

object TokenStore {

  final case class State(
    activeTokens:   Map[String, JwtToken] = Map.empty, 
    failedAttempts: Map[String, Int]      = Map.empty, 
    lockedAccounts: Set[String]           = Set.empty,
    revokedTokens:  Set[String]           = Set.empty  
  )

  def apply(eventWriter: SecurityEvent => Unit): Behavior[TokenCommand] = store(State(), eventWriter)

  private def store(state: State, eventWriter: SecurityEvent => Unit): Behavior[TokenCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        case StoreToken(token) =>
          ctx.log.info(s"[TokenStore] Token stocké — ${token.username} : $token")
          store(state.copy(
            activeTokens   = state.activeTokens + (token.value -> token),
            failedAttempts = state.failedAttempts - token.username 
          ), eventWriter)

        case ValidateToken(token, replyTo) =>
          val result = if (state.revokedTokens.contains(token.value)) {
            ctx.log.warn(s"[TokenStore] Token révoqué — ${token.username}")
            TokenInvalid
          } else if (!state.activeTokens.contains(token.value)) {
            ctx.log.warn(s"[TokenStore] Token inconnu")
            TokenInvalid
          } else if (token.isExpired) {
            ctx.log.warn(s"[TokenStore] Token expiré — ${token.username}")
            TokenInvalid
          } else {
            ctx.log.info(s"[TokenStore] Token valide — ${token.username}")
            TokenValid
          }
          replyTo ! result
          
          if (token.isExpired)
            store(state.copy(activeTokens = state.activeTokens - token.value), eventWriter)
          else
            Behaviors.same

        case ReportFailure(user, replyTo) =>

          eventWriter(SecurityEvent("AUTH_FAILURE", user, System.currentTimeMillis()))

          val attempts = state.failedAttempts.getOrElse(user, 0) + 1
          ctx.log.warn(s"[TokenStore] Échec auth — $user : $attempts/3")

          if (attempts >= 3) {

            eventWriter(SecurityEvent("ACCOUNT_LOCKED", user, System.currentTimeMillis()))
            
            ctx.log.error(s"[TokenStore] COMPTE BLOQUÉ — $user (T7 franchie)")
            replyTo ! AccountLocked
            store(state.copy(
              failedAttempts = state.failedAttempts + (user -> attempts),
              lockedAccounts = state.lockedAccounts + user
            ), eventWriter)
          } else {
            replyTo ! AuthFailure("Identifiants invalides", attempts)
            store(state.copy(failedAttempts = state.failedAttempts + (user -> attempts)), eventWriter)
          }

        case RevokeToken(token) =>

          eventWriter(SecurityEvent("TOKEN_REVOKED", token.username, System.currentTimeMillis()))
          
          ctx.log.warn(s"[TokenStore] Révocation — $token")
          store(state.copy(
            activeTokens  = state.activeTokens - token.value,
            revokedTokens = state.revokedTokens + token.value
          ), eventWriter)

        case IsAccountLocked(user, replyTo) =>
          replyTo ! state.lockedAccounts.contains(user)
          Behaviors.same
      }
    }
}