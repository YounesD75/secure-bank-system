package securebank
import securebank.analytics.{SecurityEvent, ParquetEventWriter}

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import Protocols._

object TokenStore {

  final case class State(
    activeTokens:   Map[String, JwtToken] = Map.empty, // tokenValue → JwtToken
    failedAttempts: Map[String, Int]      = Map.empty, // user → nb échecs
    lockedAccounts: Set[String]           = Set.empty,
    revokedTokens:  Set[String]           = Set.empty  // liste noire (tokenValue)
  )

  def apply(): Behavior[TokenCommand] = store(State())

  private def store(state: State): Behavior[TokenCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        // ─ T3 : StoreToken — P2 → P4 (Token Actif) ───────────────────────
        case StoreToken(token) =>
          ctx.log.info(s"[TokenStore] Token stocké — ${token.username} : $token")
          store(state.copy(
            activeTokens   = state.activeTokens + (token.value -> token),
            failedAttempts = state.failedAttempts - token.username // reset échecs
          ))

        // ─ T4 check : ValidateToken ───────────────────────────────────────
        case ValidateToken(token, replyTo) =>
          val result = if (state.revokedTokens.contains(token.value)) {
            ctx.log.warn(s"[TokenStore] Token révoqué — ${token.username}")
            TokenInvalid
          } else if (!state.activeTokens.contains(token.value)) {
            ctx.log.warn(s"[TokenStore] Token inconnu")
            TokenInvalid
          } else if (token.isExpired) {
            // Nettoyage automatique du token expiré
            ctx.log.warn(s"[TokenStore] Token expiré — ${token.username}")
            TokenInvalid
          } else {
            ctx.log.info(s"[TokenStore] Token valide — ${token.username}")
            TokenValid
          }
          replyTo ! result
          // Nettoyage si expiré
          if (token.isExpired)
            store(state.copy(activeTokens = state.activeTokens - token.value))
          else
            Behaviors.same

        // ─ T2 : ReportFailure — accumulation P3 → éventuel T7 ────────────
        case ReportFailure(user, replyTo) =>

          ParquetEventWriter.write(
            SecurityEvent("AUTH_FAILURE", user, System.currentTimeMillis())
          )

          val attempts = state.failedAttempts.getOrElse(user, 0) + 1
          ctx.log.warn(s"[TokenStore] Échec auth — $user : $attempts/3")

          if (attempts >= 3) {

            ParquetEventWriter.write(
              SecurityEvent("ACCOUNT_LOCKED", user, System.currentTimeMillis())
            )
            // ─ T7 : P3×3 → P7 Compte Bloqué ─
            ctx.log.error(s"[TokenStore] COMPTE BLOQUÉ — $user (T7 franchie)")
            replyTo ! AccountLocked
            store(state.copy(
              failedAttempts = state.failedAttempts + (user -> attempts),
              lockedAccounts = state.lockedAccounts + user
            ))
          } else {
            replyTo ! AuthFailure("Identifiants invalides", attempts)
            store(state.copy(failedAttempts = state.failedAttempts + (user -> attempts)))
          }

        // ─ T6 : RevokeToken — P4 → P6 (Token Révoqué) ────────────────────
        case RevokeToken(token) =>

          ParquetEventWriter.write(
            SecurityEvent("TOKEN_REVOKED", token.username, System.currentTimeMillis())
          )
          ctx.log.warn(s"[TokenStore] Révocation — $token")
          store(state.copy(
            activeTokens  = state.activeTokens - token.value,
            revokedTokens = state.revokedTokens + token.value
          ))

        // ─ Vérification lock — utilisé par AuthServer avant chaque auth ───
        case IsAccountLocked(user, replyTo) =>
          replyTo ! state.lockedAccounts.contains(user)
          Behaviors.same
      }
    }
}