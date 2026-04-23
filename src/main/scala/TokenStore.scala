import akka.actor.typed.{Behavior, ActorRef}
import akka.actor.typed.scaladsl.Behaviors
import Protocols._

// ─────────────────────────────────────────────────────────────────────────────
// TokenStore.scala
//
// Version de Rayan — nettoyée et complétée :
//   AJOUT : IsAccountLocked (nécessaire pour le bugfix AuthServer)
//   AJOUT : ReportFailure envoie maintenant la réponse finale au client
//           (AuthFailure ou AccountLocked selon le nb d'échecs)
//   CONSERVÉ : toute la logique de Rayan (état immutable, pattern yield)
//
// Réseau de Pétri couvert :
//   P3 (Échecs) — comptage des jetons d'échec
//   T7 : P3×3 → P7 (Compte Bloqué) — déclenché quand attempts >= 3
//   P4 (Token Actif) → T6 → P6 (Token Révoqué)
// ─────────────────────────────────────────────────────────────────────────────

object TokenStore {

  case class State(
    activeTokens:   Map[String, String] = Map.empty,  // token → user
    failedAttempts: Map[String, Int]    = Map.empty,  // user  → nb échecs
    lockedAccounts: Set[String]         = Set.empty,
    revokedTokens:  Set[String]         = Set.empty   // liste noire
  )

  def apply(): Behavior[TokenCommand] = store(State())

  private def store(state: State): Behavior[TokenCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        // ── Stockage d'un token après auth réussie ─────────────────────────
        case StoreToken(user, token) =>
          ctx.log.info(s"[TokenStore] Token stocké pour $user")
          store(state.copy(
            activeTokens   = state.activeTokens + (token -> user),
            failedAttempts = state.failedAttempts - user  // reset échecs
          ))

        // ── Validation d'un token ──────────────────────────────────────────
        case ValidateToken(token, replyTo) =>
          if (state.revokedTokens.contains(token)) {
            ctx.log.warn(s"[TokenStore] Token révoqué — accès refusé")
            replyTo ! TokenInvalid
          } else if (state.activeTokens.contains(token)) {
            ctx.log.info(s"[TokenStore] Token valide")
            replyTo ! TokenValid
          } else {
            ctx.log.warn(s"[TokenStore] Token inconnu")
            replyTo ! TokenInvalid
          }
          Behaviors.same

        // ── Signalement d'un échec d'auth ──────────────────────────────────
        // BUGFIX : le TokenStore décide de la réponse (AuthFailure ou AccountLocked)
        case ReportFailure(user, replyTo) =>
          val attempts = state.failedAttempts.getOrElse(user, 0) + 1
          ctx.log.warn(s"[TokenStore] Échec pour $user : $attempts/3")

          if (attempts >= 3) {
            // ─ T7 : P3×3 → P7 Compte Bloqué ─
            ctx.log.error(s"[TokenStore] COMPTE BLOQUÉ — $user")
            replyTo ! AccountLocked
            store(state.copy(
              failedAttempts = state.failedAttempts + (user -> attempts),
              lockedAccounts = state.lockedAccounts + user
            ))
          } else {
            replyTo ! AuthFailure("Identifiants invalides", attempts)
            store(state.copy(failedAttempts = state.failedAttempts + (user -> attempts)))
          }

        // ── Révocation d'un token ──────────────────────────────────────────
        case RevokeToken(token) =>
          ctx.log.warn(s"[TokenStore] Révocation du token ${token.take(8)}...")
          store(state.copy(
            activeTokens  = state.activeTokens - token,
            revokedTokens = state.revokedTokens + token
          ))

        // ── Vérification lock — utilisé par AuthServer avant chaque auth ───
        case IsAccountLocked(user, replyTo) =>
          replyTo ! state.lockedAccounts.contains(user)
          Behaviors.same

        // ── CheckAndAuthenticate : non utilisé directement (placeholder) ───
        case CheckAndAuthenticate(_, _, _) =>
          Behaviors.same
      }
    }
}
