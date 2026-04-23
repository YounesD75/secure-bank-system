import akka.actor.typed.{Behavior, ActorRef}
import akka.actor.typed.scaladsl.Behaviors
import Protocols._

// ─────────────────────────────────────────────────────────────────────────────
// Client.scala — NOUVEL ACTEUR (session + test)
//
// Machine à états qui suit exactement les places du réseau de Pétri :
//   P0 → idle         (état initial)
//   P1 → awaitingAuth (requête envoyée, en attente)
//   P4/P5 → inSession (token valide, session ouverte)
//   P7 → blocked      (état absorbant — plus aucune action possible)
// ─────────────────────────────────────────────────────────────────────────────

object Client {

  def apply(
    username:   String,
    password:   String,
    authServer: ActorRef[AuthCommand]
  ): Behavior[ClientCommand] =
    idle(username, password, authServer)

  // ── P0 : Idle ─────────────────────────────────────────────────────────────
  private def idle(
    username:   String,
    password:   String,
    authServer: ActorRef[AuthCommand]
  ): Behavior[ClientCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        // ─ T0 : Soumettre — P0 → P1 ─
        case StartNormalLogin =>
          ctx.log.info(s"[Client:$username] Connexion avec credentials corrects...")
          val adapter = ctx.messageAdapter[AuthResponse](GotAuthResponse)
          authServer ! Authenticate(username, password, adapter)
          awaitingAuth(username, password, authServer)

        case StartBruteForce =>
          ctx.log.warn(s"[Client:$username] BRUTE-FORCE — tentative 1/3 avec mauvais password")
          val adapter = ctx.messageAdapter[AuthResponse](GotAuthResponse)
          authServer ! Authenticate(username, "WRONG_1", adapter)
          awaitingBrute(username, authServer, attempt = 1)

        case _ => Behaviors.same
      }
    }

  // ── P1 : AwaitingAuth (connexion normale) ─────────────────────────────────
  private def awaitingAuth(
    username:   String,
    password:   String,
    authServer: ActorRef[AuthCommand]
  ): Behavior[ClientCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        // ─ T1 : Auth OK → P4/P5 Session ouverte ─
        case GotAuthResponse(AuthSuccess(token)) =>
          ctx.log.info(s"[Client:$username] Authentifié — token reçu : ${token.take(8)}...")
          ctx.log.info(s"[Client:$username] Session active — accès au compte possible")
          inSession(username, token, authServer)

        // ─ T2 : Auth KO → P0 + P3 ─
        case GotAuthResponse(AuthFailure(reason, attempts)) =>
          ctx.log.warn(s"[Client:$username] Échec ($reason) — tentative $attempts/3")
          idle(username, password, authServer)

        case GotAuthResponse(AccountLocked) =>
          ctx.log.error(s"[Client:$username] Compte bloqué — impossible de se connecter")
          blocked(username)

        case _ => Behaviors.same
      }
    }

  // ── P1 : AwaitingAuth (brute-force) ──────────────────────────────────────
  private def awaitingBrute(
    username:   String,
    authServer: ActorRef[AuthCommand],
    attempt:    Int
  ): Behavior[ClientCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        case GotAuthResponse(AuthSuccess(token)) =>
          ctx.log.info(s"[Client:$username] Brute-force réussi (improbable) — token : ${token.take(8)}...")
          inSession(username, token, authServer)

        case GotAuthResponse(AuthFailure(_, attempts)) =>
          if (attempt < 3) {
            ctx.log.warn(s"[Client:$username] BRUTE-FORCE — tentative ${attempt + 1}/3")
            val adapter = ctx.messageAdapter[AuthResponse](GotAuthResponse)
            authServer ! Authenticate(username, s"WRONG_${attempt + 1}", adapter)
            awaitingBrute(username, authServer, attempt + 1)
          } else {
            ctx.log.error(s"[Client:$username] 3 échecs — en attente de blocage")
            idle(username, "WRONG", authServer)  // le prochain appel retournera AccountLocked
          }

        // ─ T7 : P3×3 → P7 Compte Bloqué ─
        case GotAuthResponse(AccountLocked) =>
          ctx.log.error(s"[Client:$username] COMPTE BLOQUÉ — fin du brute-force")
          blocked(username)

        case _ => Behaviors.same
      }
    }

  // ── P4/P5 : In Session ────────────────────────────────────────────────────
  private def inSession(
    username:   String,
    token:      String,
    authServer: ActorRef[AuthCommand]
  ): Behavior[ClientCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        // ─ T5 : Déconnecter — P5 → P0 ─
        case Disconnect =>
          ctx.log.info(s"[Client:$username] Déconnexion — retour à Idle")
          idle(username, "", authServer)

        case _ =>
          ctx.log.warn(s"[Client:$username] Action ignorée en session : $msg")
          Behaviors.same
      }
    }

  // ── P7 : Compte Bloqué (état absorbant) ───────────────────────────────────
  private def blocked(username: String): Behavior[ClientCommand] =
    Behaviors.receive { (ctx, msg) =>
      ctx.log.error(s"[Client:$username] BLOQUÉ — toute action ignorée : $msg")
      Behaviors.same
    }
}
