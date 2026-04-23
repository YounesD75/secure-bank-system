package securebank

import akka.actor.typed.{Behavior, ActorRef}
import akka.actor.typed.scaladsl.Behaviors
import Protocols._

object Client {

  // FIX : password conservé dans tous les états pour permettre la reconnexion
  def apply(
    username:   String,
    password:   String,
    authServer: ActorRef[AuthCommand]
  ): Behavior[ClientCommand] =
    idle(username, password, authServer)

  // ─ P0 : Idle ──────────────────────────────────────────────────────────────
  private def idle(
    username:   String,
    password:   String,
    authServer: ActorRef[AuthCommand]
  ): Behavior[ClientCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {
        case StartNormalLogin =>
          ctx.log.info(s"[Client:$username] Connexion normale...")
          val adapter = ctx.messageAdapter[AuthResponse](GotAuthResponse)
          authServer ! Authenticate(username, password, adapter)
          awaitingAuth(username, password, authServer)

        case StartBruteForce =>
          ctx.log.warn(s"[Client:$username] Brute-force — tentative 1/3")
          val adapter = ctx.messageAdapter[AuthResponse](GotAuthResponse)
          authServer ! Authenticate(username, "WRONG_1", adapter)
          awaitingBrute(username, password, authServer, attempt = 1)

        case _ => Behaviors.same
      }
    }

  // ─ P1 : AwaitingAuth (connexion normale) ──────────────────────────────────
  private def awaitingAuth(
    username:   String,
    password:   String,
    authServer: ActorRef[AuthCommand]
  ): Behavior[ClientCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {
        // ─ T1 : Auth OK → P4/P5 ─
        case GotAuthResponse(AuthSuccess(token)) =>
          ctx.log.info(s"[Client:$username] Authentifié — $token")
          ctx.log.info(s"[Client:$username] Session ouverte — accès au compte disponible")
          inSession(username, password, token, authServer)

        // ─ T2 : Auth KO → P0 + P3 ─
        case GotAuthResponse(AuthFailure(reason, attempts)) =>
          ctx.log.warn(s"[Client:$username] Échec ($reason) — $attempts/3")
          idle(username, password, authServer)  // FIX : password conservé

        case GotAuthResponse(AccountLocked) =>
          ctx.log.error(s"[Client:$username] Compte bloqué — état P7")
          blocked(username)

        case _ => Behaviors.same
      }
    }

  // ─ P1 : AwaitingAuth (brute-force) ────────────────────────────────────────
  private def awaitingBrute(
    username:   String,
    password:   String,
    authServer: ActorRef[AuthCommand],
    attempt:    Int
  ): Behavior[ClientCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {
        case GotAuthResponse(AuthFailure(_, _)) if attempt < 3 =>
          ctx.log.warn(s"[Client:$username] Brute-force — tentative ${attempt + 1}/3")
          val adapter = ctx.messageAdapter[AuthResponse](GotAuthResponse)
          authServer ! Authenticate(username, s"WRONG_${attempt + 1}", adapter)
          awaitingBrute(username, password, authServer, attempt + 1)

        // ─ T7 : P3×3 → P7 Compte Bloqué (envoyé par TokenStore) ──────────
        case GotAuthResponse(AccountLocked) =>
          ctx.log.error(s"[Client:$username] COMPTE BLOQUÉ — état absorbant P7 atteint")
          blocked(username)

        case _ => Behaviors.same
      }
    }

  // ─ P4/P5 : In Session ─────────────────────────────────────────────────────
  private def inSession(
    username:   String,
    password:   String,
    token:      JwtToken,
    authServer: ActorRef[AuthCommand]
  ): Behavior[ClientCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {
        // ─ T5 : Déconnecter → P0 (FIX : password conservé) ───────────────
        case Disconnect =>
          ctx.log.info(s"[Client:$username] Déconnexion — retour à Idle")
          idle(username, password, authServer)  // FIX : password conservé

        case _ =>
          ctx.log.warn(s"[Client:$username] Action ignorée en session : $msg")
          Behaviors.same
      }
    }

  // ─ P7 : Compte Bloqué — état absorbant, aucune transition sortante ─────────
  private def blocked(username: String): Behavior[ClientCommand] =
    Behaviors.receive { (ctx, msg) =>
      ctx.log.error(s"[Client:$username] BLOQUÉ — message ignoré : $msg")
      Behaviors.same
    }
}
