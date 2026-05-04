package securebank

import akka.actor.typed.{Behavior, ActorRef}
import akka.actor.typed.scaladsl.Behaviors
import Protocols._
import ResourceServer._

object Client {

  def apply(
    username:       String,
    password:       String,
    authServer:     ActorRef[AuthCommand],
    resourceServer: ActorRef[ResourceCommand]
  ): Behavior[ClientCommand] =
    idle(username, password, authServer, resourceServer)

  // ─ P0 : Idle ──────────────────────────────────────────────────────────────
  private def idle(
    username:       String,
    password:       String,
    authServer:     ActorRef[AuthCommand],
    resourceServer: ActorRef[ResourceCommand]
  ): Behavior[ClientCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {
        case StartNormalLogin =>
          ctx.log.info(s"[Client:$username] Connexion normale...")
          val adapter = ctx.messageAdapter[AuthResponse](GotAuthResponse)
          authServer ! Authenticate(username, password, adapter)
          awaitingAuth(username, password, authServer, resourceServer)

        case StartBruteForce =>
          ctx.log.warn(s"[Client:$username] Brute-force — tentative 1/3")
          val adapter = ctx.messageAdapter[AuthResponse](GotAuthResponse)
          authServer ! Authenticate(username, "WRONG_1", adapter)
          awaitingBrute(username, password, authServer, resourceServer, attempt = 1)

        case _ => Behaviors.same
      }
    }

  // ─ P1 : AwaitingAuth (connexion normale) ──────────────────────────────────
  private def awaitingAuth(
    username:       String,
    password:       String,
    authServer:     ActorRef[AuthCommand],
    resourceServer: ActorRef[ResourceCommand]
  ): Behavior[ClientCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {
        case GotAuthResponse(AuthSuccess(token)) =>
          ctx.log.info(s"[Client:$username] Authentifié — $token")
          ctx.log.info(s"[Client:$username] Session ouverte — accès au compte disponible")
          inSession(username, password, token, authServer, resourceServer)

        case GotAuthResponse(AuthFailure(reason, attempts)) =>
          ctx.log.warn(s"[Client:$username] Échec ($reason) — $attempts/3")
          idle(username, password, authServer, resourceServer)

        case GotAuthResponse(AccountLocked) =>
          ctx.log.error(s"[Client:$username] Compte bloqué — état P7")
          blocked(username)

        case _ => Behaviors.same
      }
    }

  // ─ P1 : AwaitingAuth (brute-force) ────────────────────────────────────────
  private def awaitingBrute(
    username:       String,
    password:       String,
    authServer:     ActorRef[AuthCommand],
    resourceServer: ActorRef[ResourceCommand],
    attempt:        Int
  ): Behavior[ClientCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {
        case GotAuthResponse(AuthFailure(_, _)) if attempt < 3 =>
          ctx.log.warn(s"[Client:$username] Brute-force — tentative ${attempt + 1}/3")
          val adapter = ctx.messageAdapter[AuthResponse](GotAuthResponse)
          authServer ! Authenticate(username, s"WRONG_${attempt + 1}", adapter)
          awaitingBrute(username, password, authServer, resourceServer, attempt + 1)

        case GotAuthResponse(AccountLocked) =>
          ctx.log.error(s"[Client:$username] COMPTE BLOQUÉ — état absorbant P7 atteint")
          blocked(username)

        case _ => Behaviors.same
      }
    }

  // ─ P4/P5 : In Session ─────────────────────────────────────────────────────
  private def inSession(
    username:       String,
    password:       String,
    token:          JwtToken,
    authServer:     ActorRef[AuthCommand],
    resourceServer: ActorRef[ResourceCommand]
  ): Behavior[ClientCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {
        case RequestBalance =>
          ctx.log.info(s"[Client:$username] Demande de solde...")
          val adapter = ctx.messageAdapter[ResourceResponse] {
            case BalanceOk(_, amount)  => GotBalance(amount)
            case AccessDenied(reason)  => GotAccessDenied(reason)
          }
          resourceServer ! GetBalance(token, adapter)
          Behaviors.same

        case GotBalance(amount) =>
          ctx.log.info(f"[Client:$username] Solde : $amount%.2f €")
          Behaviors.same

        case GotAccessDenied(reason) =>
          ctx.log.warn(s"[Client:$username] Accès refusé : $reason")
          Behaviors.same

        case Disconnect =>
          ctx.log.info(s"[Client:$username] Déconnexion — retour à Idle")
          idle(username, password, authServer, resourceServer)

        case _ =>
          ctx.log.warn(s"[Client:$username] Action ignorée en session : $msg")
          Behaviors.same
      }
    }

  // ─ P7 : Compte Bloqué — état absorbant ────────────────────────────────────
  private def blocked(username: String): Behavior[ClientCommand] =
    Behaviors.receive { (ctx, msg) =>
      ctx.log.error(s"[Client:$username] BLOQUÉ — message ignoré : $msg")
      Behaviors.same
    }
}
