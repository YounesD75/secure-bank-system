package securebank

import akka.actor.typed.{Behavior, ActorRef}
import akka.actor.typed.scaladsl.Behaviors
import Protocols._
import ResourceServer._

object Attacker {

  // ── Protocole public ───────────────────────────────────────────────────────
  sealed trait AttackCommand
  case object LaunchBruteForce                               extends AttackCommand
  final case class LaunchReplayAttack(stolenToken: JwtToken) extends AttackCommand
  case object LaunchCredentialStuffing                       extends AttackCommand

  // Messages internes privés — évite les conflits de noms avec Protocols
  private sealed trait InternalMsg                             extends AttackCommand
  private final case class GotAuthResp(resp: AuthResponse)    extends InternalMsg
  private final case class GotResource(resp: ResourceResponse) extends InternalMsg

  // ── Point d'entrée ────────────────────────────────────────────────────────
  def apply(
    target:         String,
    authServer:     ActorRef[AuthCommand],
    resourceServer: ActorRef[ResourceCommand]
  ): Behavior[AttackCommand] =
    idle(target, authServer, resourceServer)

  // ── État 0 : idle ─────────────────────────────────────────────────────────
  private def idle(
    target:         String,
    authServer:     ActorRef[AuthCommand],
    resourceServer: ActorRef[ResourceCommand]
  ): Behavior[AttackCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        case LaunchBruteForce =>
          ctx.log.warn(s"[Attacker:$target] Lancement brute-force...")
          val adapter = ctx.messageAdapter[AuthResponse](GotAuthResp)
          authServer ! Authenticate(target, "brutepass_1", adapter)
          bruteForcing(target, authServer, resourceServer, attempt = 1, adapter)

        case LaunchReplayAttack(stolenToken) =>
          ctx.log.warn(s"[Attacker:$target] Replay attack — token révoqué : $stolenToken")
          val adapter = ctx.messageAdapter[ResourceResponse](GotResource)
          resourceServer ! GetBalance(stolenToken, adapter)
          waitingResource(target, authServer, resourceServer)

        // Premier Authenticate envoyé immédiatement via ctx — pas de Behaviors.receive intermédiaire
        case LaunchCredentialStuffing =>
          val credentials = List(
            ("alice", "123456"),
            ("bob",   "password"),
            ("admin", "admin"),
            ("alice", "qwerty"),
            ("bob",   "letmein"),
            ("admin", "password123")
          )
          ctx.log.warn(s"[Attacker:$target] Credential stuffing — ${credentials.size} couples")
          val adapter  = ctx.messageAdapter[AuthResponse](GotAuthResp)
          val (u, p)   = credentials.head
          ctx.log.warn(s"[Attacker:$target] Stuffing — essai $u:$p")
          authServer ! Authenticate(u, p, adapter)
          waitStuffingResp(target, authServer, resourceServer, credentials.tail, adapter)

        case _ => Behaviors.same
      }
    }

  // ── État 1 : bruteForcing ─────────────────────────────────────────────────
  private def bruteForcing(
    target:         String,
    authServer:     ActorRef[AuthCommand],
    resourceServer: ActorRef[ResourceCommand],
    attempt:        Int,
    adapter:        ActorRef[AuthResponse]
  ): Behavior[AttackCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        case GotAuthResp(AuthFailure(_, _)) =>
          val next = attempt + 1
          ctx.log.warn(s"[Attacker:$target] Brute-force échec — tentative $next (brutepass_$next)")
          authServer ! Authenticate(target, s"brutepass_$next", adapter)
          bruteForcing(target, authServer, resourceServer, next, adapter)

        case GotAuthResp(AccountLocked) =>
          ctx.log.warn(s"[Attacker:$target] LTL vérifiée : G(failures >= 3 → AF account_locked)")
          Behaviors.stopped

        case _ => Behaviors.same
      }
    }

  // ── État 2 : waitingResource ──────────────────────────────────────────────
  private def waitingResource(
    target:         String,
    authServer:     ActorRef[AuthCommand],
    resourceServer: ActorRef[ResourceCommand]
  ): Behavior[AttackCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        case GotResource(AccessDenied(_)) =>
          ctx.log.warn(s"[Attacker:$target] LTL vérifiée : G(token_revoked → AG ¬access_granted)")
          idle(target, authServer, resourceServer)

        case GotResource(_) =>
          ctx.log.error(s"[Attacker:$target] ALERTE sécurité : accès accordé avec token révoqué !")
          idle(target, authServer, resourceServer)

        case _ => Behaviors.same
      }
    }

  // ── État 3 : waitStuffingResp ─────────────────────────────────────────────
  private def waitStuffingResp(
    target:         String,
    authServer:     ActorRef[AuthCommand],
    resourceServer: ActorRef[ResourceCommand],
    remaining:      List[(String, String)],
    adapter:        ActorRef[AuthResponse]
  ): Behavior[AttackCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {

        case GotAuthResp(AuthSuccess(token)) =>
          ctx.log.warn(s"[Attacker:$target] Credential stuffing SUCCÈS — token: $token")
          nextStuffing(target, authServer, resourceServer, remaining, adapter, ctx)

        case GotAuthResp(AccountLocked) =>
          ctx.log.warn(s"[Attacker:$target] Compte verrouillé — on continue le stuffing")
          nextStuffing(target, authServer, resourceServer, remaining, adapter, ctx)

        case GotAuthResp(AuthFailure(_, _)) =>
          nextStuffing(target, authServer, resourceServer, remaining, adapter, ctx)

        case _ => Behaviors.same
      }
    }

  private def nextStuffing(
    target:         String,
    authServer:     ActorRef[AuthCommand],
    resourceServer: ActorRef[ResourceCommand],
    remaining:      List[(String, String)],
    adapter:        ActorRef[AuthResponse],
    ctx:            akka.actor.typed.scaladsl.ActorContext[AttackCommand]
  ): Behavior[AttackCommand] =
    if (remaining.isEmpty) {
      ctx.log.warn(s"[Attacker:$target] Credential stuffing terminé")
      idle(target, authServer, resourceServer)
    } else {
      val (u, p) = remaining.head
      ctx.log.warn(s"[Attacker:$target] Stuffing — essai $u:$p")
      authServer ! Authenticate(u, p, adapter)
      waitStuffingResp(target, authServer, resourceServer, remaining.tail, adapter)
    }
}
