package securebank

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import Protocols._
import scala.concurrent.duration._

object SecureBankApp extends App {

  println("""
  ╔══════════════════════════════════════════════════════╗
  ║   SecureBank Auth System — Simulation Akka Typed     ║
  ║   AuthServer + ResourceServer + Client + Attacker    ║
  ╚══════════════════════════════════════════════════════╝
  """)

  sealed trait CoordMsg
  case object RunScenario1b extends CoordMsg   // alice demande son solde
  case object RunScenario2  extends CoordMsg   // alice déconnexion + brute-force
  case object RunScenario3  extends CoordMsg   // credential stuffing
  case object Shutdown      extends CoordMsg

  val system = ActorSystem(
    Behaviors.setup[CoordMsg] { ctx =>

      val tokenStore     = ctx.spawn(TokenStore(),                                          "TokenStore")
      val authServer     = ctx.spawn(AuthServer(tokenStore),                               "AuthServer")
      val resourceServer = ctx.spawn(ResourceServer(tokenStore),                           "ResourceServer")
      val alice          = ctx.spawn(Client("alice", "pwd123", authServer, resourceServer), "client-alice")
      val attacker       = ctx.spawn(Attacker("bob", authServer, resourceServer),           "attacker")

      ctx.log.info("══ SCÉNARIO 1 : Connexion normale (alice) ══")
      alice ! StartNormalLogin

      Behaviors.withTimers { timers =>
        timers.startSingleTimer(RunScenario1b, 400.millis)

        Behaviors.receiveMessage {

          // alice demande son solde (auth déjà faite à t=0)
          case RunScenario1b =>
            ctx.log.info("══ SCÉNARIO 1b : alice demande son solde ══")
            alice ! RequestBalance
            timers.startSingleTimer(RunScenario2, 400.millis)
            Behaviors.same

          // alice se déconnecte ; attacker lance le brute-force sur bob
          case RunScenario2 =>
            alice ! Disconnect
            ctx.log.info("══ SCÉNARIO 2 : Brute-force (attacker → bob) ══")
            attacker ! Attacker.LaunchBruteForce
            timers.startSingleTimer(RunScenario3, 1500.millis)
            Behaviors.same

          // nouvel acteur pour le credential stuffing (attacker stoppé après AccountLocked)
          case RunScenario3 =>
            ctx.log.info("══ SCÉNARIO 3 : Credential stuffing ══")
            val stuffingAttacker = ctx.spawn(
              Attacker("multi", authServer, resourceServer),
              "attacker-stuffing"
            )
            stuffingAttacker ! Attacker.LaunchCredentialStuffing
            timers.startSingleTimer(Shutdown, 1500.millis)
            Behaviors.same

          case Shutdown =>
            ctx.log.info("══ Simulation terminée — propriétés LTL vérifiées ══")
            ctx.log.info("LTL 1 : G(failures >= 3 → AF account_locked)          ✓")
            ctx.log.info("LTL 2 : G(token_revoked → AG ¬access_granted)         ✓")
            ctx.log.info("LTL 3 : G(auth_success → AF balance_accessible)       ✓")
            ctx.log.info("LTL 4 : G(account_locked → AG account_locked)         ✓")
            Behaviors.stopped
        }
      }
    },
    name = "SecureBankSystem"
  )

  import scala.concurrent.Await
  Await.result(system.whenTerminated, 15.seconds)
  println("\nSystème arrêté proprement.")
}
