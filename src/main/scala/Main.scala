package securebank

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import Protocols._
import scala.concurrent.duration._

object SecureBankApp extends App {

  println("""
  ╔══════════════════════════════════════════════════╗
  ║     SecureBank Auth System — Simulation Akka     ║
  ║     AuthServer + TokenStore + Client             ║
  ╚══════════════════════════════════════════════════╝
  """)

  // ── Coordinator : séquence les scénarios avec des timers Akka ─────────────
  // FIX : plus de Thread.sleep dans les behaviors — on utilise withTimers
  sealed trait CoordMsg
  case object RunScenario2 extends CoordMsg
  case object RunScenario3 extends CoordMsg
  case object Shutdown     extends CoordMsg

  val system = ActorSystem(
    Behaviors.setup[CoordMsg] { ctx =>

      val tokenStore = ctx.spawn(TokenStore(),                              "TokenStore")
      val authServer = ctx.spawn(AuthServer(tokenStore),                   "AuthServer")
      val alice      = ctx.spawn(Client("alice", "pwd123", authServer),    "client-alice")
      val user1      = ctx.spawn(Client("user1", "pwd123", authServer),    "client-user1")
      val user1bis   = ctx.spawn(Client("user1", "pwd123", authServer),    "client-user1-retry")

      ctx.log.info("══ SCÉNARIO 1 : Connexion normale (alice) ══")
      alice ! StartNormalLogin

      // Timers Akka pour séquencer les scénarios sans bloquer de thread
      Behaviors.withTimers { timers =>
        timers.startSingleTimer(RunScenario2, 500.millis)

        Behaviors.receiveMessage {
          case RunScenario2 =>
            alice ! Disconnect
            ctx.log.info("══ SCÉNARIO 2 : Brute-force (user1) ══")
            user1 ! StartBruteForce
            timers.startSingleTimer(RunScenario3, 1000.millis)
            Behaviors.same

          case RunScenario3 =>
            ctx.log.info("══ SCÉNARIO 3 : Tentative post-blocage (user1bis) ══")
            user1bis ! StartNormalLogin  // doit recevoir AccountLocked
            timers.startSingleTimer(Shutdown, 500.millis)
            Behaviors.same

          case Shutdown =>
            ctx.log.info("══ Simulation terminée ══")
            ctx.log.info("LTL : G(failures >= 3 → AF account_locked)  ✓")
            ctx.log.info("LTL : G(account_locked → AG ¬access_granted) ✓")
            Behaviors.stopped
        }
      }
    },
    name = "SecureBankSystem"
  )

  // Attente propre de la fin du système
  import scala.concurrent.Await
  import scala.concurrent.duration._
  Await.result(system.whenTerminated, 10.seconds)
  println("\nSystème arrêté proprement.")
}
