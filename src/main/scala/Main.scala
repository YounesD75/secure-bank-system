package securebank

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import Protocols._
import scala.concurrent.duration._
import securebank.analytics.ParquetEventWriter

object SecureBankApp extends App {

  sealed trait CoordMsg
  case object RunScenario1b extends CoordMsg   
  case object RunScenario2  extends CoordMsg   
  case object RunScenario3  extends CoordMsg   
  case object Shutdown      extends CoordMsg

  val system = ActorSystem(
    Behaviors.setup[CoordMsg] { ctx =>

      val tokenStore     = ctx.spawn(TokenStore(ParquetEventWriter.write), "TokenStore")
      val authServer     = ctx.spawn(AuthServer(tokenStore, ParquetEventWriter.write), "AuthServer")
      val resourceServer = ctx.spawn(ResourceServer(tokenStore), "ResourceServer")
      val alice          = ctx.spawn(Client("alice", "pwd123", authServer, resourceServer), "client-alice")
      val attacker       = ctx.spawn(Attacker("bob", authServer, resourceServer), "attacker")

      alice ! StartNormalLogin

      Behaviors.withTimers { timers =>
        timers.startSingleTimer(RunScenario1b, 400.millis)

        Behaviors.receiveMessage {

          case RunScenario1b =>
            alice ! RequestBalance
            timers.startSingleTimer(RunScenario2, 400.millis)
            Behaviors.same

          case RunScenario2 =>
            alice ! Disconnect
            attacker ! Attacker.LaunchBruteForce
            timers.startSingleTimer(RunScenario3, 1500.millis)
            Behaviors.same

          case RunScenario3 =>
            val stuffingAttacker = ctx.spawn(
              Attacker("multi", authServer, resourceServer),
              "attacker-stuffing"
            )
            stuffingAttacker ! Attacker.LaunchCredentialStuffing
            timers.startSingleTimer(Shutdown, 1500.millis)
            Behaviors.same

          case Shutdown =>
            Behaviors.stopped
        }
      }
    },
    name = "SecureBankSystem"
  )

  import scala.concurrent.Await
  Await.result(system.whenTerminated, 15.seconds)
}