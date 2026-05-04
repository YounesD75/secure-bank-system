package securebank

import akka.actor.typed.{ActorSystem, Behavior, ActorRef}
import akka.actor.typed.scaladsl.{Behaviors, TimerScheduler}
import Protocols._
import securebank.ResourceServer.ResourceCommand
import scala.concurrent.duration._
import securebank.analytics.ParquetEventWriter

object BigDataSimulation {

  val NUM_USERS: Int = 50
  val NUM_GOOD_USERS: Int = 35
  val NUM_ATTACKER_USERS: Int = 10
  val NUM_SESSIONS_PER_USER: Int = 2
  val FAILURE_RATE: Double = 0.2
  val SIMULATION_DURATION: Int = 15

  // val VALID_USERS: Map[String, String] = Map(
  //   "alice" -> "pwd123",
  //   "bob" -> "secret", 
  //   "admin" -> "password123"
  // )

  val USERS: Map[String, String] = {
    val legitUsers = (1 to NUM_GOOD_USERS).map(i => s"user$i" -> s"pass$i")
    val attackerUsers = (1 to NUM_ATTACKER_USERS).map(i => s"attacker$i" -> s"badpass$i")
    val valideusers = Map(
    "alice" -> "pwd123",
    "bob" -> "secret", 
    "admin" -> "password123"
  )
    (legitUsers ++ attackerUsers ++ valideusers).toMap
  }

  val COMMON_PASSWORDS = List("123456", "password", "qwerty", "admin", "letmein")

  sealed trait SimulationCommand
  case object StartSimulation extends SimulationCommand
  case object RunBatch extends SimulationCommand
  case object Shutdown extends SimulationCommand
  case object RunAttacks extends SimulationCommand

  def apply(): Behavior[SimulationCommand] = {
    Behaviors.setup { ctx =>
      val tokenStore = ctx.spawn(TokenStore(ParquetEventWriter.write), "TokenStore")
      val authServer = ctx.spawn(AuthServer(tokenStore, ParquetEventWriter.write), "AuthServer")
      val resourceServer = ctx.spawn(ResourceServer(tokenStore), "ResourceServer")
      
      Behaviors.withTimers { timers =>
        timers.startSingleTimer(StartSimulation, 1.second)
        running(ctx, timers, tokenStore, authServer, resourceServer, 
                Map.empty, 0, 0)
      }
    }
  }

  private def running(
    ctx: akka.actor.typed.scaladsl.ActorContext[SimulationCommand],
    timers: TimerScheduler[SimulationCommand],
    tokenStore: ActorRef[TokenCommand],
    authServer: ActorRef[AuthCommand],
    resourceServer: ActorRef[ResourceCommand],
    activeClients: Map[String, ActorRef[ClientCommand]],
    totalStarted: Int,
    batchCounter: Int
  ): Behavior[SimulationCommand] = Behaviors.receive { (ctx, msg) =>
    msg match {
      case StartSimulation =>
        println(s"""
          ╔══════════════════════════════════════════════════════════════════╗
          ║   🚀 BIG DATA SIMULATION - SecureBank                            ║
          ║   ${USERS.size} utilisateurs, $NUM_SESSIONS_PER_USER sessions chacun     ║
          ╚══════════════════════════════════════════════════════════════════╝
        """)
        
        timers.startTimerAtFixedRate(RunBatch, 1.second)
        timers.startSingleTimer(RunAttacks, (SIMULATION_DURATION - 30).seconds)
        timers.startSingleTimer(Shutdown, SIMULATION_DURATION.seconds)
        Behaviors.same

      case RunBatch =>
        if (totalStarted < USERS.size) {
          val usersToStart = USERS.keys.toList.slice(totalStarted, totalStarted + 5)
          
          usersToStart.foreach { user =>
            val password = USERS(user)
            val client = ctx.spawn(
              Client(user, password, authServer, resourceServer),
              s"client-$user"
            )
            client ! StartNormalLogin
          }
          
          running(ctx, timers, tokenStore, authServer, resourceServer,
                  activeClients, totalStarted + usersToStart.size, batchCounter + 1)
        } else {
          Behaviors.same
        }

      case RunAttacks =>
        println("🔥 Lancement des attaques...")
        
        val stuffingAttacker = ctx.spawn(
          Attacker("mass-stuffer", authServer, resourceServer),
          "mass-stuffer"
        )
        stuffingAttacker ! Attacker.LaunchCredentialStuffing
        
        USERS.keys.toList.take(5).foreach { target =>
          val attacker = ctx.spawn(
            Attacker(target, authServer, resourceServer),
            s"attacker-$target"
          )
          attacker ! Attacker.LaunchBruteForce
        }
        Behaviors.same

      case Shutdown =>
        println("""
          ╔══════════════════════════════════════════════════════════════════╗
          ║                    📊 SIMULATION TERMINÉE                         ║
          ║                                                                  ║
          ║   Les données ont été écrites dans: data/security_events/        ║
          ║                                                                  ║
          ║   🌐 Lancez le dashboard:                                        ║
          ║   sbt 'runMain securebank.SimpleWebServer'                      ║
          ╚══════════════════════════════════════════════════════════════════╝
        """)
        
        securebank.analytics.ParquetEventWriter.stop()
        Behaviors.stopped
    }
  }

  def main(args: Array[String]): Unit = {
    println("""
    ╔══════════════════════════════════════════════════════════════════╗
    ║   🔥 SECUREBANK - BIG DATA SIMULATION                            ║
    ║   Génération de données pour tester Apache Spark...              ║
    ╚══════════════════════════════════════════════════════════════════╝
    """)
    
    val system = ActorSystem(BigDataSimulation(), "BigDataSimulation")
    Thread.sleep((SIMULATION_DURATION + 5) * 1000L)
    system.terminate()
    
    println("\n✅ Simulation terminée !")
  }
}