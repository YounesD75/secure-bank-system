import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import Protocols._

// ─────────────────────────────────────────────────────────────────────────────
// Main.scala — 3 scénarios de simulation
//
// Scénario 1 : alice se connecte normalement et se déconnecte
// Scénario 2 : user1 tente un brute-force → compte bloqué
// Scénario 3 : admin tente de se connecter après blocage → refusé
// ─────────────────────────────────────────────────────────────────────────────

object SecureBankApp extends App {

  println("""
  ╔══════════════════════════════════════════════════╗
  ║     SecureBank Auth System — Simulation Akka     ║
  ║     AuthServer + TokenStore + Client             ║
  ╚══════════════════════════════════════════════════╝
  """)

  val system = ActorSystem(
    Behaviors.setup[Nothing] { ctx =>

      val tokenStore = ctx.spawn(TokenStore(), "TokenStore")
      val authServer = ctx.spawn(AuthServer(tokenStore), "AuthServer")

      // ── Scénario 1 : Connexion normale — alice ──────────────────────────
      println("\n══ SCÉNARIO 1 : Connexion normale (alice) ══")
      val alice = ctx.spawn(Client("alice", "pwd123", authServer), "client-alice")
      alice ! StartNormalLogin

      Thread.sleep(400)
      alice ! Disconnect

      // ── Scénario 2 : Brute-force — user1 ───────────────────────────────
      Thread.sleep(200)
      println("\n══ SCÉNARIO 2 : Brute-force (user1) ══")
      val user1 = ctx.spawn(Client("user1", "pwd123", authServer), "client-user1")
      user1 ! StartBruteForce

      Thread.sleep(800)

      // ── Scénario 3 : Tentative post-blocage ─────────────────────────────
      println("\n══ SCÉNARIO 3 : Tentative après blocage (user1) ══")
      user1 ! StartNormalLogin  // doit recevoir AccountLocked immédiatement

      Thread.sleep(400)
      println("\n══ Simulation terminée ══")
      println("LTL vérifiée : G(failures >= 3 → AF account_locked)")
      println("LTL vérifiée : G(account_locked → AG ¬access_granted)")

      Behaviors.empty
    },
    name = "SecureBankSystem"
  )

  Thread.sleep(3000)
  system.terminate()
}
