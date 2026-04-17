import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import Protocols._

object SecureBankApp {
  def main(args: Array[String]): Unit = {
    val rootBehavior = Behaviors.setup[Unit] { context =>
      // 1. Démarrage du TokenStore (le stockage)
      val tokenStore = context.spawn(TokenStore(), "TokenStore")
      
      // 2. Démarrage de l'AuthServer (en lui donnant la référence du TokenStore)
      val authServer = context.spawn(AuthServer(tokenStore), "AuthServer")

      // 3. Simulation d'un client (on crée un acteur temporaire pour recevoir la réponse)
      val testResponseMapper = context.spawn(Behaviors.receiveMessage[AuthResponse] {
        case AuthSuccess(token) =>
          context.log.info(s"TEST: Connexion réussie ! Token reçu: $token")
          Behaviors.same
        case AuthFailure(reason) =>
          context.log.warn(s"TEST: Connexion échouée: $reason")
          Behaviors.same
        case AccountLocked =>
          context.log.error("TEST: Compte verrouillé !")
          Behaviors.same
      }, "TestResponseHandler")

      // SCÉNARIO 1 : Tentative réussie
      authServer ! Authenticate("admin", "password123", testResponseMapper)

      // SCÉNARIO 2 : Simulation du verrouillage (3 échecs)
      authServer ! Authenticate("user1", "wrong_pass", testResponseMapper)
      authServer ! Authenticate("user1", "wrong_pass", testResponseMapper)
      authServer ! Authenticate("user1", "wrong_pass", testResponseMapper)

      Behaviors.empty
    }

    // Lancement du système
    val system = ActorSystem(rootBehavior, "SecureBankSystem")
  }
}