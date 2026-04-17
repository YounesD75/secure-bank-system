import akka.actor.typed.{Behavior, ActorRef}
import akka.actor.typed.scaladsl.Behaviors
import Protocols._
import java.util.UUID

object AuthServer {
  def apply(tokenStore: ActorRef[TokenCommand]): Behavior[AuthCommand] = Behaviors.receive { (context, message) =>
    message match {
      case Authenticate(user, pass, replyTo) =>
        // Logique de vérification simplifiée
        if (user == "admin" && pass == "password123") {
          val token = UUID.randomUUID().toString // Simulation d'un JWT
          tokenStore ! StoreToken(user, token)
          replyTo ! AuthSuccess(token)
          println(s"[AS] Auth réussie pour $user")
        } else {
          tokenStore ! ReportFailure(user)
          replyTo ! AuthFailure("Identifiants invalides")
          println(s"[AS] Échec d'auth pour $user")
        }
        Behaviors.same
    }
  }
}