import akka.actor.typed.{Behavior, PostStop}
import akka.actor.typed.scaladsl.Behaviors
import Protocols._

object TokenStore {
  // État interne du Store
  case class State(
    activeTokens: Map[String, String] = Map.empty, // Token -> User
    failedAttempts: Map[String, Int] = Map.empty,   // User -> Nb échecs
    lockedAccounts: Set[String] = Set.empty
  )

  def apply(): Behavior[TokenCommand] = yieldTokenStore(State())

  private def yieldTokenStore(state: State): Behavior[TokenCommand] = Behaviors.receiveMessage {
    case StoreToken(user, token) =>
      println(s"[TS] Stockage du token pour $user")
      yieldTokenStore(state.copy(activeTokens = state.activeTokens + (token -> user)))

    case ValidateToken(token, replyTo) =>
      if (state.activeTokens.contains(token)) replyTo ! TokenValid
      else replyTo ! TokenInvalid
      Behaviors.same

    case ReportFailure(user) =>
      val attempts = state.failedAttempts.getOrElse(user, 0) + 1
      println(s"[TS] Échec pour $user : $attempts/3")
      if (attempts >= 3) {
        println(s"[TS] !!! COMPTE BLOQUÉ : $user !!!")
        yieldTokenStore(state.copy(
          lockedAccounts = state.lockedAccounts + user,
          failedAttempts = state.failedAttempts + (user -> attempts)
        ))
      } else {
        yieldTokenStore(state.copy(failedAttempts = state.failedAttempts + (user -> attempts)))
      }

    case RevokeToken(token) =>
      println(s"[TS] Révocation du token $token")
      yieldTokenStore(state.copy(activeTokens = state.activeTokens - token))
  }
}