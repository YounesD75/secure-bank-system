import akka.actor.typed.ActorRef

object Protocols {
  // --- Messages pour l'AuthServer (AS) ---
  sealed trait AuthCommand
  case class Authenticate(user: String, pass: String, replyTo: ActorRef[AuthResponse]) extends AuthCommand

  sealed trait AuthResponse
  case class AuthSuccess(token: String) extends AuthResponse
  case class AuthFailure(reason: String) extends AuthResponse
  case object AccountLocked extends AuthResponse

  // --- Messages pour le TokenStore (TS) ---
  sealed trait TokenCommand
  case class StoreToken(user: String, token: String) extends TokenCommand
  case class ValidateToken(token: String, replyTo: ActorRef[ValidationResponse]) extends TokenCommand
  case class ReportFailure(user: String) extends TokenCommand // Pour la règle des 3 échecs
  case class RevokeToken(token: String) extends TokenCommand   // Utilisé par Spark plus tard

  sealed trait ValidationResponse
  case object TokenValid extends ValidationResponse
  case object TokenInvalid extends ValidationResponse
}