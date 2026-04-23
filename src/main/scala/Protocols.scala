import akka.actor.typed.ActorRef

// ─────────────────────────────────────────────────────────────────────────────
// Protocols.scala — messages partagés entre tous les acteurs SecureBank
// Basé sur la version de Rayan, enrichi avec le Client et le check de lock
// ─────────────────────────────────────────────────────────────────────────────

object Protocols {

  // ── Messages pour l'AuthServer ─────────────────────────────────────────────
  sealed trait AuthCommand

  case class Authenticate(
    user:    String,
    pass:    String,
    replyTo: ActorRef[AuthResponse]
  ) extends AuthCommand

  case class ValidateToken(
    token:   String,
    replyTo: ActorRef[ValidationResponse]
  ) extends AuthCommand

  case class RevokeTokenAuth(
    token:   String,
    replyTo: ActorRef[AuthResponse]
  ) extends AuthCommand

  // ── Réponses de l'AuthServer ───────────────────────────────────────────────
  sealed trait AuthResponse

  case class AuthSuccess(token: String)       extends AuthResponse
  case class AuthFailure(reason: String, attempts: Int) extends AuthResponse
  case object AccountLocked                   extends AuthResponse
  case object TokenRevoked                    extends AuthResponse

  // ── Messages pour le TokenStore ───────────────────────────────────────────
  sealed trait TokenCommand

  case class StoreToken(user: String, token: String) extends TokenCommand
  case class CheckAndAuthenticate(
    user:    String,
    pass:    String,
    replyTo: ActorRef[AuthResponse]
  ) extends TokenCommand
  case class ReportFailure(user: String, replyTo: ActorRef[AuthResponse]) extends TokenCommand
  case class RevokeToken(token: String)                  extends TokenCommand
  case class IsAccountLocked(user: String, replyTo: ActorRef[Boolean]) extends TokenCommand

  // ── Réponses de validation ─────────────────────────────────────────────────
  sealed trait ValidationResponse

  case object TokenValid   extends ValidationResponse
  case object TokenInvalid extends ValidationResponse

  // ── Messages pour le Client ────────────────────────────────────────────────
  sealed trait ClientCommand

  case object StartNormalLogin  extends ClientCommand
  case object StartBruteForce   extends ClientCommand
  case object Disconnect        extends ClientCommand

  // adaptateurs internes reçus par le Client
  case class GotAuthResponse(resp: AuthResponse) extends ClientCommand
}
