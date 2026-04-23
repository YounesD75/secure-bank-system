package securebank

import akka.actor.typed.ActorRef

object Protocols {

  // ── Token JWT simulé ───────────────────────────────────────────────────────
  final case class JwtToken(
    value:     String,
    username:  String,
    issuedAt:  Long,
    expiresAt: Long
  ) {
    def isExpired: Boolean = System.currentTimeMillis() > expiresAt
    override def toString: String =
      s"${value.take(8)}... [user=$username, expire=${(expiresAt - System.currentTimeMillis()) / 1000}s]"
  }

  // ── Messages pour l'AuthServer ─────────────────────────────────────────────
  sealed trait AuthCommand

  final case class Authenticate(
    user:    String,
    pass:    String,
    replyTo: ActorRef[AuthResponse]
  ) extends AuthCommand

  final case class CheckToken(
    token:   JwtToken,
    replyTo: ActorRef[ValidationResponse]
  ) extends AuthCommand

  final case class RevokeTokenAuth(
    token:   JwtToken,
    replyTo: ActorRef[AuthResponse]
  ) extends AuthCommand

  // ── Réponses de l'AuthServer ───────────────────────────────────────────────
  sealed trait AuthResponse

  final case class AuthSuccess(token: JwtToken)               extends AuthResponse
  final case class AuthFailure(reason: String, attempts: Int) extends AuthResponse
  case object AccountLocked                                   extends AuthResponse
  case object TokenRevoked                                    extends AuthResponse

  // ── Messages pour le TokenStore ───────────────────────────────────────────
  sealed trait TokenCommand

  final case class StoreToken(token: JwtToken)                extends TokenCommand
  final case class ValidateToken(
    token:   JwtToken,
    replyTo: ActorRef[ValidationResponse]
  )                                                           extends TokenCommand
  final case class ReportFailure(
    user:    String,
    replyTo: ActorRef[AuthResponse]
  )                                                           extends TokenCommand
  final case class RevokeToken(token: JwtToken)               extends TokenCommand
  final case class IsAccountLocked(
    user:    String,
    replyTo: ActorRef[Boolean]
  )                                                           extends TokenCommand

  // ── Réponses de validation ─────────────────────────────────────────────────
  sealed trait ValidationResponse
  case object TokenValid   extends ValidationResponse
  case object TokenInvalid extends ValidationResponse

  // ── Messages pour le Client ────────────────────────────────────────────────
  sealed trait ClientCommand
  case object StartNormalLogin                         extends ClientCommand
  case object StartBruteForce                          extends ClientCommand
  case object Disconnect                               extends ClientCommand
  final case class GotAuthResponse(resp: AuthResponse) extends ClientCommand
}
