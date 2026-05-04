package securebank

import akka.actor.typed.{Behavior, ActorRef}
import akka.actor.typed.scaladsl.Behaviors
import Protocols._

object ResourceServer {

  // ── Protocole ─────────────────────────────────────────────────────────────
  sealed trait ResourceCommand
  final case class GetBalance(
    token:   JwtToken,
    replyTo: ActorRef[ResourceResponse]
  ) extends ResourceCommand

  sealed trait ResourceResponse
  final case class BalanceOk(username: String, amount: Double) extends ResourceResponse
  final case class AccessDenied(reason: String)                extends ResourceResponse

  private val balances: Map[String, Double] = Map(
    "alice" -> 4250.75,
    "bob"   -> 1830.00,
    "admin" -> 99999.99
  )

  // Msg interne : résultat de la vérification du token par le TokenStore
  private sealed trait InternalMsg extends ResourceCommand
  private final case class TokenCheckResult(
    username: String,
    valid:    Boolean,
    replyTo:  ActorRef[ResourceResponse]
  ) extends InternalMsg

  // ── Behavior ───────────────────────────────────────────────────────────────
  def apply(tokenStore: ActorRef[TokenCommand]): Behavior[ResourceCommand] =
    Behaviors.receive { (ctx, msg) =>
      msg match {
        case GetBalance(token, replyTo) =>
          val adapter = ctx.messageAdapter[ValidationResponse] {
            case TokenValid   => TokenCheckResult(token.username, valid = true,  replyTo)
            case TokenInvalid => TokenCheckResult(token.username, valid = false, replyTo)
          }
          tokenStore ! ValidateToken(token, adapter)
          Behaviors.same

        case TokenCheckResult(username, true, replyTo) =>
          val amount = balances.getOrElse(username, 0.0)
          ctx.log.info(s"[ResourceServer] Token valide — solde $username : $amount €")
          replyTo ! BalanceOk(username, amount)
          Behaviors.same

        case TokenCheckResult(username, false, replyTo) =>
          ctx.log.warn(s"[ResourceServer] Token invalide/révoqué — accès refusé ($username)")
          replyTo ! AccessDenied(s"Token invalide ou révoqué pour $username")
          Behaviors.same
      }
    }
}
