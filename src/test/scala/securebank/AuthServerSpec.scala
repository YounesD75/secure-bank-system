package securebank

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers
import Protocols._

class AuthServerSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike with Matchers {

  "AuthServer" should {

    "émettre un JwtToken valide pour credentials corrects" in {
      val tokenStore = spawn(TokenStore())
      val authServer = spawn(AuthServer(tokenStore))
      val probe      = createTestProbe[AuthResponse]()

      authServer ! Authenticate("alice", "pwd123", probe.ref)

      val token = probe.expectMessageType[AuthSuccess].token
      token.username shouldBe "alice"
      token.isExpired shouldBe false
    }

    "refuser des credentials invalides et compter les échecs" in {
      val tokenStore = spawn(TokenStore())
      val authServer = spawn(AuthServer(tokenStore))
      val probe      = createTestProbe[AuthResponse]()

      authServer ! Authenticate("alice", "MAUVAIS", probe.ref)

      probe.expectMessageType[AuthFailure].attempts shouldBe 1
    }

    "bloquer un compte après 3 échecs consécutifs" in {
      val tokenStore = spawn(TokenStore())
      val authServer = spawn(AuthServer(tokenStore))
      val probe      = createTestProbe[AuthResponse]()

      // échec 1 → attempts == 1
      authServer ! Authenticate("alice", "MAUVAIS", probe.ref)
      probe.expectMessageType[AuthFailure]

      // échec 2 → attempts == 2
      authServer ! Authenticate("alice", "MAUVAIS", probe.ref)
      probe.expectMessageType[AuthFailure]

      // échec 3 → attempts >= 3 : TokenStore envoie AccountLocked
      authServer ! Authenticate("alice", "MAUVAIS", probe.ref)
      probe.expectMessage(AccountLocked)

      // 4e tentative avec bon password : IsAccountLocked bloque avant vérification
      authServer ! Authenticate("alice", "pwd123", probe.ref)
      probe.expectMessage(AccountLocked)
    }

    "valider un token actif via CheckToken" in {
      val tokenStore = spawn(TokenStore())
      val authServer = spawn(AuthServer(tokenStore))
      val authProbe  = createTestProbe[AuthResponse]()
      val validProbe = createTestProbe[ValidationResponse]()

      authServer ! Authenticate("alice", "pwd123", authProbe.ref)
      val token = authProbe.expectMessageType[AuthSuccess].token

      authServer ! CheckToken(token, validProbe.ref)
      validProbe.expectMessage(TokenValid)
    }

    "refuser un token après révocation" in {
      val tokenStore = spawn(TokenStore())
      val authServer = spawn(AuthServer(tokenStore))
      val authProbe  = createTestProbe[AuthResponse]()
      val validProbe = createTestProbe[ValidationResponse]()

      authServer ! Authenticate("alice", "pwd123", authProbe.ref)
      val token = authProbe.expectMessageType[AuthSuccess].token

      authServer ! RevokeTokenAuth(token, authProbe.ref)
      authProbe.expectMessage(TokenRevoked)

      authServer ! CheckToken(token, validProbe.ref)
      validProbe.expectMessage(TokenInvalid)
    }
  }
}
