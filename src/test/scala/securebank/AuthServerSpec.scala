package securebank

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers
import Protocols._
import securebank.analytics.SecurityEvent

class AuthServerSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike with Matchers {

  // Le faux writer qui bypass Spark !
  val dummyWriter: SecurityEvent => Unit = _ => ()

  "AuthServer" should {

    "émettre un JwtToken valide pour credentials corrects" in {
      val tokenStore = spawn(TokenStore(dummyWriter))
      val authServer = spawn(AuthServer(tokenStore, dummyWriter)) // Injection
      val probe      = createTestProbe[AuthResponse]()

      authServer ! Authenticate("alice", "pwd123", probe.ref)

      val token = probe.expectMessageType[AuthSuccess].token
      token.username shouldBe "alice"
      token.isExpired shouldBe false
    }

    "refuser des credentials invalides et compter les échecs" in {
      val tokenStore = spawn(TokenStore(dummyWriter))
      val authServer = spawn(AuthServer(tokenStore, dummyWriter))
      val probe      = createTestProbe[AuthResponse]()

      authServer ! Authenticate("alice", "MAUVAIS", probe.ref)

      probe.expectMessageType[AuthFailure].attempts shouldBe 1
    }

    "bloquer un compte après 3 échecs consécutifs" in {
      val tokenStore = spawn(TokenStore(dummyWriter))
      val authServer = spawn(AuthServer(tokenStore, dummyWriter))
      val probe      = createTestProbe[AuthResponse]()

      authServer ! Authenticate("alice", "MAUVAIS", probe.ref)
      probe.expectMessageType[AuthFailure]

      authServer ! Authenticate("alice", "MAUVAIS", probe.ref)
      probe.expectMessageType[AuthFailure]

      authServer ! Authenticate("alice", "MAUVAIS", probe.ref)
      probe.expectMessage(AccountLocked)

      authServer ! Authenticate("alice", "pwd123", probe.ref)
      probe.expectMessage(AccountLocked)
    }

    "valider un token actif via CheckToken" in {
      val tokenStore = spawn(TokenStore(dummyWriter))
      val authServer = spawn(AuthServer(tokenStore, dummyWriter))
      val authProbe  = createTestProbe[AuthResponse]()
      val validProbe = createTestProbe[ValidationResponse]()

      authServer ! Authenticate("alice", "pwd123", authProbe.ref)
      val token = authProbe.expectMessageType[AuthSuccess].token

      authServer ! CheckToken(token, validProbe.ref)
      validProbe.expectMessage(TokenValid)
    }

    "refuser un token après révocation" in {
      val tokenStore = spawn(TokenStore(dummyWriter))
      val authServer = spawn(AuthServer(tokenStore, dummyWriter))
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