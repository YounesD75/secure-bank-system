package securebank

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers
import Protocols._
import securebank.analytics.SecurityEvent

class TokenStoreSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike with Matchers {

  // Le faux writer qui bypass Spark !
  val dummyWriter: SecurityEvent => Unit = _ => ()

  "TokenStore" should {

    "valider un token actif" in {
      val tokenStore = spawn(TokenStore(dummyWriter)) // Injection
      val probe      = createTestProbe[ValidationResponse]()

      val token = JwtToken(
        value     = "active-token-1",
        username  = "alice",
        issuedAt  = System.currentTimeMillis(),
        expiresAt = System.currentTimeMillis() + 60000
      )

      tokenStore ! StoreToken(token)
      tokenStore ! ValidateToken(token, probe.ref)

      probe.expectMessage(TokenValid)
    }

    "refuser un token révoqué" in {
      val tokenStore = spawn(TokenStore(dummyWriter)) // Injection
      val probe      = createTestProbe[ValidationResponse]()

      val token = JwtToken(
        value     = "revoked-token-2",
        username  = "alice",
        issuedAt  = System.currentTimeMillis(),
        expiresAt = System.currentTimeMillis() + 60000
      )

      tokenStore ! StoreToken(token)
      tokenStore ! RevokeToken(token)
      tokenStore ! ValidateToken(token, probe.ref)

      probe.expectMessage(TokenInvalid)
    }

    "refuser un token expiré" in {
      val tokenStore = spawn(TokenStore(dummyWriter)) // Injection
      val probe      = createTestProbe[ValidationResponse]()

      val token = JwtToken(
        value     = "expired-token-2",
        username  = "alice",
        issuedAt  = System.currentTimeMillis() - 60000,
        expiresAt = System.currentTimeMillis() - 1000
      )

      tokenStore ! StoreToken(token)
      tokenStore ! ValidateToken(token, probe.ref)

      probe.expectMessage(TokenInvalid)
    }

    "bloquer un compte après 3 échecs" in {
      val tokenStore = spawn(TokenStore(dummyWriter)) // Injection
      val probe      = createTestProbe[AuthResponse]()

      tokenStore ! ReportFailure("bob", probe.ref)
      probe.expectMessageType[AuthFailure].attempts shouldBe 1

      tokenStore ! ReportFailure("bob", probe.ref)
      probe.expectMessageType[AuthFailure].attempts shouldBe 2

      tokenStore ! ReportFailure("bob", probe.ref)
      probe.expectMessage(AccountLocked)

      val lockProbe = createTestProbe[Boolean]()
      tokenStore ! IsAccountLocked("bob", lockProbe.ref)
      lockProbe.expectMessage(true)
    }
  }
}