package securebank

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers
import Protocols._
import ResourceServer._
import securebank.analytics.SecurityEvent

class ResourceServerSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike with Matchers {

  // Le faux writer qui bypass Spark !
  val dummyWriter: SecurityEvent => Unit = _ => ()

  "ResourceServer" should {

    "autoriser l'accès au solde pour un token valide" in {
      val tokenStore     = spawn(TokenStore(dummyWriter)) // Injection
      val resourceServer = spawn(ResourceServer(tokenStore))
      val probe          = createTestProbe[ResourceResponse]()

      val token = JwtToken(
        value     = "valid-token-1",
        username  = "alice",
        issuedAt  = System.currentTimeMillis(),
        expiresAt = System.currentTimeMillis() + 60000
      )

      tokenStore ! StoreToken(token)
      resourceServer ! GetBalance(token, probe.ref)

      val response = probe.expectMessageType[BalanceOk]
      response.username shouldBe "alice"
      response.amount shouldBe 4250.75
    }

    "refuser l'accès pour un token révoqué" in {
      val tokenStore     = spawn(TokenStore(dummyWriter)) // Injection
      val resourceServer = spawn(ResourceServer(tokenStore))
      val probe          = createTestProbe[ResourceResponse]()

      val token = JwtToken(
        value     = "revoked-token-1",
        username  = "alice",
        issuedAt  = System.currentTimeMillis(),
        expiresAt = System.currentTimeMillis() + 60000
      )

      tokenStore ! StoreToken(token)
      tokenStore ! RevokeToken(token)
      resourceServer ! GetBalance(token, probe.ref)

      val response = probe.expectMessageType[AccessDenied]
      response.reason should include("Token invalide")
    }

    "refuser l'accès pour un token expiré" in {
      val tokenStore     = spawn(TokenStore(dummyWriter)) // Injection
      val resourceServer = spawn(ResourceServer(tokenStore))
      val probe          = createTestProbe[ResourceResponse]()

      val token = JwtToken(
        value     = "expired-token-1",
        username  = "alice",
        issuedAt  = System.currentTimeMillis() - 60000,
        expiresAt = System.currentTimeMillis() - 1000
      )

      tokenStore ! StoreToken(token)
      resourceServer ! GetBalance(token, probe.ref)

      val response = probe.expectMessageType[AccessDenied]
      response.reason should include("Token invalide")
    }
  }
}