package securebank

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers
import Protocols._
import ResourceServer._
import securebank.analytics.SecurityEvent

class SecureBankIntegrationSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike with Matchers {

  val dummyWriter: SecurityEvent => Unit = _ => ()

  "Le système SecureBank dans son ensemble" should {

    "permettre un flux complet : Auth -> Obtention du Token -> Lecture du Solde" in {
      val tokenStore     = spawn(TokenStore(dummyWriter))
      val authServer     = spawn(AuthServer(tokenStore, dummyWriter))
      val resourceServer = spawn(ResourceServer(tokenStore))
      
      val authProbe      = createTestProbe[AuthResponse]()
      val resourceProbe  = createTestProbe[ResourceResponse]()

      // 1. Authentification
      authServer ! Authenticate("alice", "pwd123", authProbe.ref)
      val token = authProbe.expectMessageType[AuthSuccess].token
      token.username shouldBe "alice"

      // 2. Utilisation de la ressource
      resourceServer ! GetBalance(token, resourceProbe.ref)
      val balance = resourceProbe.expectMessageType[BalanceOk]
      
      balance.username shouldBe "alice"
      balance.amount shouldBe 4250.75
    }

    "bloquer toute la chaîne après une attaque par force brute (LTL 1 & 4)" in {
      val tokenStore     = spawn(TokenStore(dummyWriter))
      val authServer     = spawn(AuthServer(tokenStore, dummyWriter))
      val authProbe      = createTestProbe[AuthResponse]()

      // 3 échecs consécutifs
      authServer ! Authenticate("bob", "mauvais1", authProbe.ref)
      authProbe.expectMessageType[AuthFailure]
      authServer ! Authenticate("bob", "mauvais2", authProbe.ref)
      authProbe.expectMessageType[AuthFailure]
      
      // Le 3ème échec verrouille le compte
      authServer ! Authenticate("bob", "mauvais3", authProbe.ref)
      authProbe.expectMessage(AccountLocked)

      // Même avec le bon mot de passe, l'accès reste bloqué (état absorbant)
      authServer ! Authenticate("bob", "secret", authProbe.ref)
      authProbe.expectMessage(AccountLocked)
    }
  }
}