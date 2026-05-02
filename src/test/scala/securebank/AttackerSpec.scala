package securebank

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers
import Protocols._

class AttackerSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike with Matchers {

  "L'acteur Attacker" should {

    "enchaîner les mots de passe lors d'un Credential Stuffing" in {
      val mockAuthServer     = createTestProbe[AuthCommand]()
      val mockResourceServer = createTestProbe[ResourceServer.ResourceCommand]()
      
      val attacker = spawn(Attacker("target", mockAuthServer.ref, mockResourceServer.ref))

      attacker ! Attacker.LaunchCredentialStuffing

      // Il doit envoyer la première requête de sa liste interne
      val req1 = mockAuthServer.expectMessageType[Authenticate]
      req1.user shouldBe "alice"
      req1.pass shouldBe "123456"

      // On simule un échec du serveur
      req1.replyTo ! AuthFailure("Invalid", 1)

      // Il doit immédiatement enchaîner avec le 2ème mot de passe de sa liste
      val req2 = mockAuthServer.expectMessageType[Authenticate]
      req2.user shouldBe "bob"
      req2.pass shouldBe "password"
    }
  }
}