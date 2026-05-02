package securebank

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers
import Protocols._

class ClientSpec extends ScalaTestWithActorTestKit with AnyWordSpecLike with Matchers {

  "L'acteur Client" should {

    "demander une authentification au démarrage" in {
      val mockAuthServer     = createTestProbe[AuthCommand]()
      val mockResourceServer = createTestProbe[ResourceServer.ResourceCommand]()
      
      val client = spawn(Client("alice", "pwd123", mockAuthServer.ref, mockResourceServer.ref))

      client ! StartNormalLogin

      // Vérifie que le client a bien envoyé un message Authenticate au serveur
      val authRequest = mockAuthServer.expectMessageType[Authenticate]
      authRequest.user shouldBe "alice"
      authRequest.pass shouldBe "pwd123"
    }

    "passer en état bloqué s'il reçoit AccountLocked" in {
      val mockAuthServer     = createTestProbe[AuthCommand]()
      val mockResourceServer = createTestProbe[ResourceServer.ResourceCommand]()
      
      val client = spawn(Client("bob", "secret", mockAuthServer.ref, mockResourceServer.ref))

      // On force le client à croire qu'il est bloqué
      client ! GotAuthResponse(AccountLocked)

      // S'il est bloqué, il doit ignorer cette requête et NE PAS contacter le resource server
      client ! RequestBalance
      mockResourceServer.expectNoMessage()
    }
  }
}