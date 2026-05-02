package securebank.analytics

import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.matchers.should.Matchers
import java.io.File
import scala.reflect.io.Directory

class SecurityAnalyzerSpec extends AnyWordSpecLike with Matchers {

  "SecurityAnalyzer" should {

    "analyser correctement des événements générés localement" in {
      // 1. Nettoyer les anciennes données de test pour partir de zéro
      val testDir = new Directory(new File("data/security_events"))
      if (testDir.exists) testDir.deleteRecursively()

      // 2. Générer des fausses données de test via le Writer
      // On simule 3 échecs pour bob (brute-force) et 1 succès pour alice
      ParquetEventWriter.write(SecurityEvent("AUTH_FAILURE", "bob", 1000L))
      ParquetEventWriter.write(SecurityEvent("AUTH_FAILURE", "bob", 2000L))
      ParquetEventWriter.write(SecurityEvent("AUTH_FAILURE", "bob", 3000L))
      ParquetEventWriter.write(SecurityEvent("AUTH_SUCCESS", "alice", 4000L))

      // 3. Analyser les données
      val statsOption = SecurityAnalyzer.analyze()

      // 4. Vérifier les résultats
      statsOption shouldBe defined
      val stats = statsOption.get

      stats.totalEvents shouldBe 4
      stats.authFailures shouldBe 3
      stats.authSuccess shouldBe 1
      
      // Bob doit être identifié comme attaquant brute-force
      stats.bruteForceUsers should contain ("bob")
      stats.bruteForceUsers shouldNot contain ("alice")

      // Fermer le contexte Spark proprement
      SecurityAnalyzer.stop()
      ParquetEventWriter.stop()
    }
  }
}