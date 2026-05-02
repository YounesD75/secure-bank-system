package securebank.analytics

import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions._

object SparkSecurityAnalysis extends App {

  val spark = SparkSession.builder()
    .appName("SecureBank-Security-Analysis")
    .master("local[*]")
    .getOrCreate()

  import spark.implicits._

  val events = spark.read.parquet("data/security_events")

  println("\n===== APERÇU DES DONNÉES =====")
  events.show(false)

  println("\n===== SCHÉMA =====")
  events.printSchema()

  println("\n===== NOMBRE TOTAL D'ÉVÉNEMENTS =====")
  println(events.count())

  // Nombre d’événements par type
  println("\n===== ÉVÉNEMENTS PAR TYPE =====")
  events.groupBy($"eventType")
    .count()
    .orderBy(desc("count"))
    .show()

  // 2. Activité par utilisateur
  println("\n===== ACTIVITÉ PAR UTILISATEUR =====")
  events.groupBy($"user")
    .count()
    .orderBy(desc("count"))
    .show()

  //  3. Détection brute-force (>= 3 échecs)
  println("\n===== DÉTECTION BRUTE-FORCE =====")
  events
    .filter($"eventType" === "AUTH_FAILURE")
    .groupBy($"user")
    .count()
    .filter($"count" >= 3)
    .show()

  // 4. Comptes bloqués
  println("\n===== COMPTES BLOQUÉS =====")
  events
    .filter($"eventType" === "ACCOUNT_LOCKED")
    .show(false)

  // 5. Tokens révoqués
  println("\n===== TOKENS RÉVOQUÉS =====")
  events
    .filter($"eventType" === "TOKEN_REVOKED")
    .show(false)

  //  6. Timeline des événements
  println("\n===== TIMELINE =====")
  events
    .withColumn("date", from_unixtime($"timestamp" / 1000))
    .orderBy($"timestamp")
    .show(false)

  //  7. Activité par jour
  println("\n===== ACTIVITÉ PAR JOUR =====")
  events
    .withColumn("day", to_date(from_unixtime($"timestamp" / 1000)))
    .groupBy($"day")
    .count()
    .show()

  // 8. Utilisateurs suspects (échecs > succès)
  println("\n===== UTILISATEURS SUSPECTS =====")
  val failures = events.filter($"eventType" === "AUTH_FAILURE")
    .groupBy($"user")
    .count()
    .withColumnRenamed("count", "failures")

  val success = events.filter($"eventType" === "AUTH_SUCCESS")
    .groupBy($"user")
    .count()
    .withColumnRenamed("count", "success")

  failures.join(success, Seq("user"), "left")
    .na.fill(0)
    .filter($"failures" > $"success")
    .show()

  println("\n===== FIN ANALYSE =====")

  spark.stop()
}