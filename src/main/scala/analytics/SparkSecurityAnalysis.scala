package securebank.analytics

import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions._
import org.apache.logging.log4j.LogManager

object SparkSecurityAnalysis extends App {
  
   val spark = SparkSession.builder()
    .appName("SecureBank-Security-Analysis")
    .master("local[*]")
    .config("spark.ui.enabled", "false")  // ← Désactive l'UI Spark
    .getOrCreate()

    spark.sparkContext.setLogLevel("ERROR")

  val logger = LogManager.getLogger("SecureBank")

  import spark.implicits._

  val events = spark.read.parquet("data/security_events")

  logger.info("Aperçu des données")
  events.show(false)

  logger.info("Schéma")
  events.printSchema()

  logger.info("Nombre total d'événements")
  logger.info(events.count())

  // Nombre d’événements par type
  logger.info("Événements par type")
  events.groupBy($"eventType")
    .count()
    .orderBy(desc("count"))
    .show()

  // 2. Activité par utilisateur
  logger.info("Activité par utilisateur")
  events.groupBy($"user")
    .count()
    .orderBy(desc("count"))
    .show()

  //  3. Détection brute-force (>= 3 échecs)
  logger.info("Détection brute-force")
  events
    .filter($"eventType" === "AUTH_FAILURE")
    .groupBy($"user")
    .count()
    .filter($"count" >= 3)
    .show()

  // 4. Comptes bloqués
  logger.info("Comptes bloqués")
  events
    .filter($"eventType" === "ACCOUNT_LOCKED")
    .show(false)

  // 5. Tokens révoqués
  logger.info("Tokens révoqués")
  events
    .filter($"eventType" === "TOKEN_REVOKED")
    .show(false)

  //  6. Timeline des événements
  logger.info("Timeline des événements")
  events
    .withColumn("date", from_unixtime($"timestamp" / 1000))
    .orderBy($"timestamp")
    .show(false)

  //  7. Activité par jour
  logger.info("Activité par jour")
  events
    .withColumn("day", to_date(from_unixtime($"timestamp" / 1000)))
    .groupBy($"day")
    .count()
    .show()

  // 8. Utilisateurs suspects (échecs > succès)
  logger.info("Utilisateurs suspects")
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

  logger.info("Fin de l'analyse")

  spark.stop()
}