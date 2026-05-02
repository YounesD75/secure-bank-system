// src/main/scala/securebank/analytics/SecurityAnalyzer.scala
package securebank.analytics

import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions._
import org.apache.log4j.{Level, Logger}

object SecurityAnalyzer {
  
  @transient private lazy val spark: SparkSession = SparkSession.builder()
    .appName("SecureBank-Analyzer")
    .master("local[*]")
    .config("spark.ui.enabled", "false")
    .getOrCreate()
  
  // Désactiver les logs Spark bruyants
  Logger.getLogger("org.apache.spark").setLevel(Level.ERROR)
  Logger.getLogger("org.eclipse.jetty").setLevel(Level.ERROR)
  Logger.getLogger("org.apache.hadoop").setLevel(Level.ERROR)
  
  import spark.implicits._
  
  case class SecurityStats(
    totalEvents: Long,
    authFailures: Long,
    authSuccess: Long,
    lockedAccounts: Long,
    revokedTokens: Long,
    bruteForceUsers: Seq[String],
    suspiciousUsers: Seq[(String, Long, Long)]
  )
  
  def analyze(): Option[SecurityStats] = {
    try {
      val eventsPath = "data/security_events"
      val file = new java.io.File(eventsPath)
      
      if (!file.exists() || Option(file.listFiles()).isEmpty || file.listFiles().length == 0) {
        println("⚠️ Aucune donnée d'événement trouvée. Lancez d'abord SecureBankApp.")
        return None
      }
      
      val events = spark.read.parquet(eventsPath)
      
      val total = events.count()
      val failures = events.filter($"eventType" === "AUTH_FAILURE").count()
      val success = events.filter($"eventType" === "AUTH_SUCCESS").count()
      val locked = events.filter($"eventType" === "ACCOUNT_LOCKED").count()
      val revoked = events.filter($"eventType" === "TOKEN_REVOKED").count()
      
      // Utilisateurs en brute-force (>= 3 échecs)
      val bruteForceUsers = events.filter($"eventType" === "AUTH_FAILURE")
        .groupBy($"user")
        .count()
        .filter($"count" >= 3)
        .select($"user")
        .as[String]
        .collect()
        .toSeq
      
      // Utilisateurs suspects (échecs > succès)
      val failuresByUser = events.filter($"eventType" === "AUTH_FAILURE")
        .groupBy($"user")
        .count()
        .withColumnRenamed("count", "failures")
      
      val successByUser = events.filter($"eventType" === "AUTH_SUCCESS")
        .groupBy($"user")
        .count()
        .withColumnRenamed("count", "success")
      
      val suspicious = failuresByUser.join(successByUser, Seq("user"), "left")
        .na.fill(0)
        .filter($"failures" > $"success")
        .select($"user", $"failures", $"success")
        .as[(String, Long, Long)]
        .collect()
        .toSeq
      
      Some(SecurityStats(total, failures, success, locked, revoked, bruteForceUsers, suspicious))
      
    } catch {
      case e: Exception =>
        println(s"❌ Erreur d'analyse : ${e.getMessage}")
        None
    }
  }
  
  def stop(): Unit = {
    if (spark != null) spark.stop()
  }
  
  // Pour tester directement
  def main(args: Array[String]): Unit = {
    analyze() match {
      case Some(stats) =>
        println(s"Total événements: ${stats.totalEvents}")
        println(s"Échecs: ${stats.authFailures}")
        println(s"Succès: ${stats.authSuccess}")
        println(s"Comptes bloqués: ${stats.lockedAccounts}")
        println(s"Brute-force: ${stats.bruteForceUsers.mkString(", ")}")
      case None =>
        println("Aucune donnée")
    }
    stop()
  }
}