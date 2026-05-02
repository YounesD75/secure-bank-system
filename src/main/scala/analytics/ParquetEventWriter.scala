package securebank.analytics

import org.apache.spark.sql.{SparkSession}
import java.time.LocalDate

object ParquetEventWriter {

  lazy val spark: SparkSession = {
  val spark = SparkSession.builder()
      .appName("SecureBank-Parquet-Writer")
      .master("local[*]")
      .config("spark.ui.enabled", "false")
      .getOrCreate()

    spark.sparkContext.setLogLevel("ERROR")
    spark
  }


  import spark.implicits._

  def write(event: SecurityEvent): Unit = {
    Seq(event)
      .toDS()
      .write
      .mode("append")
      .parquet(s"data/security_events/day=${LocalDate.now}")
  }
}
