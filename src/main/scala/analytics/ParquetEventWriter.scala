package securebank.analytics

import org.apache.spark.sql.{SparkSession}
import java.time.LocalDate

object ParquetEventWriter {

  lazy val spark: SparkSession =
    SparkSession.builder()
      .appName("SecureBank-Parquet-Writer")
      .master("local[*]")
      .getOrCreate()

  import spark.implicits._

  def write(event: SecurityEvent): Unit = {
    Seq(event)
      .toDS()
      .write
      .mode("append")
      .parquet(s"data/security_events/day=${LocalDate.now}")
  }
}
