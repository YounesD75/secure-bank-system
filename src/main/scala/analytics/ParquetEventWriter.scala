// package securebank.analytics

// import org.apache.spark.sql.{SparkSession}
// import java.time.LocalDate

// object ParquetEventWriter {

//   lazy val spark: SparkSession = {
//   val spark = SparkSession.builder()
//       .appName("SecureBank-Parquet-Writer")
//       .master("local[*]")
//       .config("spark.ui.enabled", "false")
//       .getOrCreate()

//     spark.sparkContext.setLogLevel("ERROR")
//     spark
//   }


//   import spark.implicits._

//   def write(event: SecurityEvent): Unit = {
//     Seq(event)
//       .toDS()
//       .write
//       .mode("append")
//       .parquet(s"data/security_events/day=${LocalDate.now}")
//   }
// }

// src/main/scala/securebank/analytics/ParquetEventWriter.scala
package securebank.analytics

import org.apache.spark.sql.{SparkSession, SaveMode}
import java.io.File


object ParquetEventWriter {
  private val parquetPath = "data/security_events"
  
  @transient private lazy val spark: SparkSession = SparkSession.builder()
    .appName("SecureBank-EventWriter")
    .master("local[*]")
    .config("spark.ui.enabled", "false")
    .getOrCreate()
  
  def write(event: SecurityEvent): Unit = {
    import spark.implicits._
    
    val file = new File(parquetPath)
    if (!file.exists()) file.mkdirs()
    
    Seq(event).toDF()
      .withColumn("day", org.apache.spark.sql.functions.to_date(
        org.apache.spark.sql.functions.from_unixtime($"timestamp" / 1000)
      ))
      .write
      .mode(SaveMode.Append)
      .parquet(parquetPath)
  }
  
  def stop(): Unit = {
    if (spark != null) spark.stop()
  }
}


