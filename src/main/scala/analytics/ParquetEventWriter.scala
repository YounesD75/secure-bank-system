package securebank.analytics

import org.apache.spark.sql.{SparkSession, SaveMode}
import java.io.File
import scala.concurrent.{Future, ExecutionContext}
import java.util.concurrent.Executors


object ParquetEventWriter {
  private val parquetPath = "data/security_events"

  private val writeEc = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  @transient private lazy val spark: SparkSession = SparkSession.builder()
    .appName("SecureBank-EventWriter")
    .master("local[*]")
    .config("spark.ui.enabled", "false")
    .config("spark.driver.host", "localhost")
    .config("spark.driver.bindAddress", "127.0.0.1")
    .config("spark.hadoop.mapreduce.fileoutputcommitter.algorithm.version", "2")
    .config("spark.sql.parquet.compression.codec", "none")
    .getOrCreate()

  def write(event: SecurityEvent): Unit = {
    Future {
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
    }(writeEc)
  }

  def stop(): Unit = {
    if (spark != null) spark.stop()
  }
}


