fork := true

name         := "SecureBank"
version      := "0.1"
scalaVersion := "2.13.12"

val AkkaVersion  = "2.6.20"
val SparkVersion = "3.5.3"
val Log4jVersion = "2.19.0"

javaOptions ++= Seq(
  "--add-exports=java.base/sun.nio.ch=ALL-UNNAMED",
  "--add-opens=java.base/java.nio=ALL-UNNAMED",
  "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED",
  "--add-opens=java.base/java.lang=ALL-UNNAMED",
  "--add-opens=java.base/java.lang.invoke=ALL-UNNAMED",
  "--add-opens=java.base/java.util=ALL-UNNAMED"
)

libraryDependencies ++= Seq(
  // --- Akka ---
  "com.typesafe.akka" %% "akka-actor-typed"         % AkkaVersion,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % AkkaVersion % Test,
  
  // --- Apache Spark ---
  "org.apache.spark"  %% "spark-core"               % SparkVersion,
  "org.apache.spark"  %% "spark-sql"                % SparkVersion,
  
  // --- Logging  ---
  "org.apache.logging.log4j" %% "log4j-api-scala"   % "12.0",
  "org.apache.logging.log4j" %  "log4j-slf4j-impl" % Log4jVersion,
  "org.apache.logging.log4j" %  "log4j-core"        % Log4jVersion,

  // --- Tests ---
  "org.scalatest"     %% "scalatest"                % "3.2.17"    % Test
)