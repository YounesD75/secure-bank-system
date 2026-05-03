fork := true

name         := "SecureBank"
version      := "0.1"
scalaVersion := "2.13.12"

val AkkaVersion  = "2.6.20"
val SparkVersion = "3.4.1"

javaOptions ++= Seq(
  "--add-exports=java.base/sun.nio.ch=ALL-UNNAMED"
)


libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed"         % AkkaVersion,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % AkkaVersion % Test,
  "ch.qos.logback"    %  "logback-classic"           % "1.2.11",
  "org.scalatest"     %% "scalatest"                 % "3.2.17"    % Test,
  "org.apache.spark" %% "spark-core" % SparkVersion,
  "org.apache.spark" %% "spark-sql"  % SparkVersion,
  "org.apache.logging.log4j" %% "log4j-api-scala"   % "12.0" 
)
