name := "SecureBank"
version := "0.1"
scalaVersion := "2.13.12"

val AkkaVersion = "2.6.20"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
  "ch.qos.logback"    %  "logback-classic"   % "1.2.11"
)