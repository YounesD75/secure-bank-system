name         := "SecureBank"
version      := "0.1"
scalaVersion := "2.13.12"

val AkkaVersion = "2.6.20"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed"         % AkkaVersion,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % AkkaVersion % Test,
  "ch.qos.logback"    %  "logback-classic"           % "1.2.11",
  "org.scalatest"     %% "scalatest"                 % "3.2.17"    % Test
)
