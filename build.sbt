name := "ScalaSessionTypes"

version := "1.0"

scalaVersion := "2.11.0"

crossScalaVersions := Seq("2.11.0", "2.10.4")

scalacOptions ++= Seq("-feature")


libraryDependencies += "com.chuusai" %% "shapeless" % "2.0.0" cross CrossVersion.binaryMapped {
  case "2.10" => "2.10.4"
  case other => other
}

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.2" % "provided"


libraryDependencies += "org.specs2" %% "specs2" % "2.3.11" % "test"

libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.3.2" % "test"