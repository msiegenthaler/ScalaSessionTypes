name := "ScalaSessionTypes"

version := "1.0"

scalacOptions ++= Seq("-feature")


libraryDependencies += "com.chuusai" % "shapeless" % "2.0.0" cross CrossVersion.full


libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.2" % "provided"


libraryDependencies += "org.specs2" %% "specs2" % "2.3.10" % "test"

libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.3.2" % "test"