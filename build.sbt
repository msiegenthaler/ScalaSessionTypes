name := "ScalaSessionTypes"

version := "1.0"


libraryDependencies += "com.chuusai" % "shapeless" % "2.0.0" cross CrossVersion.full

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.2" % "provided"