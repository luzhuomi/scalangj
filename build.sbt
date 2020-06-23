name := "scalangj"

version := "1.0"


sbtVersion in Global := "1.3.12"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

resolvers += "Maven Repository" at "https://mvnrepository.com/artifact/"

resolvers += "clojars" at "https://clojars.org/repo"


libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.10"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"

// libraryDependencies += "org.typelevel" %% "kittens" % "2.1.0"

scalacOptions ++= Seq("-feature", "-deprecation", "-Yresolve-term-conflict:package", "-Ypartial-unification" )
