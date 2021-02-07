name := "scalangj"

version := "0.1.5"


sbtVersion in Global := "1.3.12"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

resolvers += "Maven Repository" at "https://mvnrepository.com/artifact/"

resolvers += "clojars" at "https://clojars.org/repo"


libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.10"

libraryDependencies += "org.typelevel" %% "paiges-core" % "0.3.0"

scalacOptions ++= Seq("-feature", "-deprecation", "-Yresolve-term-conflict:package", "-Ypartial-unification" )



// for publishing to github

organization := "com.github.luzhuomi"

publishMavenStyle := true


publishTo := Some(Resolver.file("mavenLocal",  new File(Path.userHome.absolutePath+"/git/mavenrepo/")))


publishArtifact in Test := false


pomIncludeRepository := { _ => false }


pomExtra := (
  <url>https://github.com/luzhuomi/scalangj</url>
  <licenses>
    <license>
      <name>Apache License 2.0</name>
      <url>https://www.apache.org/licenses/LICENSE-2.0</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:luzhuomi/scalangj.git</url>
    <connection>scm:git:git@github.com:luzhuomi/scalangj.git</connection>
  </scm>
  <developers>
    <developer>
      <id>luzhuomi</id>
      <name>Kenny Zhuo Ming Lu</name>
      <url>http://sites.google.com/site/luzhuomi</url>
    </developer>
  </developers>)