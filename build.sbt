/* basic project info */
name := "marc2bs"

organization := "fi.sange"

version := "0.1.0-SNAPSHOT"

mainClass := Some("fi.sange.marc2bs.ImportMarcToBookSampoSPARQL")

// description := "this project can foo a bar!"

homepage := Some(url("https://github.com/jiemakel/marc2bs"))

startYear := Some(2013)

licenses := Seq(
  ("MIT", url("http://opensource.org/licenses/MIT"))
)

scmInfo := Some(
  ScmInfo(
    url("https://github.com/jiemakel/marc2bs"),
    "scm:git:https://github.com/jiemakel/marc2bs.git",
    Some("scm:git:git@github.com:jiemakel/marc2bs.git")
  )
)

organizationName := "Sange Coop"

/* scala versions and options */
scalaVersion := "2.10.3"

crossScalaVersions := Seq(
  "2.8.0", "2.8.1", "2.8.2",
  "2.9.0", "2.9.0-1",
  "2.9.1", "2.9.1-1",
  "2.9.2",
  "2.9.3"
)

// These options will be used for *all* versions.
scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-encoding", "UTF-8",
  // "-Xcheckinit" // for debugging only, see https://github.com/paulp/scala-faq/wiki/Initialization-Order
  "-optimise"   // this option will slow your build
)

scalacOptions ++= Seq(
  "-Yclosure-elim",
  "-Yinline",
  "-Yinline-warnings"
)

// These language flags will be used only for 2.10.x.
// Uncomment those you need, or if you hate SIP-18, all of them.
scalacOptions <++= scalaVersion map { sv =>
  if (sv startsWith "2.10") List(
    "-Xverify",
    "-Ywarn-all",
    "-feature"
    // "-language:postfixOps",
    // "-language:reflectiveCalls",
    // "-language:implicitConversions"
    // "-language:higherKinds",
    // "-language:existentials",
    // "-language:experimental.macros",
    // "-language:experimental.dynamics"
  )
  else Nil
}

javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation")

// CONTINUATIONS
// autoCompilerPlugins := true
// addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.9.2")
// scalacOptions += "-P:continuations:enable"

/* dependencies */
libraryDependencies ++= Seq (
  // -- lang --
  // "org.apache.commons" % "commons-lang3" % "3.1",
  "org.scalaz" %% "scalaz-core" % "7.0.0",
  "org.scalaz" %% "scalaz-effect" % "7.0.0",
  "org.scalaz" %% "scalaz-typelevel" % "7.0.0",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.0.0" % "test",
  // -- util --
  // "com.github.nscala-time" %% "nscala-time" % "0.2.0",
  // "org.spire-math" % "spire_2.10.0" % "0.3.0-M7",
  "com.github.scopt" %% "scopt" % "3.1.0",
  // "org.rogach" %% "scallop" % "0.6.3",
  // -- collections --
  // "com.google.guava" % "guava" % "13.0.1",
  // "com.chuusai" %% "shapeless" % "1.2.3",
  // "de.sciss" %% "fingertree" % "1.2.+",
  // "com.assembla.scala-incubator" % "graph-core_2.10" % "1.6.0",
  // -- io --
  // "commons-io" % "commons-io" % "2.4",
  "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.2",
  "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2",
  // -- logging & configuration --
  "com.typesafe" %% "scalalogging-slf4j" % "1.0.1",
  "ch.qos.logback" % "logback-classic" % "1.0.13",
  "com.typesafe" % "config" % "1.0.2",
  // -- database drivers --
  // "com.h2database" % "h2" % "1.2.127",
  // "mysql" % "mysql-connector-java" % "5.1.10",
  // -- persistence --
  // "com.novus" %% "salat" % "1.9.2-SNAPSHOT",
  // "net.debasishg" %% "redisclient" % "2.9",
  // "com.typesafe" %% "slick" % "1.0.0-RC1",
  // "org.squeryl" %% "squeryl" % "0.9.5-6",
  // "com.github.nikita-volkov" % "sorm" % "0.3.5",
  // "fi.reaktor" %% "sqltyped" % "0.1.0",
  // "com.imageworks.scala-migrations" %% "scala-migrations" % "1.1.1",
  // -- serialization --
  // "org.json4s" %% "json4s-native" % "3.1.0",
  // "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.1.3",
  // -- concurrency --
  // "com.typesafe.akka" %% "akka-actor" % "2.2-SNAPSHOT",
  // "org.scala-stm" %% "scala-stm" % "0.7",
  // -- network --
  // "net.databinder.dispatch" %% "dispatch-core" % "0.9.2",
  // -- testing --
  "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
  "org.specs2" %% "specs2" % "2.2.2" % "test",
  // "org.scalatest" % "scalatest_2.10" % "2.0.M5b"
  // -- actual --
  "org.w3" %% "banana-jena" % "0.5-SNAPSHOT" exclude("log4j","log4j") exclude("org.slf4j","slf4j-log4j12") exclude("javax.xml.stream","stax-api"),
  "org.marc4j" % "marc4j" % "2.5",
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.0",
  "org.geonames" % "geonames-ws-client" % "1.1.10-2"
)

/* you may need these repos */
resolvers ++= Seq(
  //Resolver.sonatypeRepo("snapshots"),
  // Resolver.typesafeIvyRepo("snapshots")
  // Resolver.typesafeIvyRepo("releases")
  Resolver.typesafeRepo("releases"),
  // Resolver.typesafeRepo("snapshots")
  // JavaNet2Repository,
  // JavaNet1Repository,
  // "spray repo" at "http://repo.spray.io",
  "id-maven-repo" at "http://maven.indexdata.com",
//  Resolver.url("Local Ivy Repository", url(Path.userHome.asFile.toURI.toURL + ".ivy2/local"))(Resolver.ivyStylePatterns),
  "Local Maven Repository" at Path.userHome.asFile.toURI.toURL + ".m2/repository"
)

// ivyXML := <dependencies>
//             <exclude module="logback-classic" />
//           </dependencies>

/* testing */
parallelExecution in Test := false

// testOptions += Tests.Argument(TestFrameworks.Specs2, "console", "junitxml")

// parallelExecution in Global := false //no parallelism between subprojects

/* sbt behavior */
logLevel in compile := Level.Warn

traceLevel := 5

offline := false

/* publishing */
publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) Some(
    "snapshots" at nexus + "content/repositories/snapshots"
  )
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

mappings in (Compile, packageBin) ~= { (ms: Seq[(File, String)]) =>
  ms filter { case (file, toPath) =>
      toPath != "application.conf"
  }
}

publishArtifact in Test := false

publishArtifact in (Compile, packageDoc) := false

publishArtifact in (Compile, packageSrc) := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <developers>
    <developer>
      <id>jiemakel</id>
      <name>Eetu Mäkelä</name>
      <email>eetu.makela@sange.fi</email>
//    <url>http://johndoe.com</url>
    </developer>
  </developers>
)
