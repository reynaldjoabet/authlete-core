name := """Authlete-Core"""

version := "1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.3"
ThisBuild / scalacOptions ++= Seq(
  "-no-indent",
  // "-deprecation", // Warns about deprecated APIs
  "-feature", // Warns about advanced language features
  // "-unchecked",//[warn] Flag -unchecked set repeatedly
  // "-Wunused:imports",
  //   "-Wunused:privates",
  //   "-Wunused:locals",
  //   "-Wunused:explicits",
  //   "-Wunused:implicits",
  //   "-Wunused:params",
  //   "-Wvalue-discard",
  // "-language:strictEquality",
  "-Xmax-inlines:100000"
)
val CirceVersion = "0.14.13"
val Http4sVersion = "0.23.28"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.outr" %% "scribe" % "3.16.1",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.35.3",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.35.3" % "provided",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-circe" % "2.35.3",
      "com.softwaremill.sttp.client4" %% "core" % "4.0.3",
      "com.softwaremill.sttp.client4" %% "jsoniter" % "4.0.3",
      "com.outr" %% "scribe" % "3.16.1",
      "com.outr" %% "scribe-slf4j" % "3.16.1",
      "com.outr" %% "scribe-cats" % "3.16.1",
      "org.http4s" %% "http4s-ember-client" % Http4sVersion,
      "org.http4s" %% "http4s-circe" % Http4sVersion,
      "org.http4s" %% "http4s-dsl" % Http4sVersion,
      "org.http4s" %% "http4s-client" % Http4sVersion,
      "io.circe" %% "circe-core" % CirceVersion,
      "io.circe" %% "circe-generic" % CirceVersion,
      "io.circe" %% "circe-parser" % CirceVersion
    )
  )
  .settings(
    // testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v"),
    libraryDependencies ++= Seq(
      "qa.hedgehog" %% "hedgehog-sbt" % "0.12.0" % Test,
      "qa.hedgehog" %% "hedgehog-core" % "0.12.0" % Test,
      "qa.hedgehog" %% "hedgehog-runner" % "0.12.0" % Test
    )
  )
//.aggregate() // no arguments = aggregate nothing

Global / onChangedBuildSource := IgnoreSourceChanges

ThisBuild / Compile / fork := true
//ThisBuild / Runtime / fork := true
ThisBuild / Test / parallelExecution := true
ThisBuild / Test / fork := true
//ThisBuild / run / fork := true wrong,should be ThisBuild / Compile/ fork := true

//Metals requires the semanticdb compiler plugin

Global / parallelExecution := true

Global / concurrentRestrictions += Tags.limit(Tags.Compile, 4) // 4 cores
