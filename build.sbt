Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val scala3 = "3.7.3"

ThisBuild / scalaVersion := scala3
ThisBuild / crossScalaVersions := Seq(scala3)
ThisBuild / semanticdbEnabled := true

// disables publish step
ThisBuild / githubWorkflowPublishTargetBranches := Seq.empty
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.zulu("21"))

lazy val V = new {
  val circe = "0.14.15"
  val cats = "2.13.0"
  val fs2 = "3.10.2"
  val kittens = "3.2.0"
  val literally = "1.2.0"
  val mouse = "1.3.0"
  val ff4s = "0.26.1"
  val http4s = "0.23.27"
  val monocle = "3.3.0"
  val logback = "1.5.7"
  val munit = "1.2.1"
  val `munit-cats-effect` = "2.0.0-M3"
  val `cats-effect` = "3.5.4"
  val `cats-time` = "0.5.1"
  val `log-4cats` = "2.7.0"
  val `commons-math` = "3.6.1"
}

lazy val root =
  (project in file(".")).aggregate(lib, `json-rpc`, `lib-dtos`.jvm, `lib-dtos`.js)

lazy val `lib-dtos` = crossProject(JSPlatform, JVMPlatform)
  .in(file("lib-dtos"))
  .settings(
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++=
      Seq(
        "io.circe" %% "circe-core" % V.circe,
        "io.circe" %% "circe-generic" % V.circe,
        "io.circe" %% "circe-literal" % V.circe,
        "io.circe" %% "circe-parser" % V.circe
      ),
    scalacOptions -= "-Xfatal-warnings"
  )

lazy val lib = project.in(file("lib")).settings(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % V.cats,
    "org.typelevel" %% "literally" % V.literally,
    "org.apache.commons" % "commons-math3" % V.`commons-math`,
    "org.scalameta" %% "munit" % V.munit % Test
  ),
  scalacOptions -= "-Xfatal-warnings"
).dependsOn(`lib-dtos`.jvm)

lazy val `json-rpc` = project
  .in(file("json-rpc"))
  .enablePlugins(JavaAppPackaging)
  .settings(
    fork := true,
    libraryDependencies ++=
      Seq(
        "ch.qos.logback" % "logback-classic" % V.logback,
        "io.circe" %% "circe-core" % V.circe,
        "io.circe" %% "circe-generic" % V.circe,
        "io.circe" %% "circe-literal" % V.circe,
        "io.circe" %% "circe-parser" % V.circe,
        "org.typelevel" %% "cats-core" % V.cats,
        "co.fs2" %% "fs2-core" % V.fs2,
        "co.fs2" %% "fs2-io" % V.fs2,
        "org.typelevel" %% "kittens" % V.kittens,
        "org.typelevel" %% "mouse" % V.mouse,
        "org.typelevel" %% "cats-effect" % V.`cats-effect`,
        "org.typelevel" %% "cats-effect-std" % V.`cats-effect`,
        "org.typelevel" %% "cats-time" % V.`cats-time`,
        "org.typelevel" %% "literally" % V.literally,
        "org.typelevel" %% "log4cats-core" % V.`log-4cats`,
        "org.typelevel" %% "log4cats-slf4j" % V.`log-4cats`,
        "org.apache.commons" % "commons-math3" % V.`commons-math`,
        "org.http4s" %% "http4s-dsl" % V.http4s,
        "org.http4s" %% "http4s-circe" % V.http4s,
        "org.http4s" %% "http4s-ember-server" % V.http4s,
        "org.typelevel" %% "munit-cats-effect" % V.`munit-cats-effect` % Test
      ),
    scalacOptions -= "-Xfatal-warnings"
  )
  .dependsOn(lib, `lib-dtos`.jvm)
