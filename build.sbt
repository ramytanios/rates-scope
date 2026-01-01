Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val scala3 = "3.7.3"

ThisBuild / scalaVersion := scala3
ThisBuild / crossScalaVersions := Seq(scala3)
ThisBuild / semanticdbEnabled := true

// disables publish step
ThisBuild / githubWorkflowPublishTargetBranches := Seq.empty
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.zulu("21"))

lazy val V = new {
  val circe = "0.14.6"
  val cats = "2.12.0"
  val fs2 = "3.10.2"
  val kittens = "3.2.0"
  val literally = "1.2.0"
  val mouse = "1.3.0"
  val ff4s = "0.26.1"
  val http4s = "0.23.27"
  val monocle = "3.3.0"
  val logback = "1.5.7"
  val test = "0.7.29"
  val `cats-effect` = "3.5.4"
  val `cats-time` = "0.5.1"
  val `scala-java-time` = "2.5.0"
  val `ff4s-shoelace` = "0.0.1"
  val `ff4s-heroicons` = "0.0.1"
  val `ff4s-canvas` = "0.0.1"
  val `log-4cats` = "2.7.0"
  val `commons-math` = "3.6.1"
}

lazy val root =
  (project in file(".")).aggregate(`lib-dtos`.jvm, `lib-dtos`.js, lib)

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
    "org.scalameta" %% "munit" % V.test % Test
  ),
  scalacOptions -= "-Xfatal-warnings"
).dependsOn(`lib-dtos`.jvm)
