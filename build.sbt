val scala3Version = "3.10.0"

val catsEffectVersion = "3.7.1"
val zioVersion        = "2.1.27"
val kyoVersion        = "1.0.0-RC7"
val munitVersion      = "1.3.6"

ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := scala3Version
ThisBuild / scalacOptions ++= Seq(
    "-Wvalue-discard",
    "-Wnonunit-statement",
    "-Wconf:msg=(unused.*value|discarded.*value|pure.*statement):error",
    "-language:strictEquality"
)

lazy val root = project
  .in(file("."))
  .aggregate(common, raw, catsEffect, zio, kyo)
  .settings(
    name := "FP-Scala-2026",
  )

lazy val common = project
  .in(file("modules/common"))
  .settings(
    name := "fp-common",
    libraryDependencies ++= Seq(
      "eu.joaocosta"  %% "minart" % "0.6.6",
      "org.scalameta" %% "munit"  % munitVersion % Test
    )
  )

lazy val raw = project
  .in(file("modules/raw"))
  .dependsOn(common)
  .settings(
    name := "fp-raw",
    libraryDependencies += "org.scalameta" %% "munit" % munitVersion % Test
  )

lazy val catsEffect = project
  .in(file("modules/cats-effect"))
  .dependsOn(common)
  .settings(
    name := "fp-cats-effect",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect"       % catsEffectVersion,
      "org.typelevel" %% "munit-cats-effect" % "2.2.1" % Test
    )
  )

lazy val zio = project
  .in(file("modules/zio"))
  .dependsOn(common)
  .settings(
    name := "fp-zio",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"          % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test
    )
  ) 

lazy val kyo = project
  .in(file("modules/kyo"))
  .dependsOn(common)
  .settings(
    name := "fp-kyo",
    libraryDependencies ++= Seq(
      "io.getkyo"     %% "kyo-core" % kyoVersion,
      "org.scalameta" %% "munit"    % munitVersion % Test
    )
  )

