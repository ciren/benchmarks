import sbt._
import Keys._

val zioVersion = "1.0.12"

val zioPrelude = "dev.zio" %% "zio-prelude"  % "1.0.0-RC5"
val zioTest    = "dev.zio" %% "zio-test"     % zioVersion % Test
val zioTestSbt = "dev.zio" %% "zio-test-sbt" % zioVersion % Test

val attoCore = "org.tpolecat" %% "atto-core" % "0.9.5"

val cilibCore = "net.cilib" %% "core" % "2.0.0+97-d11405f0-SNAPSHOT"

Global / onChangedBuildSource := ReloadOnSourceChanges

inThisBuild(
  List(
    organization := "net.cilib",
    crossScalaVersions := Seq("2.12.12", "2.13.6"),// "3.0.0"),
    homepage := Some(url("https://cilib,net")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "gpampara",
        "Gary Pamparà",
        "",
        url("http://gpampara.github.io")
      )
    ),
    scmInfo := Some(
      ScmInfo(url("https://github.com/ciren/benchmarks/"), "scm:git:git@github.com:ciren/benckmarks.git")
    ),
    semanticdbEnabled := true, // enable SemanticDB
    semanticdbVersion := scalafixSemanticdb.revision // only required for Scala 2.x
  )
)

lazy val benchmarks =
  project.in(file("."))
    .settings(
      name := "benchmarks",
      scalacOptions ++= Seq(
        "-deprecation",
        "-encoding",
        "UTF-8",
        "-feature",
        "-language:existentials",
        "-language:higherKinds",
        "-language:experimental.macros",
        "-unchecked",
        // "-Xfatal-warnings",
        "-Xlint",
        "-Yno-adapted-args",
        "-Ywarn-dead-code",
        "-Ywarn-numeric-widen",
        "-Ywarn-value-discard",
        // "-verbose",
        "-Xfuture"
      ),
      resolvers ++= Seq(
        Resolver.sonatypeRepo("releases"),
        Resolver.sonatypeRepo("snapshots")
      ),
      libraryDependencies := Seq(
        cilibCore,
        zioPrelude,
        attoCore,
        zioTest, zioTestSbt
      ),
      testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
      publishMavenStyle := true,
      Test / publishArtifact := false,
      pomExtra := (
        <developers>
          {
            Seq(
              ("robgarden", "Robert Garden"),
              ("gpampara", "Gary Pamparà"),
              ("filinep", "Filipe Nepomuceno"),
              ("benniel", "Bennie Leonard")
            ).map {
              case (id, name) =>
                <developer>
                <id>{id}</id>
                <name>{name}</name>
                <url>http://github.com/{id}</url>
                    </developer>
            }
          }
          </developers>
      ),
      ThisBuild / scalafixDependencies += "com.nequissimus" %% "sort-imports" % "0.5.0"
    )
