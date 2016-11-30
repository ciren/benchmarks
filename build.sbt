import sbt._
import Keys._

val scalazVersion     = "7.2.7"
val spireVersion      = "0.13.0"
val scalacheckVersion = "1.12.6"

organization := "net.cilib"

name := "benchmarks"

version := "0.1.1"

scmInfo := Some(ScmInfo(url("https://github.com/cirg-up/benchmarks"),
    "git@github.com:cirg-up/benchmarks.git"))

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

homepage := Some(url("http://cirg-up.github.io/cilib"))

scalaVersion := "2.11.8"

crossScalaVersions := Seq("2.11.8", "2.12.0")

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:experimental.macros",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  "bintray/non" at "http://dl.bintray.com/non/maven"
)

libraryDependencies ++= Seq(
  "org.scalaz"     %% "scalaz-core" % scalazVersion,
  "org.spire-math" %% "spire"       % spireVersion,
  "org.scalacheck" %% "scalacheck" % scalacheckVersion % "test",
  "org.scalaz"     %% "scalaz-scalacheck-binding" % scalazVersion % "test"
)

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

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
)
