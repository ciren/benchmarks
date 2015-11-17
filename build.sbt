import sbt._
import Keys._

val scalazVersion     = "7.1.4"
val spireVersion      = "0.11.0"
val scalacheckVersion = "1.12.5"

organization := "net.cilib"

name := "cilib-benchmarks"

scmInfo := Some(ScmInfo(url("https://github.com/cirg-up/benchmarks"),
    "git@github.com:cirg-up/benchmarks.git"))

scalaVersion := "2.11.7"

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
