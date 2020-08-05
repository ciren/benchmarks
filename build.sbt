import sbt._
import Keys._
import _root_.scalafix.sbt.ScalafixPlugin.autoImport.scalafixSemanticdb

val scalazVersion     = "7.2.20"
val scalacheckVersion = "1.12.6" // remain on 1.12.x because scalaz-binding is built against this version
val spireVersion      = "0.13.0"
val shapelessVersion  = "2.3.3"

Global / onChangedBuildSource := ReloadOnSourceChanges

inThisBuild(
  List(
    organization := "net.cilib",
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
    )
  )
)

name := "benchmarks"

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
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  "bintray/non" at "https://dl.bintray.com/non/maven"
)

libraryDependencies ++= Seq(
  "org.scalaz"     %% "scalaz-core"               % scalazVersion,
  "org.spire-math" %% "spire"                     % spireVersion,
  "net.cilib"      %% "cilib-core"                % "2.0.1",
  "com.chuusai"    %% "shapeless"                 % shapelessVersion,
  "org.scalacheck" %% "scalacheck"                % scalacheckVersion % "test",
  "org.scalaz"     %% "scalaz-scalacheck-binding" % scalazVersion % "test",
  compilerPlugin(scalafixSemanticdb)
)

publishMavenStyle := true

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

scalafixDependencies in ThisBuild += "com.nequissimus" %% "sort-imports" % "0.5.0"
