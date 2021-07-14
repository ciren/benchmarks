import sbt._
import Keys._

val scalazVersion     = "7.3.2"
val scalacheckVersion = "1.14.3" // remain on 1.12.x because scalaz-binding is built against this version
val spireVersion      = "0.17.0-RC1"
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
    ),
    semanticdbEnabled := true, // enable SemanticDB
    semanticdbVersion := scalafixSemanticdb.revision // only required for Scala 2.x
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
  Resolver.sonatypeRepo("snapshots")
  //"bintray/non" at "https://dl.bintray.com/non/maven"
)

libraryDependencies ++= Seq(
  "org.scalaz"     %% "scalaz-core"               % scalazVersion,
  "org.typelevel" %% "spire"                     % spireVersion,
  "net.cilib"      %% "cilib-core"                % "2.0.1",
  "com.chuusai"    %% "shapeless"                 % shapelessVersion,
  "org.scalacheck" %% "scalacheck"                % scalacheckVersion % "test",
  "org.scalaz"     %% "scalaz-scalacheck-binding" % scalazVersion % "test"
//  compilerPlugin(scalafixSemanticdb)
)

publishMavenStyle := true

Test / publishArtifact := false

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

ThisBuild / scalafixDependencies += "com.nequissimus" %% "sort-imports" % "0.5.0"
