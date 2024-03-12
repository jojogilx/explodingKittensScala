ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.13"

val catsVersion        = "2.10.0"
val catsEffect3Version = "3.3.0"
val http4sVersion = "0.23.26"


libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,

  "org.typelevel"            %% "cats-effect"                   % catsEffect3Version,

  "org.http4s" %% "http4s-ember-client" % http4sVersion,
  "org.http4s" %% "http4s-ember-server" % http4sVersion,
  "org.http4s" %% "http4s-dsl"          % http4sVersion,
)

lazy val root = (project in file("."))
  .settings(
    name := "ExplodingKittens"
  )
