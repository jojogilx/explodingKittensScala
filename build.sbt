ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.13"

val catsVersion        = "2.10.0"
val catsEffect3Version = "3.3.0"


libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,

  "org.typelevel"            %% "cats-effect"                   % catsEffect3Version,
)

lazy val root = (project in file("."))
  .settings(
    name := "ExplodingKittens"
  )
