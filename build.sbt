ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .settings(
    name := "retirement_simulator"
  )

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.19",
  "org.typelevel" %% "cats-core" % "2.12.0"
)



