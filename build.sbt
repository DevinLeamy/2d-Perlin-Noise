import sbt.Keys.libraryDependencies

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"


lazy val root = (project in file("."))
  .settings(
    name := "Scala Workshop",
  )

libraryDependencies += "org.creativescala" %% "doodle" % "0.10.1"
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
