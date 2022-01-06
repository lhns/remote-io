ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.7"

lazy val root = (project in file("."))
  .settings(
    name := "remote-io",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.2.0",
      "org.http4s" %% "http4s-server" % "0.23.7",
      "org.http4s" %% "http4s-client" % "0.23.7",
    )
  )
