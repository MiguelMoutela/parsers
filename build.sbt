lazy val root = (project in file("."))
  .enablePlugins(TutPlugin)
  .settings(
    inThisBuild(
      List(
        scalaVersion := "2.12.4",
        version := "0.1.0-SNAPSHOT"
      )),
    name := "parser combinators",
    scalacOptions += "-Ypartial-unification",
      // scalacOptions in Tut ~= filterConsoleScalacOptions,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
    libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"
  )
