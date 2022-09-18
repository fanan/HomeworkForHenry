ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "HomeworkForHenry",
    libraryDependencies ++= Seq(
      "org.apache.pdfbox" % "pdfbox" % "2.0.22",
      "com.github.scopt" %% "scopt" % "4.0.1",
      "org.scalatest" %% "scalatest" % "3.2.12" % Test
    ),
    assembly / mainClass := Some("math.arithmetic.Runner")
  )
