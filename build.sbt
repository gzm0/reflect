// shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.{crossProject, CrossType}

lazy val reflect = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(
    crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.5"),
    scalaVersion := crossScalaVersions.value.last,
    libraryDependencies += "com.lihaoyi" %%% "utest" % "0.6.3" % "test",
    testFrameworks += new TestFramework("utest.runner.Framework")
  )

lazy val reflectJVM = reflect.jvm
lazy val reflectJS = reflect.js
