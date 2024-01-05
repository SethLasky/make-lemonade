enablePlugins(ScalaJSPlugin)

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

scalaJSUseMainModuleInitializer := true

lazy val root = (project in file("."))
  .settings(
    name := "make-lemonade",
    libraryDependencies ++= Seq(
      "com.armanbilge" %%% "calico" % "0.2.1",
      "org.typelevel" %%% "kittens" % "3.1.0"
    )
  )
