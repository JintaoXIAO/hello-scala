ThisBuild / scalaVersion     := "2.13.10"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.haskell"
ThisBuild / organizationName := "haskell"

val dep_cats_core = "org.typelevel" %% "cats-core" % "2.9.0"
val dep_cats_effect = "org.typelevel" %% "cats-effect" % "3.4.9"
val dep_cats_free = "org.typelevel" %% "cats-free" % "2.9.0"


lazy val scalawithcats = (project in file ("scalawithcats"))
  .settings(
    name := "scala-with-cats",
    libraryDependencies += dep_cats_core,
    libraryDependencies += dep_cats_free,
    addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
  )

lazy val catseffect = (project in file ("catseffect"))
  .settings(
    name := "cats-effect",
    libraryDependencies ++= Seq(dep_cats_core, dep_cats_effect)
  )

lazy val fpinscala = (project in file ("fpinscala"))
  .settings(
    name := "fp-in-scala"
  )