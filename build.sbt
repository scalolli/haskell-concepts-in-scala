name := "haskell-concepts-in-scala"

version := "0.1"

scalaVersion := "2.12.5"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xfatal-warnings")

libraryDependencies ++=
  Seq(
    "org.typelevel"              %% "cats-core"                 % "1.1.0",
    "org.typelevel"              %% "cats-laws"                 % "1.0.1" % Test,
    "org.typelevel"              %% "cats-testkit"              % "1.0.1" % Test,
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % Test
  )
