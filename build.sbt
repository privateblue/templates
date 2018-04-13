name := "language"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",
  "org.parboiled" %% "parboiled" % "2.1.4",
  "org.typelevel" %% "cats-core" % "1.0.1"
)
