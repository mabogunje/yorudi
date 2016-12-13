name := "yorudi"
scalaVersion := "2.11.8"
fork := true


version := "0.1-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.5" % "test",
  "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"
)
