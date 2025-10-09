name := "yorudi"

scalaVersion := "2.12.18"

fork := true

version := "1.0"

libraryDependencies ++= { 
    val scalatraVersion = "2.7.0"
    val jettyVersion = "9.3.0.M1"
    val slf4jVersion = "1.7.7"
    Seq(
    "org.scalatra"              %%  "scalatra"                % scalatraVersion,
    "org.scalatra"              %%  "scalatra-json"           % scalatraVersion,
    "org.scalatra"              %%  "scalatra-scalatest"      % scalatraVersion % "test",
    "org.eclipse.jetty"         %   "jetty-server"            % jettyVersion,
    "org.eclipse.jetty"         %   "jetty-webapp"            % jettyVersion,
    "org.json4s"                %%  "json4s-jackson"          % "3.6.12",
    "org.slf4j"                 %   "slf4j-api"               % slf4jVersion,
    "org.slf4j"                 %   "slf4j-simple"            % slf4jVersion,
    "org.scalatest"             %   "scalatest_2.12"          % "3.0.8" % "test",
    "org.scala-lang.modules"    %%  "scala-xml"               % "1.2.0",
    "org.scala-lang.modules"    %%  "scala-parser-combinators" % "1.1.2"
    )
}

mainClass in assembly := Some("YorubaRestService")
// unmanagedResourceDirectories in Compile += baseDirectory.value / "src/main/webapp"
