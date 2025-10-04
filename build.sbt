name := "yorudi"

scalaVersion := "2.11.8"

fork := true

version := "1.0"

libraryDependencies ++= { 
    val scalatraVersion = "2.3.0"
    val jettyVersion = "9.3.0.M1"
    val slf4jVersion = "1.7.7"
    Seq(
    "org.scalatra"              %%  "scalatra"                % scalatraVersion,
    "org.scalatra"              %%  "scalatra-json"           % scalatraVersion,
    "org.scalatra"              %% "scalatra-scalatest"       % scalatraVersion % "test",
    "org.eclipse.jetty"         %   "jetty-server"            % jettyVersion,
    "org.eclipse.jetty"         %   "jetty-webapp"            % jettyVersion,
    "org.json4s"                %%  "json4s-jackson"          % "3.2.9",
    "org.slf4j"                 %   "slf4j-api"               % slf4jVersion,
    "org.slf4j"                 %   "slf4j-simple"            % slf4jVersion,
    "org.scalatest"             % "scalatest_2.11"            % "2.2.5" % "test",
    "org.scala-lang.modules"    % "scala-xml_2.11"            % "1.0.5",
    "org.scala-lang.modules"    %% "scala-parser-combinators" % "1.0.3"
    )
}

mainClass in assembly := Some("YorubaRestService")

// Include text dictionaries in JAR
resourceDirectory in Compile := baseDirectory.value / "dictionaries"

