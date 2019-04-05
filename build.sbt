// Import general settings from Silver, Silicon and Carbon
lazy val silver = project in file("silver")
lazy val silicon = project in file("silicon")
lazy val carbon = project in file("carbon")

lazy val common = (project in file("common"))

// Viper Server specific project settings
lazy val server = (project in file("."))
    .dependsOn(silver % "compile->compile;test->test")
    .dependsOn(silicon % "compile->compile;test->test")
    .dependsOn(carbon % "compile->compile;test->test")
    .dependsOn(common)
    .aggregate(common)
    .enablePlugins(JavaAppPackaging)
    .settings(
        // General settings
        name := "ViperServer",
        organization := "viper",
        version := "1.1-SNAPSHOT",

        // Compilation settings
        libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.17",
        libraryDependencies += "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.5",
        libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.5.17",
        libraryDependencies += "com.typesafe.akka" %% "akka-http-testkit" % "10.1.5" % Test,

        // Assembly settings
        assembly / assemblyJarName := "viper.jar",                      // JAR filename
        assembly / mainClass := Some("viper.server.ViperServerRunner"), // Define JAR's entry point
        assembly / test := {},                                          // Prevent testing before packaging
        assembly / assemblyMergeStrategy := {
            case "logback.xml" => MergeStrategy.first
            case "viper/silicon/BuildInfo$.class" => MergeStrategy.first
            case x =>
                val fallbackStrategy = (assembly / assemblyMergeStrategy).value
                fallbackStrategy(x)
        }
    )
