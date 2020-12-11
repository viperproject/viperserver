// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

// Import general settings from Silver, Silicon and Carbon
lazy val silver = project in file("silver")
lazy val silicon = project in file("silicon")
lazy val carbon = project in file("carbon")

// Viper Server specific project settings
lazy val server = (project in file("."))
    .dependsOn(silver % "compile->compile;test->test")
    .dependsOn(silicon % "compile->compile;test->test")
    .dependsOn(carbon % "compile->compile;test->test")
    .enablePlugins(JavaAppPackaging)
    .settings(
        // General settings
        name := "ViperServer",
        organization := "viper",
        version := "1.1-SNAPSHOT",

        // Fork test to a different JVM than SBT's, avoiding SBT's classpath interfering with
        // classpath used by Scala's reflection.
        Test / fork := true,

        // Compilation settings
        libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.6.10",
        libraryDependencies += "com.typesafe.akka" %% "akka-http-spray-json" % "10.2.1",
        libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.6.10",
        libraryDependencies += "com.typesafe.akka" %% "akka-stream-testkit" % "2.6.10" % Test,
        libraryDependencies += "com.typesafe.akka" %% "akka-http-testkit" % "10.2.1" % Test,

        // Run settings
        run / javaOptions += "-Xss128m",

        // Test settings.
        Test / parallelExecution := false,

        // Assembly settings
        assembly / assemblyJarName := "viper.jar",                      // JAR filename
        assembly / mainClass := Some("viper.server.ViperServerRunner"), // Define JAR's entry point
        assembly / test := {},                                          // Prevent testing before packaging
        assembly / assemblyMergeStrategy := {
            case "logback.xml" => MergeStrategy.first
            case PathList("viper", "silicon", "BuildInfo$.class") => MergeStrategy.first
            case x =>
                val fallbackStrategy = (assembly / assemblyMergeStrategy).value
                fallbackStrategy(x)
        }
    )
