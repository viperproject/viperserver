import scala.sys.process.Process
import scala.util.Try
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

lazy val silicon = project in file("silicon")
lazy val carbon = project in file("carbon")

// Viper Server specific project settings
lazy val server = (project in file("."))
    .dependsOn(silicon % "compile->compile;test->test")
    .dependsOn(carbon % "compile->compile;test->test")
    .enablePlugins(JavaAppPackaging)
    .settings(
        // General settings
        name := "ViperServer",
        organization := "viper",
        version := "2.0.0", // has to be a proper semver

        // Fork test to a different JVM than SBT's, avoiding SBT's classpath interfering with
        // classpath used by Scala's reflection.
        fork := true,

        libraryDependencies += "net.liftweb" %% "lift-json" % "3.5.0",
        libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.6.10",
        libraryDependencies += "com.typesafe.akka" %% "akka-http-spray-json" % "10.2.1",
        libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.6.10",
        libraryDependencies += "com.typesafe.akka" %% "akka-stream-testkit" % "2.6.10" % Test,
        libraryDependencies += "com.typesafe.akka" %% "akka-http-testkit" % "10.2.1" % Test,
        libraryDependencies += "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.15.0", // Java implementation of language server protocol

        silicon / excludeFilter := "logback.xml", /* Ignore Silicon's Logback configuration */
        carbon / excludeFilter := "logback.xml", /* Ignore Carbon's Logback configuration */

        // Run settings
        run / javaOptions += "-Xss128m",

        // Test settings
        Test / parallelExecution := false,

        // Assembly settings
        assembly / assemblyJarName := "viperserver.jar",                // JAR filename
        assembly / mainClass := Some("viper.server.ViperServerRunner"), // Define JAR's entry point
        assembly / test := {},                                          // Prevent testing before packaging
        assembly / assemblyMergeStrategy := {
            case LogbackConfigurationFilePattern() => MergeStrategy.first
            case PathList("viper", "silicon", "BuildInfo$.class") => MergeStrategy.first
            case x =>
                val fallbackStrategy = (assembly / assemblyMergeStrategy).value
                fallbackStrategy(x)
        },

        // Test settings
        // [2020-10-12 MS]
        //   When assembling a fat test JAR (test:assembly), the files under
        //   src/test don't end up in the JAR if the next line is missing.
        //   I'm not sure why that is, or why exactly the next line helps.
        //   To be investigated.
        // [2021-03-22 LA]
        //  If the following line is missing, viperserver-test.jar will not even
        //  be created.
        inConfig(Test)(baseAssemblySettings),
        Test / assembly / assemblyJarName := "viperserver-test.jar",
        Test / assembly / test := {},
        Test / assembly / assemblyMergeStrategy := {
          case LogbackConfigurationFilePattern() => MergeStrategy.discard
          case n if n.startsWith("LICENSE.txt") => MergeStrategy.discard
          case x =>
            val fallbackStrategy = (assembly / assemblyMergeStrategy).value
            fallbackStrategy(x)
        }
    )
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoKeys := Seq[BuildInfoKey](
      "projectName" -> name.value,
      "projectVersion" -> version.value,
      scalaVersion,
      sbtVersion,
      "gitRevision" -> gitInfo.value._1,
      "gitBranch" -> gitInfo.value._2,
      // combine version with branch name (if not 'master') and the git revision:
      "projectVersionExtended" -> s"${version.value} (${gitInfo.value._1}${if (gitInfo.value._2 == "master") "" else s"@${gitInfo.value._2}"})"
    ),
    buildInfoPackage := "viper.viperserver"
  )

// Pair of git revision and branch information
lazy val gitInfo: Def.Initialize[(String, String)] = Def.setting {
  (Try(Process("git rev-parse --short HEAD").!!.trim).getOrElse("<revision>"),
    Try(Process("git rev-parse --abbrev-ref HEAD").!!.trim).getOrElse("<branch>"))
}

lazy val LogbackConfigurationFilePattern = """logback.*?\.xml""".r
