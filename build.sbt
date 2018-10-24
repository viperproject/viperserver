// Import general settings from Silver, Silicon and Carbon
lazy val silver = project in file("silver")
lazy val silicon = project in file("silicon")
lazy val carbon = project in file("carbon")

lazy val common = (project in file("common"))
//?    .dependsOn(silver)

// Viper Server specific project settings
lazy val server = (project in file("."))
    .dependsOn(silver % "compile->compile;test->test")
    .dependsOn(silicon % "compile->compile;test->test")
    .dependsOn(carbon % "compile->compile;test->test")
    .dependsOn(common)
    .aggregate(common)
    .settings(
        // General settings
        name := "ViperServer",
        organization := "viper",                                        //? Should those come from Silver?
        version := "1.1-SNAPSHOT",                                      //? Should come from Silver?

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
            case x =>
                val fallbackStrategy = (assembly / assemblyMergeStrategy).value
                fallbackStrategy(x)
        }
    )



//object ViperServerBuild extends Build {
//
//  /* Base settings */
//
//  lazy val baseSettings = (
//    hgIdSettings
//      ++ brandSettings
//      ++ Seq(
//      unmanagedResourceDirectories in Compile := Seq(baseDirectory.value / "src/main/resources"),
//      includeFilter in unmanagedResources := "jawr.properties",
//      resolvers += "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
//      traceLevel := 10,
//      maxErrors := 6))
//
//  /* Projects */
//
//  lazy val viper = {
//    var p = Project(
//      id = "viper",
//      settings = (
//        baseSettings
//          ++ Seq(
//          name := "ViperServer",
//          assemblyMergeStrategy in assembly := {
//            case PathList("META-INF", xs @ _*) => MergeStrategy.discard
//            case "reference.conf" => MergeStrategy.concat
//            case x => MergeStrategy.first
//          },
//          /* Skip tests before assembling fat jar. Assembling stops if tests fails. */
//          // scalacOptions ++= Seq("-Xelide-below", "1000"),
//          /* remove elidable method calls such as in SymbExLogger during compiling */
//          fork := true,
//          javaOptions in run ++= Seq("-Xss128M", "-Xmx1512M", "-Dfile.encoding=UTF-8"),
//          javaOptions in Test ++= Seq("-Xss128M", "-Xmx1512M"),
//          /* Options passed to JVMs forked by test-related Sbt command.
//           * See http://www.scala-sbt.org/0.12.4/docs/Detailed-Topics/Forking.html
//           * In contrast to what the documentation states, it seemed
//           * that neither were the options passed to Sbt's JVM forwarded
//           * to forked JVMs, nor did "javaOptions in (Test,run)"
//           * work for me (Malte, using Sbt 0.12.4).
//           * You can inspect the settings in effect using via
//           * "show javaOptions" on the Sbt console.
//           */
//
//          libraryDependencies ++= externalDep,
//          BrandKeys.dataPackage := "viper.server",
//          BrandKeys.dataObject := "brandingData",
//          BrandKeys.data += Val("buildDate", new java.text.SimpleDateFormat("yyyy/MM/dd HH:mm:ss").format(new java.util.Date)),
//          BrandKeys.data <+= scalaVersion(Val("scalaVersion", _)),
//          BrandKeys.data <+= sbtBinaryVersion(Val("sbtBinaryVersion", _)),
//          BrandKeys.data <+= sbtVersion(Val("sbtVersion", _)),
//          BrandKeys.data <+= name(Val("sbtProjectName", _)),
//          BrandKeys.data <+= version(Val("sbtProjectVersion", _)),
//          BrandKeys.data <++= HgIdKeys.projectId(idOrException => {
//            val id =
//              idOrException.fold(Predef.identity,
//                _ => de.oakgrove.SbtHgId.Id("<unknown>", "<unknown>", "<unknown>", "<unknown>"))
//
//            Seq(Val("hgid_version", id.version),
//              Val("hgid_id", id.id),
//              Val("hgid_branch", id.branch),
//              Val("hgid_tags", id.tags))
//          }),
//          sourceGenerators in Compile <+= BrandKeys.generateDataFile)
//          ++ addCommandAlias("tn", "test-only -- -n "))
//    ).dependsOn(common)
//
//    for (dep <- internalDep) {
//      p = p.dependsOn(dep)
//    }
//
//    p.aggregate(common)
//    p.enablePlugins(JavaAppPackaging)
//  }
//
//
//  lazy val common = Project(
//    id = "common",
//    base = file("common"),
//    settings = (
//      baseSettings
//        ++ Seq(name := "ViperServer-Common",
//        javacOptions ++= Seq("-source", "1.7", "-target", "1.7"),
//        libraryDependencies += dependencies.commonsIO)))
//
//  /* On the build-server, we cannot have all project in the same directory, and
//   * thus we use the publish-local mechanism for dependencies.
//   */
//
//  def isBuildServer = sys.env.contains("BUILD_TAG") /* Should only be defined on the build server */
//
//  def externalDep = (
//    Seq(dependencies.jgrapht, dependencies.commonsIO, dependencies.commonsPool, dependencies.scallop,
//      dependencies.actors, dependencies.akka_testing, dependencies.akka_http_testing, dependencies.akka_json)
//      ++ dependencies.logging
//      ++ (if (isBuildServer) Seq(
//
//  /* Dependencies */
//
//  object dependencies {
//    lazy val logging = Seq(
//      "org.slf4j" % "slf4j-api" % "1.7.12",
//      "ch.qos.logback" % "logback-classic" % "1.2.3")
//
//    lazy val commonsPool = "org.apache.commons" % "commons-pool2" % "2.4.2"
//
//
//    lazy val akka_testing = "com.typesafe.akka" %% "akka-testkit" % "2.4.17" % "test"
//  }
//
//}
