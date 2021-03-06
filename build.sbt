import _root_.bloop.integrations.sbt.BloopDefaults
import build.BuildImplementation.BuildDefaults

// Tell bloop to aggregate source deps (benchmark) config files in the same bloop config dir
bloopAggregateSourceDependencies in Global := true

bloopExportJarClassifiers in ThisBuild := Some(Set("sources"))

/***************************************************************************************************/
/*                      This is the build definition of the source deps                            */
/***************************************************************************************************/
val benchmarkBridge = project
  .in(file(".benchmark-bridge-compilation"))
  .aggregate(BenchmarkBridgeCompilation)
  .disablePlugins(ScriptedPlugin)
  .settings(
    releaseEarly := { () },
    skip in publish := true,
    bloopGenerate in Compile := None,
    bloopGenerate in Test := None
  )

lazy val bloopShared = (project in file("shared"))
  .settings(
    libraryDependencies ++= Seq(
      Dependencies.bsp,
      Dependencies.zinc,
      Dependencies.xxHashLibrary,
      Dependencies.configDirectories,
      Dependencies.sbtTestInterface,
      Dependencies.sbtTestAgent
    )
  )

/***************************************************************************************************/
/*                            This is the build definition of the wrapper                          */
/***************************************************************************************************/
import build.Dependencies
import build.Dependencies.{Scala210Version, Scala211Version, Scala212Version}

lazy val backend = project
  .enablePlugins(BuildInfoPlugin)
  .disablePlugins(ScriptedPlugin)
  .settings(testSettings ++ testSuiteSettings)
  .dependsOn(bloopShared)
  .settings(
    name := "bloop-backend",
    buildInfoPackage := "bloop.internal.build",
    buildInfoKeys := BloopBackendInfoKeys,
    buildInfoObject := "BloopScalaInfo",
    libraryDependencies ++= List(
      Dependencies.nailgun,
      Dependencies.scalazCore,
      Dependencies.scalazConcurrent,
      Dependencies.coursier,
      Dependencies.coursierCache,
      Dependencies.libraryManagement,
      Dependencies.sourcecode,
      Dependencies.monix,
      Dependencies.directoryWatcher,
      Dependencies.zt,
      Dependencies.brave,
      Dependencies.zipkinSender,
      Dependencies.pprint,
      Dependencies.difflib,
      Dependencies.asm,
      Dependencies.asmUtil
    )
  )

lazy val jsonConfig210 = project
  .in(file("config"))
  .disablePlugins(ScriptedPlugin)
  .settings(testSettings)
  .settings(
    name := "bloop-config",
    target := (file("config") / "target" / "json-config-2.10").getAbsoluteFile,
    scalaVersion := Scala210Version,
    publishM2Configuration := publishM2Configuration.value.withOverwrite(true),
    publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
    // We compile in both so that the maven integration can be tested locally
    publishLocal := publishLocal.dependsOn(publishM2).value,
    libraryDependencies ++= {
      List(
        Dependencies.circeParser,
        Dependencies.circeCore,
        Dependencies.circeGeneric,
        compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),
        Dependencies.scalacheck % Test
      )
    }
  )

lazy val jsonConfig211 = project
  .in(file("config"))
  .disablePlugins(ScriptedPlugin)
  .settings(testSettings)
  .settings(
    name := "bloop-config",
    target := (file("config") / "target" / "json-config-2.11").getAbsoluteFile,
    scalaVersion := Scala211Version,
    unmanagedSourceDirectories in Compile +=
      Keys.baseDirectory.value./("src")./("main")./("scala-2.11-12"),
    publishM2Configuration := publishM2Configuration.value.withOverwrite(true),
    publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
    // We compile in both so that the gradle integration can be tested locally
    publishLocal := publishLocal.dependsOn(publishM2).value,
    libraryDependencies ++= {
      List(
        Dependencies.circeParser,
        Dependencies.circeDerivation,
        Dependencies.scalacheck % Test
      )
    }
  )

// Needs to be called `jsonConfig` because of naming conflict with sbt universe...
lazy val jsonConfig212 = project
  .in(file("config"))
  .disablePlugins(ScriptedPlugin)
  .settings(testSettings)
  .settings(
    name := "bloop-config",
    unmanagedSourceDirectories in Compile +=
      Keys.baseDirectory.value./("src")./("main")./("scala-2.11-12"),
    target := (file("config") / "target" / "json-config-2.12").getAbsoluteFile,
    scalaVersion := Keys.scalaVersion.in(backend).value,
    publishM2Configuration := publishM2Configuration.value.withOverwrite(true),
    publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
    publishLocal := publishLocal.dependsOn(publishM2).value,
    libraryDependencies ++= {
      List(
        Dependencies.circeParser,
        Dependencies.circeDerivation,
        Dependencies.scalacheck % Test
      )
    }
  )

lazy val sockets: Project = project
  .settings(
    crossPaths := false,
    autoScalaLibrary := false,
    description := "IPC: Unix Domain Socket and Windows Named Pipes for Java",
    libraryDependencies ++= Seq(Dependencies.jna, Dependencies.jnaPlatform)
  )

import build.BuildImplementation.jvmOptions
// For the moment, the dependency is fixed
lazy val frontend: Project = project
  .dependsOn(
    sockets,
    bloopShared,
    backend,
    backend % "test->test",
    jsonConfig212,
    buildpressConfig % "it->compile"
  )
  .disablePlugins(ScriptedPlugin)
  .enablePlugins(BuildInfoPlugin)
  .configs(IntegrationTest)
  .settings(assemblySettings, releaseSettings)
  .settings(
    testSettings,
    testSuiteSettings,
    Defaults.itSettings,
    BuildDefaults.frontendTestBuildSettings,
    // Can be removed when metals upgrades to 1.3.0
    inConfig(IntegrationTest)(BloopDefaults.configSettings),
    inConfig(Compile)(
      build.BuildKeys.lazyFullClasspath := {
        val ownProductDirectories = Keys.productDirectories.value
        val dependencyClasspath = build.BuildImplementation.lazyDependencyClasspath.value
        ownProductDirectories ++ dependencyClasspath
      }
    )
  )
  .settings(
    name := "bloop-frontend",
    bloopName := "bloop",
    mainClass in Compile in run := Some("bloop.Cli"),
    bloopMainClass in Compile in run := Some("bloop.Cli"),
    buildInfoPackage := "bloop.internal.build",
    buildInfoKeys := bloopInfoKeys(nativeBridge, jsBridge06, jsBridge10),
    javaOptions in run ++= jvmOptions,
    javaOptions in Test ++= jvmOptions,
    javaOptions in IntegrationTest ++= jvmOptions,
    libraryDependencies += Dependencies.graphviz % Test,
    fork in run := true,
    fork in Test := true,
    fork in run in IntegrationTest := true,
    parallelExecution in test := false,
    libraryDependencies ++= List(
      Dependencies.scalazCore,
      Dependencies.monix,
      Dependencies.caseApp,
      Dependencies.nuprocess
    ),
    dependencyOverrides += Dependencies.shapeless
  )

lazy val launcher: Project = project
  .disablePlugins(ScriptedPlugin)
  .dependsOn(sockets, frontend % "test->test")
  .settings(testSuiteSettings)
  .settings(
    name := "bloop-launcher",
    fork in Test := true,
    parallelExecution in Test := false,
    libraryDependencies ++= List(
      Dependencies.coursier,
      Dependencies.coursierCache,
      Dependencies.nuprocess
    )
  )

lazy val benchmarks = project
  .dependsOn(frontend % "compile->it", BenchmarkBridgeCompilation % "compile->compile")
  .disablePlugins(ScriptedPlugin)
  .enablePlugins(BuildInfoPlugin, JmhPlugin)
  .settings(benchmarksSettings(frontend))
  .settings(
    skip in publish := true
  )

val integrations = file("integrations")

lazy val sbtBloop10 = project
  .enablePlugins(ScriptedPlugin)
  .in(integrations / "sbt-bloop")
  .settings(BuildDefaults.scriptedSettings)
  .settings(sbtPluginSettings("1.1.4", jsonConfig212))
  .dependsOn(jsonConfig212)

// Let's remove scripted for 0.13, we only test 1.0
lazy val sbtBloop013 = project
  .disablePlugins(ScriptedPlugin)
  .in(integrations / "sbt-bloop")
  .settings(scalaVersion := Scala210Version)
  .settings(sbtPluginSettings("0.13.18", jsonConfig210))
  .dependsOn(jsonConfig210)

lazy val mavenBloop = project
  .in(integrations / "maven-bloop")
  .disablePlugins(ScriptedPlugin)
  .dependsOn(jsonConfig210)
  .settings(name := "maven-bloop", scalaVersion := Scala210Version)
  .settings(BuildDefaults.mavenPluginBuildSettings)

lazy val gradleBloop211 = project
  .in(file("integrations") / "gradle-bloop")
  .enablePlugins(BuildInfoPlugin)
  .disablePlugins(ScriptedPlugin)
  .dependsOn(jsonConfig211)
  .settings(name := "gradle-bloop")
  .settings(BuildDefaults.gradlePluginBuildSettings)
  .settings(BuildInfoPlugin.buildInfoScopedSettings(Test))
  .settings(scalaVersion := Keys.scalaVersion.in(jsonConfig211).value)
  .settings(
    target := (file("integrations") / "gradle-bloop" / "target" / "gradle-bloop-2.11").getAbsoluteFile
  )
  .settings(
    sourceDirectories in Test := Nil,
    publishLocal := publishLocal.dependsOn(publishLocal.in(jsonConfig211)).value,
    bloopGenerate in Test := None,
    test in Test := Def.task {
      Keys.streams.value.log.error("Run 'gradleBloopTests/test' instead to test the gradle plugin.")
    }
  )

lazy val gradleBloop212 = project
  .in(file("integrations") / "gradle-bloop")
  .enablePlugins(BuildInfoPlugin)
  .disablePlugins(ScriptedPlugin)
  .dependsOn(jsonConfig212, frontend % "test->test")
  .settings(BuildDefaults.gradlePluginBuildSettings, testSettings)
  .settings(BuildInfoPlugin.buildInfoScopedSettings(Test))
  .settings(scalaVersion := Keys.scalaVersion.in(jsonConfig212).value)
  .settings(
    target := (file("integrations") / "gradle-bloop" / "target" / "gradle-bloop-2.12").getAbsoluteFile
  )
  .settings(
    publishLocal := publishLocal.dependsOn(publishLocal.in(jsonConfig212)).value
  )

lazy val millBloop = project
  .in(integrations / "mill-bloop")
  .disablePlugins(ScriptedPlugin)
  .dependsOn(jsonConfig212)
  .settings(name := "mill-bloop")
  .settings(BuildDefaults.millModuleBuildSettings)

lazy val buildpressConfig = (project in file("buildpress-config"))
  .settings(
    scalaVersion := Scala212Version,
    libraryDependencies ++= List(
      Dependencies.circeParser,
      Dependencies.circeCore,
      Dependencies.circeGeneric
    ),
    addCompilerPlugin(
      "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
    )
  )

lazy val buildpress = project
  .dependsOn(launcher, bloopShared, buildpressConfig)
  .settings(buildpressSettings)
  .settings(
    scalaVersion := Scala212Version,
    libraryDependencies ++= List(
      Dependencies.caseApp,
      Dependencies.nuprocess
    )
  )

val docs = project
  .in(file("docs-gen"))
  .dependsOn(frontend)
  .enablePlugins(MdocPlugin, DocusaurusPlugin)
  .settings(
    name := "bloop-docs",
    moduleName := "bloop-docs",
    skip in publish := true,
    scalaVersion := "2.12.6",
    mdoc := run.in(Compile).evaluated,
    mainClass.in(Compile) := Some("bloop.Docs"),
    resources.in(Compile) ++= {
      List(baseDirectory.in(ThisBuild).value / "docs")
    }
  )

lazy val jsBridge06 = project
  .dependsOn(frontend % Provided, frontend % "test->test")
  .in(file("bridges") / "scalajs-0.6")
  .disablePlugins(ScriptedPlugin)
  .settings(testSettings)
  .settings(
    name := "bloop-js-bridge-0.6",
    libraryDependencies ++= List(
      Dependencies.scalaJsTools06,
      Dependencies.scalaJsSbtTestAdapter06,
      Dependencies.scalaJsEnvs06
    )
  )

lazy val jsBridge10 = project
  .dependsOn(frontend % Provided, frontend % "test->test")
  .in(file("bridges") / "scalajs-1.0")
  .disablePlugins(ScriptedPlugin)
  .settings(testSettings)
  .settings(
    name := "bloop-js-bridge-1.0",
    libraryDependencies ++= List(
      Dependencies.scalaJsLinker10,
      Dependencies.scalaJsLogging10,
      Dependencies.scalaJsIO10,
      Dependencies.scalaJsEnvs10,
      Dependencies.scalaJsEnvNode10,
      Dependencies.scalaJsEnvJsdomNode10,
      Dependencies.scalaJsSbtTestAdapter10
    )
  )

lazy val nativeBridge = project
  .dependsOn(frontend % Provided, frontend % "test->test")
  .in(file("bridges") / "scala-native")
  .disablePlugins(ScriptedPlugin)
  .settings(testSettings)
  .settings(
    name := "bloop-native-bridge",
    libraryDependencies += Dependencies.scalaNativeTools,
    javaOptions in Test ++= jvmOptions,
    fork in Test := true
  )

/* This project has the only purpose of forcing the resolution of some artifacts that fail spuriously to be fetched.  */
lazy val twitterIntegrationProjects = project
  .disablePlugins(CoursierPlugin, BloopPlugin)
  .in(file("target") / "twitter-integration-projects")
  .settings(
    resolvers += MavenRepository("twitter-resolver", "https://maven.twttr.com"),
    libraryDependencies += "com.hadoop.gplcompression" % "hadoop-lzo" % "0.4.19",
    libraryDependencies += "com.twitter.common" % "util" % "0.0.118",
    libraryDependencies += "com.twitter.common" % "quantity" % "0.0.96",
    libraryDependencies += "com.twitter" % "scrooge-serializer_2.11" % "4.12.0"
  )

val allProjects = Seq(
  bloopShared,
  backend,
  benchmarks,
  frontend,
  jsonConfig210,
  jsonConfig211,
  jsonConfig212,
  sbtBloop013,
  sbtBloop10,
  mavenBloop,
  gradleBloop211,
  gradleBloop212,
  millBloop,
  nativeBridge,
  jsBridge06,
  jsBridge10,
  launcher,
  sockets
)

val allProjectReferences = allProjects.map(p => LocalProject(p.id))
val bloop = project
  .in(file("."))
  .disablePlugins(ScriptedPlugin)
  .aggregate(allProjectReferences: _*)
  .settings(
    releaseEarly := { () },
    skip in publish := true,
    crossSbtVersions := Seq("1.2.8", "0.13.18"),
    commands += BuildDefaults.exportProjectsInTestResourcesCmd,
    buildIntegrationsBase := (Keys.baseDirectory in ThisBuild).value / "build-integrations",
    twitterDodo := buildIntegrationsBase.value./("build-twitter"),
    exportCommunityBuild := {
      build.BuildImplementation
        .exportCommunityBuild(
          buildpress,
          jsonConfig210,
          jsonConfig212,
          sbtBloop013,
          sbtBloop10
        )
        .value
    }
  )

/**************************************************************************************************/
/*                      This is the corner for all the command definitions                        */
/**************************************************************************************************/
val publishLocalCmd = Keys.publishLocal.key.label

// Runs the scripted tests to setup integration tests
// ! This is used by the benchmarks too !
addCommandAlias(
  "install",
  Seq(
    s"${bloopShared.id}/$publishLocalCmd",
    s"${jsonConfig210.id}/$publishLocalCmd",
    s"${jsonConfig211.id}/$publishLocalCmd",
    s"${jsonConfig212.id}/$publishLocalCmd",
    s"${sbtBloop013.id}/$publishLocalCmd",
    s"${sbtBloop10.id}/$publishLocalCmd",
    s"${mavenBloop.id}/$publishLocalCmd",
    s"${gradleBloop211.id}/$publishLocalCmd",
    s"${gradleBloop212.id}/$publishLocalCmd",
    s"${backend.id}/$publishLocalCmd",
    s"${frontend.id}/$publishLocalCmd",
    s"${nativeBridge.id}/$publishLocalCmd",
    s"${millBloop.id}/$publishLocalCmd",
    s"${jsBridge06.id}/$publishLocalCmd",
    s"${jsBridge10.id}/$publishLocalCmd",
    s"${sockets.id}/$publishLocalCmd",
    s"${launcher.id}/$publishLocalCmd",
    s"${buildpressConfig.id}/$publishLocalCmd",
    s"${buildpress.id}/$publishLocalCmd",
    // Force build info generators in frontend-test
    s"${frontend.id}/test:compile",
    "createLocalHomebrewFormula",
    "createLocalScoopFormula",
    "createLocalArchPackage",
    "generateInstallationWitness"
  ).mkString(";", ";", "")
)

addCommandAlias(
  "publishSbtBloop",
  Seq(
    s"${jsonConfig210.id}/$publishLocalCmd",
    s"${jsonConfig212.id}/$publishLocalCmd",
    s"${sbtBloop013.id}/$publishLocalCmd",
    s"${sbtBloop10.id}/$publishLocalCmd"
  ).mkString(";", ";", "")
)

val releaseEarlyCmd = releaseEarly.key.label

val allBloopReleases = List(
  s"${bloopShared.id}/$releaseEarlyCmd",
  s"${backend.id}/$releaseEarlyCmd",
  s"${frontend.id}/$releaseEarlyCmd",
  s"${jsonConfig210.id}/$releaseEarlyCmd",
  s"${jsonConfig211.id}/$releaseEarlyCmd",
  s"${jsonConfig212.id}/$releaseEarlyCmd",
  s"${sbtBloop013.id}/$releaseEarlyCmd",
  s"${sbtBloop10.id}/$releaseEarlyCmd",
  s"${mavenBloop.id}/$releaseEarlyCmd",
  s"${gradleBloop211.id}/$releaseEarlyCmd",
  s"${gradleBloop212.id}/$releaseEarlyCmd",
  s"${millBloop.id}/$releaseEarlyCmd",
  s"${nativeBridge.id}/$releaseEarlyCmd",
  s"${jsBridge06.id}/$releaseEarlyCmd",
  s"${jsBridge10.id}/$releaseEarlyCmd",
  s"${sockets.id}/$releaseEarlyCmd",
  s"${launcher.id}/$releaseEarlyCmd",
  s"${buildpressConfig.id}/$releaseEarlyCmd",
  s"${buildpress.id}/$releaseEarlyCmd"
)

val allReleaseActions = allBloopReleases ++ List("sonatypeReleaseAll")
addCommandAlias("releaseBloop", allReleaseActions.mkString(";", ";", ""))
