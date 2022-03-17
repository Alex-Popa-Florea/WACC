Global / onChangedBuildSource := ReloadOnSourceChanges

val projectName = "WACC"

lazy val sbtAssemblySettings = baseAssemblySettings ++ Seq(
  assembly / assemblyOutputPath := baseDirectory.value / s"$projectName.jar",
  assembly / assemblyMergeStrategy := {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case _                             => MergeStrategy.first
  }
)

lazy val sbtAssemblySettingsShell = baseAssemblySettings ++ Seq(
  assembly / assemblyOutputPath := baseDirectory.value / s"QWACC.jar",
  assembly / assemblyMergeStrategy := {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case _                             => MergeStrategy.first
  }
)

lazy val root = (project in file("."))
    .settings(
  		name := "WACC_03",
      organization := "uk.ac.imperial.doc",
      scalaVersion := "2.13.7",
      version := "0.1.0",

      sbtAssemblySettings,
      libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % Test,
      libraryDependencies += "com.github.j-mie6" %% "parsley" % "3.3.5"
    )


lazy val shell = (project in file("shell"))
    .settings(
      name := "QWACC",
      organization := "uk.ac.imperial.doc",
      scalaVersion := "2.13.7",
      version := "0.1.0",

      sbtAssemblySettingsShell,
      assembly / mainClass:= Some("qwacc.shell"),
      libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % Test
    )
  .dependsOn(root)
