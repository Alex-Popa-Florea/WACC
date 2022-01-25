Global / onChangedBuildSource := ReloadOnSourceChanges
lazy val root = (project in file("."))
    .settings(
        name := "WACC_03",
        organization := "uk.ac.imperial.doc",
        scalaVersion := "2.13.7",

        
        libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % Test,
        libraryDependencies += "com.github.j-mie6" %% "parsley" % "3.3.2"
    )

