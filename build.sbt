ThisBuild / organization := "io.higherkindness"
ThisBuild / githubOrganization := "47degrees"
ThisBuild / crossScalaVersions := Seq("2.12.11", "2.13.1")

lazy val checkScalafmt = "+scalafmtCheckAll; +scalafmtSbtCheck;"
lazy val checkDocs     = "+docs/mdoc;"
lazy val checkTests    = "+coverage; +test; +coverageReport; +coverageAggregate;"

addCommandAlias("ci-test", s"$checkScalafmt $checkDocs $checkTests")
addCommandAlias("ci-docs", "project-docs/mdoc; docs/mdoc; headerCreateAll")
addCommandAlias("ci-microsite", "docs/publishMicrosite")

lazy val skeuomorph = project
  .in(file("."))
  .settings(commonSettings)
  .settings(moduleName := "skeuomorph")

lazy val docs = project
  .in(file("docs"))
  .dependsOn(skeuomorph)
  .settings(moduleName := "skeuomorph-docs")
  .settings(commonSettings)
  .settings(skip in publish := true)
  .settings(mdocSettings)
  .settings(
    micrositeName := "Skeuomorph",
    micrositeDescription := "Skeuomorph is a library for transforming different schemas in Scala",
    micrositeBaseUrl := "/skeuomorph",
    micrositeHighlightTheme := "tomorrow",
    micrositeCompilingDocsTool := WithMdoc,
    micrositeDocumentationUrl := "docs",
    includeFilter in Jekyll := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.md" | "*.svg",
    micrositeGithubToken := Option(System.getenv().get("GITHUB_TOKEN")),
    micrositePushSiteWith := GitHub4s,
    micrositeFavicons := Seq(microsites.MicrositeFavicon("favicon.png", "32x32")),
    micrositePalette := Map(
      "brand-primary"   -> "#4A00D8",
      "brand-secondary" -> "#FC00CD",
      "white-color"     -> "#FFF"
    ),
    micrositeExtraMdFiles := Map(
      file("CHANGELOG.md") -> microsites.ExtraMdFileConfig(
        "changelog.md",
        "docs",
        Map("title" -> "changelog", "permalink" -> "docs/changelog/")
      )
    )
  )
  .enablePlugins(MicrositesPlugin)

lazy val `project-docs` = (project in file(".docs"))
  .aggregate(skeuomorph)
  .dependsOn(skeuomorph)
  .settings(moduleName := "skeuomorph-project-docs")
  .settings(mdocIn := file(".docs"))
  .settings(mdocOut := file("."))
  .settings(skip in publish := true)
  .enablePlugins(MdocPlugin)

// General Settings
lazy val commonSettings = Seq(
  scalacOptions ~= (_ filterNot Set("-Xfuture", "-Xfatal-warnings").contains),
  libraryDependencies ++= Seq(
    "org.typelevel"          %% "cats-core"               % "2.1.1",
    "org.typelevel"          %% "cats-effect"             % "2.1.3",
    "io.higherkindness"      %% "droste-core"             % "0.8.0",
    "io.higherkindness"      %% "droste-macros"           % "0.8.0",
    "org.apache.avro"         % "avro"                    % "1.9.2",
    "com.github.os72"         % "protoc-jar"              % "3.11.4",
    "com.google.protobuf"     % "protobuf-java"           % "3.12.0",
    "io.circe"               %% "circe-core"              % "0.13.0",
    "io.circe"               %% "circe-parser"            % "0.13.0",
    "io.circe"               %% "circe-yaml"              % "0.13.1",
    "org.scalameta"          %% "scalameta"               % "4.3.10",
    "org.scala-lang.modules" %% "scala-collection-compat" % "2.1.6",
    "org.apache.avro"         % "avro-compiler"           % "1.9.2"  % Test,
    "org.typelevel"          %% "cats-laws"               % "2.1.1"  % Test,
    "io.circe"               %% "circe-testing"           % "0.13.0" % Test,
    "org.typelevel"          %% "discipline-specs2"       % "1.1.0"  % Test,
    "org.specs2"             %% "specs2-core"             % "4.9.4"  % Test,
    "org.specs2"             %% "specs2-scalacheck"       % "4.9.4"  % Test,
    "org.scalacheck"         %% "scalacheck"              % "1.14.3" % Test,
    "io.chrisdavenport"      %% "cats-scalacheck"         % "0.2.0"  % Test
  ),
  coverageFailOnMinimum := false
) ++ compilerPlugins ++ macroSettings

lazy val mdocSettings = Seq(
  scalacOptions ~= filterConsoleScalacOptions,
  scalacOptions ~= (_ filterNot Set("-Xfatal-warnings", "-Ywarn-unused-import", "-Xlint").contains)
)

lazy val compilerPlugins = Seq(
  libraryDependencies ++= Seq(
    compilerPlugin("org.typelevel" % "kind-projector"     % "0.10.3" cross CrossVersion.binary),
    compilerPlugin("com.olegpy"   %% "better-monadic-for" % "0.3.1")
  )
)

def isOlderScalaVersion(sv: String): Boolean =
  CrossVersion.partialVersion(sv) match {
    case Some((2, minor)) if minor < 13 => true
    case _                              => false
  }

lazy val macroSettings: Seq[Setting[_]] = {

  def paradiseDependency(sv: String): Seq[ModuleID] =
    if (isOlderScalaVersion(sv)) {
      Seq(
        compilerPlugin(
          ("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.patch)
        )
      )
    } else Seq.empty

  def macroAnnotationScalacOption(sv: String): Seq[String] =
    if (isOlderScalaVersion(sv)) Seq.empty
    else Seq("-Ymacro-annotations")

  Seq(
    libraryDependencies ++= Seq(
      scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided
    ) ++ paradiseDependency(scalaVersion.value),
    scalacOptions ++= macroAnnotationScalacOption(scalaVersion.value)
  )
}
