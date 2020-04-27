import microsites._

lazy val checkScalafmt = "+scalafmtCheck; +scalafmtSbtCheck;"
lazy val checkDocs     = "+docs/mdoc;"
lazy val checkTests    = "+coverage; +test; +coverageReport; +coverageAggregate;"

addCommandAlias("ci-test", s"$checkScalafmt $checkDocs $checkTests")
addCommandAlias("ci-docs", "project-docs/mdoc; docs/mdoc; headerCreateAll")
addCommandAlias("ci-microsite", "docs/publishMicrosite")

val V = new {
  val avro             = "1.9.2"
  val betterMonadicFor = "0.3.1"
  val cats             = "2.1.1"
  val catsEffect       = "2.1.3"
  val catsScalacheck   = "0.2.0"
  val circe            = "0.13.0"
  val circeYaml        = "0.13.0"
  val collectionCompat = "2.1.6"
  val disciplineSpecs2 = "1.1.0"
  val droste           = "0.8.0"
  val kindProjector    = "0.10.3"
  val macroParadise    = "2.1.1"
  val meta             = "4.3.0"
  val scala212         = "2.12.11"
  val scala213         = "2.13.1"
  val scalacheck       = "1.14.3"
  val specs2           = "4.9.4"
  val protoc           = "3.11.4"
  val protobuf         = "3.11.4"
}

lazy val skeuomorph = project
  .in(file("."))
  .settings(commonSettings)
  .settings(moduleName := "skeuomorph")

lazy val docs = project
  .in(file("docs"))
  .dependsOn(skeuomorph)
  .settings(moduleName := "skeuomorph-docs")
  .settings(commonSettings)
  .settings(noPublishSettings)
  .settings(mdocSettings)
  .settings(
    micrositeName := "Skeuomorph",
    micrositeDescription := "Skeuomorph is a library for transforming different schemas in Scala",
    micrositeBaseUrl := "/skeuomorph",
    micrositeGithubOwner := "higherkindness",
    micrositeGithubRepo := "skeuomorph",
    micrositeHighlightTheme := "tomorrow",
    micrositeCompilingDocsTool := WithMdoc,
    micrositeDocumentationUrl := "docs",
    includeFilter in Jekyll := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.md" | "*.svg",
    micrositeGithubToken := Option(System.getenv().get("GITHUB_TOKEN")),
    micrositePushSiteWith := GitHub4s,
    micrositeFavicons := Seq(MicrositeFavicon("favicon.png", "32x32")),
    micrositePalette := Map(
      "brand-primary"   -> "#4A00D8",
      "brand-secondary" -> "#FC00CD",
      "white-color"     -> "#FFF"
    ),
    micrositeExtraMdFiles := Map(
      file("CHANGELOG.md") -> ExtraMdFileConfig(
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
  .settings(noPublishSettings)
  .enablePlugins(MdocPlugin)

// General Settings
lazy val commonSettings = Seq(
  organization := "io.higherkindness",
  organizationName := "47 Degrees",
  organizationHomepage := Some(url("http://47deg.com")),
  crossScalaVersions := Seq(V.scala212, V.scala213),
  startYear := Some(2018),
  crossScalaVersions := Seq(V.scala212, V.scala213),
  scalacOptions ~= (_ filterNot Set("-Xfuture", "-Xfatal-warnings").contains),
  libraryDependencies ++= Seq(
    "org.typelevel"          %% "cats-core"               % V.cats,
    "org.typelevel"          %% "cats-effect"             % V.catsEffect,
    "io.higherkindness"      %% "droste-core"             % V.droste,
    "io.higherkindness"      %% "droste-macros"           % V.droste,
    "org.apache.avro"        % "avro"                     % V.avro,
    "com.github.os72"        % "protoc-jar"               % V.protoc,
    "com.google.protobuf"    % "protobuf-java"            % V.protobuf,
    "io.circe"               %% "circe-core"              % V.circe,
    "io.circe"               %% "circe-parser"            % V.circe,
    "io.circe"               %% "circe-yaml"              % V.circeYaml,
    "org.scalameta"          %% "scalameta"               % V.meta,
    "org.scala-lang.modules" %% "scala-collection-compat" % V.collectionCompat,
    "org.apache.avro"        % "avro-compiler"            % V.avro % Test,
    "org.typelevel"          %% "cats-laws"               % V.cats % Test,
    "io.circe"               %% "circe-testing"           % V.circe % Test,
    "org.typelevel"          %% "discipline-specs2"       % V.disciplineSpecs2 % Test,
    "org.specs2"             %% "specs2-core"             % V.specs2 % Test,
    "org.specs2"             %% "specs2-scalacheck"       % V.specs2 % Test,
    "org.scalacheck"         %% "scalacheck"              % V.scalacheck % Test,
    "io.chrisdavenport"      %% "cats-scalacheck"         % V.catsScalacheck % Test
  ),
  coverageFailOnMinimum := false
) ++ compilerPlugins ++ macroSettings

lazy val mdocSettings = Seq(
  scalacOptions ~= filterConsoleScalacOptions,
  scalacOptions ~= (_ filterNot Set("-Xfatal-warnings", "-Ywarn-unused-import", "-Xlint").contains)
)

lazy val compilerPlugins = Seq(
  libraryDependencies ++= Seq(
    compilerPlugin("org.typelevel" % "kind-projector"      % V.kindProjector cross CrossVersion.binary),
    compilerPlugin("com.olegpy"    %% "better-monadic-for" % V.betterMonadicFor)
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
          ("org.scalamacros" % "paradise" % V.macroParadise).cross(CrossVersion.patch)
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

lazy val noPublishSettings = Seq(
  publish := ((): Unit),
  publishLocal := ((): Unit),
  publishArtifact := false,
  publishMavenStyle := false // suppress warnings about intransitive deps (not published anyway)
)
