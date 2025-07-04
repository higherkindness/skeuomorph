ThisBuild / organization       := "io.higherkindness"
ThisBuild / githubOrganization := "47degrees"
ThisBuild / scalaVersion       := "2.13.16"
ThisBuild / crossScalaVersions := Seq("2.12.20", "2.13.16")

addCommandAlias("ci-test", "scalafmtCheckAll; scalafmtSbtCheck; microsite/mdoc; +test")
addCommandAlias("ci-docs", "github; documentation/mdoc; headerCreateAll; microsite/publishMicrosite")
addCommandAlias("ci-publish", "github; ci-release")

lazy val skeuomorph = project
  .in(file("."))
  .settings(commonSettings)
  .settings(moduleName := "skeuomorph")

lazy val microsite = project
  .dependsOn(skeuomorph)
  .settings(commonSettings)
  .settings(publish / skip := true)
  .settings(mdocSettings)
  .settings(
    micrositeName             := "Skeuomorph",
    micrositeDescription      := "Skeuomorph is a library for transforming different schemas in Scala",
    micrositeBaseUrl          := "/skeuomorph",
    micrositeHighlightTheme   := "tomorrow",
    micrositeDocumentationUrl := "docs",
    Jekyll / includeFilter    := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.md" | "*.svg",
    micrositeGithubToken      := Option(System.getenv().get("GITHUB_TOKEN")),
    micrositePushSiteWith     := GitHub4s,
    micrositeFavicons         := Seq(microsites.MicrositeFavicon("favicon.png", "32x32")),
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

lazy val documentation = project
  .settings(mdocOut := file("."))
  .settings(publish / skip := true)
  .enablePlugins(MdocPlugin)

lazy val filterConsoleScalacOptions = { options: Seq[String] =>
  options.filterNot(
    Set(
      "-Werror",
      "-Wdead-code",
      "-Wunused:imports",
      "-Ywarn-unused",
      "-Ywarn-unused:imports",
      "-Ywarn-unused-import",
      "-Ywarn-dead-code",
      "-Xfatal-warnings"
    )
  )
}

// General Settings
lazy val commonSettings = Seq(
  scalacOptions ~= (_ filterNot Set("-Xfuture", "-Xfatal-warnings").contains),
  libraryDependencies ++= Seq(
    "org.typelevel"        %% "cats-core"         % "2.13.0",
    "org.typelevel"        %% "cats-effect"       % "3.6.1",
    "io.higherkindness"    %% "droste-core"       % "0.10.0",
    "io.higherkindness"    %% "droste-macros"     % "0.10.0",
    "org.apache.avro"       % "avro"              % "1.12.0",
    "com.github.os72"       % "protoc-jar"        % "3.11.4",
    "com.google.protobuf"   % "protobuf-java"     % "4.31.1",
    "io.circe"             %% "circe-core"        % "0.14.14",
    "io.circe"             %% "circe-parser"      % "0.14.14",
    "io.circe"             %% "circe-yaml"        % "1.15.0",
    "com.julianpeeters"    %% "avrohugger-core"   % "2.8.4"        % Test,
    "org.typelevel"        %% "cats-laws"         % "2.13.0"       % Test,
    "io.circe"             %% "circe-testing"     % "0.14.14"      % Test,
    "org.typelevel"        %% "discipline-specs2" % "1.5.0"        % Test,
    "org.specs2"           %% "specs2-core"       % "4.12.4-js-ec" % Test,
    "org.specs2"           %% "specs2-scalacheck" % "4.12.4-js-ec" % Test,
    "org.scalacheck"       %% "scalacheck"        % "1.18.1"       % Test,
    "io.chrisdavenport"    %% "cats-scalacheck"   % "0.3.2"        % Test,
    "org.scalatra.scalate" %% "scalate-core"      % "1.10.1"       % Test
  )
) ++ compilerPlugins ++ macroSettings ++ Seq(
  libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, _)) =>
      Seq(
        "org.scalameta" %% "scalameta" % "4.9.7"
      )
    case _ => Seq.empty
  })
)

lazy val mdocSettings = Seq(
  scalacOptions ~= filterConsoleScalacOptions,
  scalacOptions ~= (_ filterNot Set("-Xfatal-warnings", "-Ywarn-unused-import", "-Xlint").contains)
)

lazy val compilerPlugins = Seq(
  libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, _)) =>
      Seq(
        compilerPlugin("org.typelevel" % "kind-projector"     % "0.13.3" cross CrossVersion.full),
        compilerPlugin("com.olegpy"   %% "better-monadic-for" % "0.3.1")
      )
    case _ => Seq.empty
  })
)

lazy val macroSettings: Seq[Setting[_]] = Seq(
  libraryDependencies ++= Seq(
    scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided
  ),
  libraryDependencies ++= on(2, 12)(
    compilerPlugin("org.scalamacros" %% "paradise" % "2.1.1" cross CrossVersion.full)
  ).value,
  scalacOptions ++= on(2, 13)("-Ymacro-annotations").value
)

def on[A](major: Int, minor: Int)(a: A): Def.Initialize[Seq[A]] =
  Def.setting {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some(v) if v == (major, minor) => Seq(a)
      case _                              => Nil
    }
  }
