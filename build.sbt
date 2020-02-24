import microsites._
import sbtorgpolicies.OrgPoliciesPlugin.autoImport._
import sbtorgpolicies.model._
import sbtorgpolicies.runnable.syntax._
import sbtorgpolicies.templates._
import sbtorgpolicies.templates.badges._

val V = new {
  val ammonite         = "2.0.4"
  val avro             = "1.9.2"
  val betterMonadicFor = "0.3.1"
  val cats             = "2.1.0"
  val catsEffect       = "2.1.1"
  val catsScalacheck   = "0.2.0"
  val circe            = "0.13.0"
  val circeYaml        = "0.12.0"
  val collectionCompat = "2.1.4"
  val disciplineSpecs2 = "1.0.0"
  val droste           = "0.8.0"
  val kindProjector    = "0.10.3"
  val macroParadise    = "2.1.1"
  val meta             = "4.3.0"
  val metaContrib      = "4.1.6"
  val scala212         = "2.12.10"
  val scala213         = "2.13.1"
  val scalacheck       = "1.14.3"
  val specs2           = "4.8.3"
  val protoc           = "3.11.1"
  val protobuf         = "3.11.4"
}

lazy val skeuomorph = project
  .in(file("."))
  .settings(commonSettings)
  .settings(moduleName := "skeuomorph")
  .settings(
    libraryDependencies ++= Seq(
      ("com.lihaoyi" % "ammonite" % V.ammonite % Test).cross(CrossVersion.full)
    ))
  .settings(
    sourceGenerators in Test += Def.task {
      val file = (sourceManaged in Test).value / "amm.scala"
      IO.write(file, """object amm extends App {
        ammonite.Main.main(args)
      }""")
      Seq(file)
    }.taskValue
  )

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
    micrositeGithubToken := getEnvVar(orgGithubTokenSetting.value),
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

pgpPassphrase := sys.env.get("PGP_PASSPHRASE").map(_.toCharArray)

// General Settings
lazy val commonSettings = Seq(
  name := "skeuomorph",
  orgGithubSetting := GitHubSettings(
    organization = "higherkindness",
    project = (name in LocalRootProject).value,
    organizationName = "47 Degrees",
    groupId = "io.higherkindness",
    organizationHomePage = url("https://www.47deg.com"),
    organizationEmail = "hello@47deg.com"
  ),
  startYear := Some(2018),
  scalaVersion := V.scala213,
  crossScalaVersions := Seq(V.scala212, V.scala213),
  scalacOptions ~= (_ filterNot Set("-Xfuture", "-Xfatal-warnings").contains),
  libraryDependencies ++= Seq(
    %%("cats-core", V.cats),
    "io.higherkindness"   %% "droste-core"   % V.droste,
    "io.higherkindness"   %% "droste-macros" % V.droste,
    "org.apache.avro"     % "avro"           % V.avro,
    "com.github.os72"     % "protoc-jar"     % V.protoc,
    "com.google.protobuf" % "protobuf-java"  % V.protobuf,
    "io.circe"            %% "circe-yaml"    % V.circeYaml,
    %%("cats-effect", V.catsEffect),
    %%("circe-core", V.circe),
    %%("circe-parser", V.circe),
    "org.scalameta"                   %% "scalameta" % V.meta,
    "org.scala-lang.modules"          %% "scala-collection-compat" % V.collectionCompat,
    %%("cats-laws", V.cats)           % Test,
    "io.circe"                        %% "circe-testing" % V.circe % Test,
    %%("scalacheck", V.scalacheck)    % Test,
    %%("specs2-core", V.specs2)       % Test,
    "org.typelevel"                   %% "discipline-specs2" % V.disciplineSpecs2 % Test,
    %%("specs2-scalacheck", V.specs2) % Test,
    "io.chrisdavenport"               %% "cats-scalacheck" % V.catsScalacheck % Test excludeAll (
      ExclusionRule(organization = "org.scalacheck"),
      "org.scalameta" %% "contrib" % V.metaContrib % Test
    )
  ),
  orgProjectName := "Skeuomorph",
  orgUpdateDocFilesSetting ++= List(
    baseDirectory.value / "docs" / "docs",
    baseDirectory.value / "docs" / "docs" / "docs"),
  orgMaintainersSetting := List(Dev("developer47deg", Some("47 Degrees (twitter: @47deg)"), Some("hello@47deg.com"))),
  orgBadgeListSetting := List(
    TravisBadge.apply,
    CodecovBadge.apply, { info =>
      MavenCentralBadge.apply(info.copy(libName = "skeuomorph"))
    },
    ScalaLangBadge.apply,
    LicenseBadge.apply, { info =>
      GitterBadge.apply(info.copy(owner = "higherkindness", repo = "skeuomorph"))
    },
    GitHubIssuesBadge.apply
  ),
  orgEnforcedFilesSetting := List(
    LicenseFileType(orgGithubSetting.value, orgLicenseSetting.value, startYear.value),
    ContributingFileType(
      orgProjectName.value,
      // Organization field can be configured with default value if we migrate it to the frees-io organization
      orgGithubSetting.value.copy(organization = "higherkindness", project = "skeuomorph")
    ),
    AuthorsFileType(name.value, orgGithubSetting.value, orgMaintainersSetting.value, orgContributorsSetting.value),
    NoticeFileType(orgProjectName.value, orgGithubSetting.value, orgLicenseSetting.value, startYear.value),
    VersionSbtFileType,
    ChangelogFileType,
    ReadmeFileType(
      orgProjectName.value,
      orgGithubSetting.value,
      startYear.value,
      orgLicenseSetting.value,
      orgCommitBranchSetting.value,
      sbtPlugin.value,
      name.value,
      version.value,
      scalaBinaryVersion.value,
      sbtBinaryVersion.value,
      orgSupportedScalaJSVersion.value,
      orgBadgeListSetting.value
    ),
    ScalafmtFileType,
    TravisFileType(crossScalaVersions.value, orgScriptCICommandKey, orgAfterCISuccessCommandKey)
  ),
  orgScriptTaskListSetting := List(
    (clean in Global).asRunnableItemFull,
    (compile in Compile).asRunnableItemFull,
    (test in Test).asRunnableItemFull,
    "docs/tut".asRunnableItem
  ),
  releaseIgnoreUntrackedFiles := true,
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
