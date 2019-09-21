import microsites._
import sbtorgpolicies.OrgPoliciesPlugin.autoImport._
import sbtorgpolicies.model._
import sbtorgpolicies.runnable.syntax._
import sbtorgpolicies.templates._
import sbtorgpolicies.templates.badges._

val V = new {
  val avro             = "1.8.2"
  val betterMonadicFor = "0.3.1"
  val cats             = "1.6.0"
  val catsEffect       = "1.2.0"
  val catsScalacheck   = "0.1.1"
  val circe            = "0.11.1"
  val droste           = "0.6.0"
  val kindProjector    = "0.9.9"
  val macroParadise    = "2.1.1"
  val scalacheck       = "1.13.5"
  val specs2           = "4.1.0" // DO NOT BUMP. We need all dependent libraries to bump version of scalacheck to 1.14, otherwise we face a bincompat issue between scalacheck 1.14 & scalacheck 1.13.5
  val protoc           = "3.6.0.1"
  val protobuf         = "3.6.1"
}

lazy val skeuomorph = project
  .in(file("."))
  .settings(commonSettings)
  .settings(moduleName := "skeuomorph")
  .settings( libraryDependencies ++= Seq(
    ("com.lihaoyi" % "ammonite" % "1.6.7" % "test").cross(CrossVersion.full)
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
  .settings(tutSettings)
  .settings(
    micrositeName := "Skeuomorph",
    micrositeDescription := "Schema transformations",
    micrositeBaseUrl := "/skeuomorph",
    micrositeGithubOwner := "higherkindness",
    micrositeGithubRepo := "skeuomorph",
    micrositeHighlightTheme := "tomorrow",
    includeFilter in Jekyll := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.md",
    micrositeGithubToken := getEnvVar(orgGithubTokenSetting.value),
    micrositePushSiteWith := GitHub4s,
    micrositeExtraMdFiles := Map(
      file("CHANGELOG.md") -> ExtraMdFileConfig(
        "changelog.md",
        "home",
        Map("title" -> "changelog", "section" -> "changelog", "position" -> "99")
      )
    )
  )
  .enablePlugins(MicrositesPlugin)

// check for library updates whenever the project is [re]load
onLoad in Global := { s =>
  "dependencyUpdates" :: s
}

pgpPassphrase := Some(getEnvVar("PGP_PASSPHRASE").getOrElse("").toCharArray)
pgpPublicRing := file(s"$gpgFolder/pubring.gpg")
pgpSecretRing := file(s"$gpgFolder/secring.gpg")

// General Settings
lazy val commonSettings = Seq(
  name := "skeuomorph",
  orgGithubSetting := GitHubSettings(
    organization = "higherkindness",
    project = (name in LocalRootProject).value,
    organizationName = "47 Degrees",
    groupId = "io.higherkindness",
    organizationHomePage = url("http://47deg.com"),
    organizationEmail = "hello@47deg.com"
  ),
  scalaVersion := "2.12.8",
  startYear := Some(2018),
  crossScalaVersions := Seq(scalaVersion.value, "2.11.12"),
  ThisBuild / scalacOptions -= "-Xplugin-require:macroparadise",
  libraryDependencies ++= Seq(
    %%("cats-laws", V.cats) % Test,
    %%("cats-core", V.cats),
    "io.higherkindness"   %% "droste-core"   % V.droste,
    "io.higherkindness"   %% "droste-macros" % V.droste,
    "org.apache.avro"     % "avro"           % V.avro,
    "com.github.os72"     % "protoc-jar"     % V.protoc,
    "com.google.protobuf" % "protobuf-java"  % V.protobuf,
    "io.circe"            %% "circe-yaml"    % "0.10.0",
    "io.circe"            %% "circe-testing" % V.circe % Test,
    %%("cats-effect", V.catsEffect),
    %%("circe-core", V.circe),
    %%("circe-parser", V.circe)       ,
    %%("scalacheck", V.scalacheck)    % Test,
    %%("specs2-core", V.specs2)       % Test,
    %%("specs2-scalacheck", V.specs2) % Test,
    "io.chrisdavenport"               %% "cats-scalacheck" % V.catsScalacheck % Test excludeAll (
      ExclusionRule(organization = "org.scalacheck")
    )
  ),
  orgProjectName := "Skeuomorph",
  orgUpdateDocFilesSetting += baseDirectory.value / "docs",
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
) ++ compilerPlugins

lazy val tutSettings = Seq(
  scalacOptions in Tut ~= filterConsoleScalacOptions,
  scalacOptions ~= (_ filterNot Set("-Xfatal-warnings", "-Ywarn-unused-import", "-Xlint").contains),
  scalacOptions in Tut += "-language:postfixOps"
)

lazy val compilerPlugins = Seq(
  libraryDependencies ++= Seq(
    compilerPlugin("org.spire-math"  % "kind-projector"      % V.kindProjector cross CrossVersion.binary),
    compilerPlugin("com.olegpy"      %% "better-monadic-for" % V.betterMonadicFor),
    compilerPlugin("org.scalamacros" % "paradise"            % V.macroParadise cross CrossVersion.patch)
  )
)
