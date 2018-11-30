import microsites._
import sbtorgpolicies.OrgPoliciesPlugin.autoImport._
import sbtorgpolicies.model._
import sbtorgpolicies.runnable.SetSetting
import sbtorgpolicies.runnable.syntax._
import sbtorgpolicies.templates._
import sbtorgpolicies.templates.badges._
import scoverage.ScoverageKeys

lazy val root = project
  .in(file("."))
  .settings(commonSettings)

lazy val docs = project
  .in(file("docs"))
  .dependsOn(root)
  .settings(moduleName := "skeuomorph-docs")
  .settings(commonSettings)
  .settings(sbtMicrositesSettings)
  .settings(noPublishSettings)
  .settings(
    micrositeName := "Skeuomorph",
    micrositeDescription := "Schema transformations",
    micrositeBaseUrl := "/skeuomorph",
    micrositeGithubOwner := "higherkindness",
    micrositeGithubRepo := "skeuomorph",
    micrositeHighlightTheme := "tomorrow",
    includeFilter in Jekyll := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.md",
    micrositePushSiteWith := GitHub4s,
    micrositeExtraMdFiles := Map(
      file("README.md") -> ExtraMdFileConfig(
        "index.md",
        "home",
        Map("title" -> "Home", "section" -> "home", "position" -> "0")
      ),
      file("CHANGELOG.md") -> ExtraMdFileConfig(
        "changelog.md",
        "home",
        Map("title" -> "changelog", "section" -> "changelog", "position" -> "99")
      )
    ),
    scalacOptions in Tut ~= filterConsoleScalacOptions,
    scalacOptions in Tut += "-language:postfixOps"
  )
  .enablePlugins(MicrositesPlugin)

lazy val readme = (project in file("readme"))
  .settings(moduleName := "skeuomorph-readme")
  .dependsOn(root)
  .settings(commonSettings)
  .settings(noPublishSettings)
  .settings(
    tutSourceDirectory := baseDirectory.value,
    tutTargetDirectory := baseDirectory.value.getParentFile,
    tutNameFilter := """README.md""".r,
    scalacOptions ~= (_ filterNot Set("-Xfatal-warnings", "-Ywarn-unused-import", "-Xlint").contains)
  )
  .enablePlugins(TutPlugin)

// check for library updates whenever the project is [re]load
onLoad in Global := { s =>
  "dependencyUpdates" :: s
}

pgpPassphrase := Some(getEnvVar("PGP_PASSPHRASE").getOrElse("").toCharArray)
pgpPublicRing := file(s"$gpgFolder/pubring.gpg")
pgpSecretRing := file(s"$gpgFolder/secring.gpg")

// General Settings
lazy val commonSettings = Seq(
  organization := "higherkindness",
  scalaVersion := "2.12.7",
  startYear := Some(2018),
  crossScalaVersions := Seq(scalaVersion.value, "2.11.12"),
  ThisBuild / scalacOptions -= "-Xplugin-require:macroparadise",
  libraryDependencies ++= Seq(
    %%("cats-laws") % Test,
    %%("cats-core"),
    "io.higherkindness"   %% "droste-core"   % "0.5.0",
    "io.higherkindness"   %% "droste-macros" % "0.5.0",
    "org.apache.avro"     % "avro"           % "1.8.2",
    // TODO: PICK ONE
    "com.github.os72" % "protoc-jar" % "3.6.0",
    "com.google.protobuf"   % "protobuf-java"       % "3.6.1",
    "com.thesamet.scalapb" %% "compilerplugin" % "0.8.2",
    "com.thesamet.scalapb" %% "scalapb-runtime" % "0.8.2",
    %%("circe-core"),
    %%("specs2-core")       % Test,
    %%("specs2-scalacheck") % Test,
    "io.chrisdavenport"     %% "cats-scalacheck" % "0.1.0" % Test
  ),
//  classpathTypes += "maven-plugin",
  orgProjectName := "Skeuomorph",
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
    AuthorsFileType(
      name.value,
      orgGithubSetting.value,
      orgMaintainersSetting.value,
      orgContributorsSetting.value),
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
    "docs/tut".asRunnableItem,
    "readme/tut".asRunnableItem
  )
) ++ compilerPlugins

lazy val compilerPlugins = Seq(
  libraryDependencies ++= Seq(
    compilerPlugin("org.spire-math"  % "kind-projector"      % "0.9.8" cross CrossVersion.binary),
    compilerPlugin("com.olegpy"      %% "better-monadic-for" % "0.2.4"),
    compilerPlugin("org.scalamacros" % "paradise"            % "2.1.1" cross CrossVersion.patch)
  )
)
