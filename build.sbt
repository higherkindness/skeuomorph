import microsites._
import sbtorgpolicies.OrgPoliciesPlugin.autoImport._
import sbtorgpolicies.templates.badges._

lazy val root = project
  .in(file("."))
  .settings(commonSettings)
  .settings(
    name := "skeuomorph"
  )

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
    micrositeGithubOwner := "frees-io",
    micrositeGithubRepo := "skeuomorph",
    micrositeHighlightTheme := "tomorrow",
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

// check for library updates whenever the project is [re]load
onLoad in Global := { s =>
  "dependencyUpdates" :: s
}

pgpPassphrase := Some(getEnvVar("PGP_PASSPHRASE").getOrElse("").toCharArray)
pgpPublicRing := file(s"$gpgFolder/pubring.gpg")
pgpSecretRing := file(s"$gpgFolder/secring.gpg")

// General Settings
lazy val commonSettings = Seq(
  organization := "io.frees",
  scalaVersion := "2.12.6",
  startYear := Some(2018),
  crossScalaVersions := Seq(scalaVersion.value, "2.11.12"),
  ThisBuild / scalacOptions -= "-Xplugin-require:macroparadise",
  libraryDependencies ++= Seq(
    %%("cats-core"),
    "io.higherkindness" %% "droste-core" % "0.3.1",
    "org.apache.avro"   % "avro"             % "1.8.2",
    %%("circe-core"),
    %%("specs2-core") % Test,
    %%("specs2-scalacheck") % Test,
    "io.chrisdavenport" %% "cats-scalacheck" % "0.1.0" % Test
  ),
  orgProjectName := "Skeuomorph",
  orgBadgeListSetting := List(
    TravisBadge.apply,
    CodecovBadge.apply,
    { info => MavenCentralBadge.apply(info.copy(libName = "skeuomorph")) },
    ScalaLangBadge.apply,
    LicenseBadge.apply,
    { info => GitterBadge.apply(info.copy(owner = "frees-io", repo = "skeuomorph")) },
    GitHubIssuesBadge.apply
  )
) ++ compilerPlugins

lazy val compilerPlugins = Seq(
  libraryDependencies ++= Seq(
    compilerPlugin("org.spire-math" % "kind-projector"      % "0.9.7" cross CrossVersion.binary),
    compilerPlugin("com.olegpy"     %% "better-monadic-for" % "0.2.4"),
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch)
  )
)
