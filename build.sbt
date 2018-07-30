import microsites._

lazy val core = project
  .in(file("."))
  .settings(commonSettings)
  .settings(
    name := "skeuomorph"
  )

lazy val docs = project
  .in(file("docs"))
  .dependsOn(core)
  .settings(moduleName := "skeuomorph-docs")
  .settings(commonSettings)
  .settings(compilerPlugins)
  .settings(noPublishSettings)
  .settings(
    micrositeName := "Skeuomorph",
    micrositeDescription := "IDL schema transformations",
    micrositeBaseUrl := "frees.io/skeuomorph",
    micrositeGithubOwner := "frees-io",
    micrositeGithubRepo := "skeuomorph",
    micrositeHighlightTheme := "tomorrow",
    micrositePushSiteWith := GitHub4s,
    micrositeGithubToken := sys.env.get("GITHUB_TOKEN"),
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

val catsV       = "1.1.0"
val kittensV    = "1.1.0"
val catsEffectV = "0.10.1"
val mouseV      = "0.17"
val shapelessV  = "2.3.2"
val fs2V        = "0.10.5"
val http4sV     = "0.18.14"
val circeV      = "0.9.3"
val doobieV     = "0.5.3"
val pureConfigV = "0.9.1"
val refinedV    = "0.9.1"

val specs2V      = "4.2.0"
val disciplineV  = "0.8"
val scShapelessV = "1.1.6"

lazy val contributors = Seq(
  "pepegar" -> "Pepe Garcia"
)

// check for library updates whenever the project is [re]load
onLoad in Global := { s =>
  "dependencyUpdates" :: s
}


lazy val compilerPlugins = Seq(
  libraryDependencies ++= Seq(
    compilerPlugin("org.spire-math" % "kind-projector"      % "0.9.7" cross CrossVersion.binary),
    compilerPlugin("com.olegpy"     %% "better-monadic-for" % "0.2.4"),
  )
)


// General Settings
lazy val commonSettings = Seq(
  organization := "com.pepegar",
  scalaVersion := "2.12.6",
  crossScalaVersions := Seq(scalaVersion.value, "2.11.12"),
  scalafmtOnCompile in ThisBuild := true,
  libraryDependencies ++= Seq(
    "org.typelevel"              %% "cats-core"                 % catsV,
    "org.typelevel"              %% "kittens"                   % kittensV,
    "org.typelevel"              %% "alleycats-core"            % catsV,
    "org.typelevel"              %% "mouse"                     % mouseV,
    "org.typelevel"              %% "cats-effect"               % catsEffectV,
    "org.technomadic"            %% "turtles-core"              % "0.1.0",
    "org.apache.avro"            %  "avro"                      % "1.8.2",
    "org.specs2"                 %% "specs2-core"               % specs2V % Test,
    "org.specs2"                 %% "specs2-scalacheck"         % specs2V % Test,
    "org.typelevel"              %% "discipline"                % disciplineV % Test,
    "io.chrisdavenport"          %% "cats-scalacheck"           % "0.1.0" % Test
  )
) ++ compilerPlugins

lazy val releaseSettings = {
  import ReleaseTransformations._
  Seq(
    releaseCrossBuild := true,
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      // For non cross-build projects, use releaseStepCommand("publishSigned")
      releaseStepCommandAndRemaining("+publishSigned"),
      setNextVersion,
      commitNextVersion,
      releaseStepCommand("sonatypeReleaseAll"),
      pushChanges
    ),
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    credentials ++= (
      for {
        username <- Option(System.getenv().get("SONATYPE_USERNAME"))
        password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
      } yield
        Credentials(
          "Sonatype Nexus Repository Manager",
          "oss.sonatype.org",
          username,
          password
        )
    ).toSeq,
    publishArtifact in Test := false,
    releasePublishArtifactsAction := PgpKeys.publishSigned.value,
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/pepegar/skeuomorph"),
        "git@github.com:pepegar/skeuomorph.git"
      )
    ),
    homepage := Some(url("https://github.com/pepegar/skeuomorph")),
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
    publishMavenStyle := true,
    pomIncludeRepository := { _ =>
      false
    },
    pomExtra := {
      <developers>
        {for ((username, name) <- contributors) yield
        <developer>
          <id>{username}</id>
          <name>{name}</name>
          <url>http://github.com/{username}</url>
        </developer>
        }
      </developers>
    }
  )
}

lazy val mimaSettings = {
  import sbtrelease.Version

  def semverBinCompatVersions(major: Int, minor: Int, patch: Int): Set[(Int, Int, Int)] = {
    val majorVersions: List[Int] = List(major)
    val minorVersions: List[Int] =
      if (major >= 1) Range(0, minor).inclusive.toList
      else List(minor)
    def patchVersions(currentMinVersion: Int): List[Int] =
      if (minor == 0 && patch == 0) List.empty[Int]
      else if (currentMinVersion != minor) List(0)
      else Range(0, patch - 1).inclusive.toList

    val versions = for {
      maj <- majorVersions
      min <- minorVersions
      pat <- patchVersions(min)
    } yield (maj, min, pat)
    versions.toSet
  }

  def mimaVersions(version: String): Set[String] = {
    Version(version) match {
      case Some(Version(major, Seq(minor, patch), _)) =>
        semverBinCompatVersions(major.toInt, minor.toInt, patch.toInt)
          .map { case (maj, min, pat) => maj.toString + "." + min.toString + "." + pat.toString }
      case _ =>
        Set.empty[String]
    }
  }
  // Safety Net For Exclusions
  lazy val excludedVersions: Set[String] = Set()

  // Safety Net for Inclusions
  lazy val extraVersions: Set[String] = Set()

  Seq(
    mimaFailOnProblem := mimaVersions(version.value).toList.headOption.isDefined,
    mimaPreviousArtifacts := (mimaVersions(version.value) ++ extraVersions)
      .filterNot(excludedVersions.contains(_))
      .map { v =>
        val moduleN = moduleName.value + "_" + scalaBinaryVersion.value.toString
        organization.value % moduleN % v
      },
    mimaBinaryIssueFilters ++= {
      import com.typesafe.tools.mima.core._
      import com.typesafe.tools.mima.core.ProblemFilters._
      Seq()
    }
  )
}

lazy val noPublishSettings = Seq(
  skip in publish := true,
  publish := (()),
  publishLocal := (()),
  publishArtifact := false,
  publishTo := None
)
