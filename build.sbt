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


// General Settings
lazy val commonSettings = Seq(
  organization := "com.pepegar",
  scalaVersion := "2.12.6",
  crossScalaVersions := Seq(scalaVersion.value, "2.11.12"),
  scalafmtOnCompile in ThisBuild := true,
  libraryDependencies ++= Seq(
    "org.typelevel"              %% "cats-core"                 % catsV,
    "org.technomadic"            %% "turtles-core"              % "0.1.0",
    "org.apache.avro"            %  "avro"                      % "1.8.2",
    "org.specs2"                 %% "specs2-core"               % specs2V % Test,
    "org.specs2"                 %% "specs2-scalacheck"         % specs2V % Test,
    "org.typelevel"              %% "discipline"                % disciplineV % Test,
    "io.chrisdavenport"          %% "cats-scalacheck"           % "0.1.0" % Test
  )
) ++ compilerPlugins

lazy val compilerPlugins = Seq(
  libraryDependencies ++= Seq(
    compilerPlugin("org.spire-math" % "kind-projector"      % "0.9.7" cross CrossVersion.binary),
    compilerPlugin("com.olegpy"     %% "better-monadic-for" % "0.2.4"),
  )
)

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

lazy val noPublishSettings = Seq(
  skip in publish := true,
  publish := (()),
  publishLocal := (()),
  publishArtifact := false,
  publishTo := None
)
