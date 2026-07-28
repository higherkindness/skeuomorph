ThisBuild / organization       := "io.higherkindness"
ThisBuild / githubOrganization := "47degrees"
ThisBuild / scalaVersion       := "2.13.16"
ThisBuild / crossScalaVersions := Seq("2.12.20", "2.13.16", "3.3.5")

addCommandAlias("ci-test", "scalafmtCheckAll; scalafmtSbtCheck; microsite/mdoc; +test")
addCommandAlias("ci-docs", "github; documentation/mdoc; headerCreateAll; microsite/publishMicrosite")
addCommandAlias("ci-publish", "github; ci-release")

lazy val skeuomorph = project
  .in(file("."))
  .settings(commonSettings)
  .settings(moduleName := "skeuomorph")

// microsite project removed for this local spike (see project/plugins.sbt note).

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
  // JsonSchemaF's `derives Traverse` (16 case-class variants) recurses deeper than Scala 3's
  // default inline-expansion budget during shapeless3/kittens-style K1 derivation.
  scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, _)) => Seq("-Xmax-inlines", "64")
    case _            => Seq.empty
  }),
  libraryDependencies ++= Seq(
    // mu/comparison/Comparison.scala (Scala 2 only, see scala-2/ source split) needs this
    // directly; something in the current dependency graph stopped bringing it in transitively.
    "org.scala-lang.modules" %% "scala-collection-compat" % "2.13.0",
    "org.typelevel"        %% "cats-core"         % "2.13.0",
    "org.typelevel"        %% "cats-effect"       % "3.6.3",
    "io.higherkindness"    %% "droste-core"       % "0.10.0",
    "io.higherkindness"    %% "droste-macros"     % "0.10.0",
    "org.apache.avro"       % "avro"              % "1.12.0",
    "com.github.os72"       % "protoc-jar"        % "3.11.4",
    "com.google.protobuf"   % "protobuf-java"     % "4.32.1",
    "io.circe"             %% "circe-core"        % "0.14.14",
    "io.circe"             %% "circe-parser"      % "0.14.14",
    "io.circe"             %% "circe-yaml"        % "1.15.0"
  ),
  // Test-only deps for this local spike: several (specs2's "-js-ec" build in particular)
  // have no Scala 3 artifact at all, and porting the mu/avro/protobuf test suites (which
  // is what most of these actually serve) is out of scope here — we're only validating
  // that the *library* (Compile scope) is portable to Scala 3.
  libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, _)) =>
      Seq(
        "com.julianpeeters"    %% "avrohugger-core"   % "2.8.4"        % Test,
        "org.typelevel"        %% "cats-laws"         % "2.13.0"       % Test,
        "io.circe"             %% "circe-testing"     % "0.14.14"      % Test,
        "org.typelevel"        %% "discipline-specs2" % "1.5.0"        % Test,
        "org.specs2"           %% "specs2-core"       % "4.12.4-js-ec" % Test,
        "org.specs2"           %% "specs2-scalacheck" % "4.12.4-js-ec" % Test,
        "org.scalacheck"       %% "scalacheck"        % "1.19.0"       % Test,
        "io.chrisdavenport"    %% "cats-scalacheck"   % "0.3.2"        % Test,
        "org.scalatra.scalate" %% "scalate-core"      % "1.10.1"       % Test
      )
    case _ => Seq.empty
  })
) ++ compilerPlugins ++ macroSettings ++ Seq(
  // Printer.scala's toValidIdentifier uses scala.meta.Term.Name regardless of Scala version
  // (it's just an identifier-escaping helper, not a macro) — this was needlessly gated to
  // Scala 2 only before; bumped to a version cross-published for 2.12/2.13/3 alike.
  libraryDependencies += "org.scalameta" %% "scalameta" % "4.17.3"
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
  // scala-reflect macro annotations (droste's `@deriveTraverse`) are a Scala 2-only
  // mechanism; Scala 3 doesn't have a "scala-compiler" artifact under this name at all,
  // and doesn't need it since the openapi/JsonSchema.scala Scala 3 source uses `derives`
  // clauses instead (see src/main/scala-3).
  libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, _)) => Seq(scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided)
    case _            => Seq.empty
  }),
  libraryDependencies ++= on(2, 12)(
    compilerPlugin("org.scalamacros" %% "paradise" % "2.1.1" cross CrossVersion.full)
  ).value,
  scalacOptions ++= on(2, 13)("-Ymacro-annotations").value,
  // shapeless3-deriving backs droste's Scala 3 `derives Traverse` support.
  libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, _)) => Seq("org.typelevel" %% "shapeless3-deriving" % "3.5.0")
    case _            => Seq.empty
  })
)

def on[A](major: Int, minor: Int)(a: A): Def.Initialize[Seq[A]] =
  Def.setting {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some(v) if v == (major, minor) => Seq(a)
      case _                              => Nil
    }
  }
