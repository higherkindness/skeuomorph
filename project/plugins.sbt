ThisBuild / libraryDependencySchemes ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
)
addSbtPlugin("org.scoverage"       % "sbt-scoverage"     % "2.3.1")
addSbtPlugin("com.github.sbt"      % "sbt-ci-release"    % "1.11.2")
// sbt-microsites removed for this local spike: it pulls sbt-site 1.4.1, which depends on the
// now-dead repo.typesafe.com and can't resolve at all, unrelated to the Scala 3 port itself.
addSbtPlugin("org.scalameta"       % "sbt-scalafmt"      % "2.5.5")
addSbtPlugin("org.scalameta"       % "sbt-mdoc"          % "2.7.2")
addSbtPlugin("de.heikoseeberger"   % "sbt-header"        % "5.10.0")
addSbtPlugin("com.alejandrohdezma" % "sbt-codecov"       % "0.2.1")
addSbtPlugin("com.alejandrohdezma" % "sbt-github"        % "0.12.0")
addSbtPlugin("com.alejandrohdezma" % "sbt-github-header" % "0.12.0")
addSbtPlugin("com.alejandrohdezma" % "sbt-github-mdoc"   % "0.12.0")
addSbtPlugin("org.typelevel"       % "sbt-tpolecat"      % "0.5.1")
