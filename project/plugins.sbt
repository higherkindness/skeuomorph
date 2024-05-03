ThisBuild / libraryDependencySchemes ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
)
addSbtPlugin("org.scoverage"       % "sbt-scoverage"     % "2.0.12")
addSbtPlugin("com.github.sbt"      % "sbt-ci-release"    % "1.5.12")
addSbtPlugin("com.47deg"           % "sbt-microsites"    % "1.4.4")
addSbtPlugin("org.scalameta"       % "sbt-scalafmt"      % "2.5.2")
addSbtPlugin("org.scalameta"       % "sbt-mdoc"          % "2.5.2")
addSbtPlugin("de.heikoseeberger"   % "sbt-header"        % "5.10.0")
addSbtPlugin("com.alejandrohdezma" % "sbt-codecov"       % "0.2.1")
addSbtPlugin("com.alejandrohdezma" % "sbt-github"        % "0.11.13")
addSbtPlugin("com.alejandrohdezma" % "sbt-github-header" % "0.11.13")
addSbtPlugin("com.alejandrohdezma" % "sbt-github-mdoc"   % "0.11.13")
addSbtPlugin("org.typelevel"       % "sbt-tpolecat"      % "0.5.1")
