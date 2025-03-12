ThisBuild / libraryDependencySchemes ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
)
addSbtPlugin("org.scoverage"       % "sbt-scoverage"     % "2.3.1")
addSbtPlugin("com.github.sbt"      % "sbt-ci-release"    % "1.9.3")
addSbtPlugin("com.47deg"           % "sbt-microsites"    % "1.4.4")
addSbtPlugin("org.scalameta"       % "sbt-scalafmt"      % "2.5.4")
addSbtPlugin("org.scalameta"       % "sbt-mdoc"          % "2.6.4")
addSbtPlugin("de.heikoseeberger"   % "sbt-header"        % "5.10.0")
addSbtPlugin("com.alejandrohdezma" % "sbt-codecov"       % "0.2.1")
addSbtPlugin("com.alejandrohdezma" % "sbt-github"        % "0.12.0")
addSbtPlugin("com.alejandrohdezma" % "sbt-github-header" % "0.12.0")
addSbtPlugin("com.alejandrohdezma" % "sbt-github-mdoc"   % "0.12.0")
addSbtPlugin("org.typelevel"       % "sbt-tpolecat"      % "0.5.1")
