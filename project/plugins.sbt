ThisBuild / libraryDependencySchemes ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
)
addSbtPlugin("org.scoverage"             % "sbt-scoverage"     % "2.0.8")
addSbtPlugin("com.github.sbt"            % "sbt-ci-release"    % "1.5.12")
addSbtPlugin("com.47deg"                 % "sbt-microsites"    % "1.4.3")
addSbtPlugin("org.scalameta"             % "sbt-scalafmt"      % "2.5.0")
addSbtPlugin("org.scalameta"             % "sbt-mdoc"          % "2.3.7")
addSbtPlugin("de.heikoseeberger"         % "sbt-header"        % "5.10.0")
addSbtPlugin("com.alejandrohdezma"       % "sbt-codecov"       % "0.2.1")
addSbtPlugin("com.alejandrohdezma"       % "sbt-github"        % "0.11.11")
addSbtPlugin("com.alejandrohdezma"       % "sbt-github-header" % "0.11.11")
addSbtPlugin("com.alejandrohdezma"       % "sbt-github-mdoc"   % "0.11.11")
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat"      % "0.4.2")
