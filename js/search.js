// When the user clicks on the search box, we want to toggle the search dropdown
function displayToggleSearch(e) {
  e.preventDefault();
  e.stopPropagation();

  closeDropdownSearch(e);
  
  if (idx === null) {
    console.log("Building search index...");
    prepareIdxAndDocMap();
    console.log("Search index built.");
  }
  const dropdown = document.querySelector("#search-dropdown-content");
  if (dropdown) {
    if (!dropdown.classList.contains("show")) {
      dropdown.classList.add("show");
    }
    document.addEventListener("click", closeDropdownSearch);
    document.addEventListener("keydown", searchOnKeyDown);
    document.addEventListener("keyup", searchOnKeyUp);
  }
}

//We want to prepare the index only after clicking the search bar
var idx = null
const docMap = new Map()

function prepareIdxAndDocMap() {
  const docs = [  
    {
      "title": "changelog",
      "url": "/skeuomorph/docs/changelog/",
      "content": "Changelog v0.1.2 (2021-09-28) Full Changelog 🐛 Bug Fixes Restores the cats implicits import #521 (fedefernandez) v0.1.1 (2021-09-28) Full Changelog 📈 Dependency updates Update discipline-specs2 to 1.2.2 #510 (47erbot) Update scalafmt-core to 3.0.3 #509 (47erbot) Update discipline-specs2 to 1.2.1 #508 (47erbot) Update discipline-specs2 to 1.2.0 #507 (47erbot) Update cats-effect to 3.2.8 #506 (47erbot) Update kind-projector to 0.13.2 #505 (47erbot) Update scalafmt-core to 3.0.2 #503 (47erbot) Update scalafmt-core to 3.0.1 #500 (47erbot) Update sbt-mdoc to 2.2.23 #499 (47erbot) Update kind-projector to 0.13.1 #498 (47erbot) Update scalafmt-core to 3.0.0 #497 (47erbot) Update scalameta to 4.4.27 #495 (47erbot) Update circe-yaml to 0.14.1 #494 (47erbot) Update cats-scalacheck to 0.3.1 #491 (47erbot) Update specs2-core, specs2-scalacheck to 4.12.4-js-ec #489 (47erbot) Update scalameta to 4.4.25 #488 (47erbot) Update sbt-mdoc to 2.2.22 #486 (47erbot) Update scalameta to 4.4.24 #483 (47erbot) Update sbt to 1.5.5 #482 (47erbot) Update sbt-scalafmt to 2.4.3 #481 (47erbot) Update sbt-github, sbt-github-header, … to 0.9.3 #480 (47erbot) Update specs2-core, specs2-scalacheck to 4.12.3 #479 (47erbot) Update scalameta to 4.4.23 #478 (47erbot) Update specs2-core, specs2-scalacheck to 4.12.2 #477 (47erbot) Update scalameta to 4.4.22 #476 (47erbot) Update scalate-core to 1.9.7 #475 (47erbot) Update sbt-microsites to 1.3.4 #440 (47erbot) Merged pull requests: Revert “Update sbt-github, sbt-github-header, … to 0.10.0” #520 (juanpedromoreno) Update scalafmt-core to 3.0.5 #519 (47erbot) Update sbt-ci-release to 1.5.9 #518 (47erbot) Update sbt-github, sbt-github-header, … to 0.10.0 #517 (47erbot) Update cats-effect to 3.2.9 #516 (47erbot) Update scalafmt-core to 3.0.4 #515 (47erbot) Update sbt-scoverage to 1.9.0 #514 (47erbot) Update scalameta to 4.4.28 #513 (47erbot) Update scala-compiler, scala-library to 2.12.15 #512 (47erbot) Update protobuf-java to 3.18.0 #511 (47erbot) Prepare for scala3 crossbuilding #484 (Daenyth) v0.1.0 (2021-06-14) Full Changelog 📈 Dependency updates Update specs2-core, specs2-scalacheck to 4.12.1 #473 (47erbot) Update protobuf-java to 3.17.3 #471 (47erbot) Update protobuf-java to 3.17.2 #469 (47erbot) Update sbt-tpolecat to 0.1.20 #468 (47erbot) Update sbt-scoverage to 1.8.2 #466 (47erbot) Update scalameta to 4.4.20 #465 (47erbot) Update sbt-tpolecat to 0.1.19 #463 (47erbot) Update circe-core, circe-parser, … to 0.14.1 #462 (47erbot) Update protobuf-java to 3.17.1 #461 (47erbot) Update specs2-core, specs2-scalacheck to 4.12.0 #460 (47erbot) Update sbt-tpolecat to 0.1.18 #459 (47erbot) Update sbt-scoverage to 1.8.1 #458 (47erbot) Update scalameta to 4.4.18 #457 (47erbot) Update sbt-mdoc to 2.2.21 #455 (47erbot) Update discipline-specs2 to 1.1.6 #454 (47erbot) Update cats-effect to 2.5.1 #453 (47erbot) Update cats-core, cats-laws to 2.6.1 #452 (47erbot) Update scalameta to 4.4.17 #451 (47erbot) Update protobuf-java to 3.17.0 #450 (47erbot) Update scalameta to 4.4.16 #448 (47erbot) Update sbt-scoverage to 1.7.3 #446 (47erbot) Update scalacheck to 1.15.4 #445 (47erbot) Update sbt-scoverage to 1.7.2 #444 (47erbot) Update sbt-scoverage to 1.7.0 #443 (47erbot) Update specs2-core, specs2-scalacheck to 4.11.0 #442 (47erbot) Update scalameta to 4.4.15 #441 (47erbot) Update discipline-specs2 to 1.1.5 #439 (47erbot) Update scalameta to 4.4.14 #436 (47erbot) Update sbt-mdoc to 2.2.20 #435 (47erbot) Update sbt-codecov to 0.2.1 #434 (47erbot) Update scalameta to 4.4.13 #432 (47erbot) Update scalameta to 4.4.12 #431 (47erbot) Update protobuf-java to 3.15.8 #430 (47erbot) Update scalameta to 4.4.11 #427 (47erbot) Update sbt-microsites to 1.3.3 #426 (47erbot) Update cats-effect to 2.4.1 #425 (47erbot) Update cats-core, cats-laws to 2.5.0 #424 (47erbot) Update sbt-ci-release to 1.5.7 #422 (47erbot) Update cats-effect to 2.4.0 #421 (47erbot) Update avro to 1.10.2 #420 (47erbot) Update specs2-core, specs2-scalacheck to 4.10.6 #388 (47erbot) Merged pull requests: Update to cats-effect 3 #472 (Daenyth) v0.0.29 (2021-03-17) Full Changelog ⚠️ Breaking changes Adding the Logical Type Time millisecond to Avro #417 (IsabelDuran) 📈 Dependency updates Update sbt-tpolecat to 0.1.17 #419 (47erbot) Update sbt-github, sbt-github-header, … to 0.9.2 #418 (47erbot) Update protobuf-java to 3.15.6 #416 (47erbot) Update sbt-ci-release to 1.5.6 #415 (47erbot) Update sbt to 1.4.8 #413 (47erbot) Update sbt-github, sbt-github-header, … to 0.9.1 #412 (47erbot) Update protobuf-java to 3.15.5 #411 (47erbot) Update protobuf-java to 3.15.3 #410 (47erbot) Update scalameta to 4.4.10 #409 (47erbot) Update protobuf-java to 3.15.2 #408 (47erbot) Update scalameta to 4.4.9 #407 (47erbot) Update protobuf-java to 3.15.1 #406 (47erbot) Update cats-effect to 2.3.3 #405 (47erbot) Update cats-core, cats-laws to 2.4.2 #404 (47erbot) Update sbt-mdoc to 2.2.18 #403 (47erbot) Update protobuf-java to 3.15.0 #402 (47erbot) Update discipline-specs2 to 1.1.4 #401 (47erbot) Update scalacheck to 1.15.3 #400 (47erbot) Update avrohugger-core to 1.0.0-RC24 #399 (47erbot) Update avrohugger-core to 1.0.0-RC23 #398 (47erbot) Update cats-core, cats-laws to 2.4.1 #397 (47erbot) Update cats-core, cats-laws to 2.4.0 #396 (47erbot) Update sbt-mdoc to 2.2.17 #395 (47erbot) Update scalameta to 4.4.8 #394 (47erbot) Update sbt-microsites to 1.3.2 #393 (47erbot) Update sbt-microsites to 1.3.1 #391 (47erbot) Update scalameta to 4.4.7 #390 (47erbot) Update sbt-mdoc to 2.2.16 #387 (47erbot) Update scalameta to 4.4.5 #385 (47erbot) Update scalameta to 4.4.4 #384 (47erbot) Update sbt to 1.4.6 #383 (47erbot) Update sbt-microsites to 1.3.0 #382 (47erbot) v0.0.28 (2020-12-23) Full Changelog ⚠️ Breaking changes Stop setting methodNameStyle = Capitalize when useIdiomaticEndpoints is true #342 (L-Lavigne) 🐛 Bug Fixes Invalid enums generated if package option is not supplied #337 📈 Dependency updates Update scalameta to 4.4.3 #381 (47erbot) Update discipline-specs2 to 1.1.3 #380 (47erbot) Update cats-core, cats-effect, cats-laws to 2.3.1 #379 (47erbot) Update sbt-mdoc to 2.2.14 #378 (47erbot) Update scalacheck to 1.15.2 #377 (47erbot) Update sbt to 1.4.5 #376 (47erbot) Update sbt-github, sbt-github-header, … to 0.9.0 #375 (47erbot) Update scalameta to 4.4.1 #373 (47erbot) Update sbt-tpolecat to 0.1.16 #372 (47erbot) Update sbt-ci-release to 1.5.5 #371 (47erbot) Update avro to 1.10.1 #370 (47erbot) Update kind-projector to 0.11.2 #369 (47erbot) Update discipline-specs2 to 1.1.2 #368 (47erbot) Update sbt-mdoc to 2.2.13 #366 (47erbot) Update kind-projector to 0.11.1 #365 (47erbot) Update sbt to 1.4.4 #364 (47erbot) Update scalameta to 4.4.0 #363 (47erbot) Update discipline-specs2 to 1.1.1 #362 (47erbot) Update sbt-mdoc to 2.2.12 #361 (47erbot) Update scalacheck to 1.15.1 #360 (47erbot) Update sbt-tpolecat to 0.1.15 #359 (47erbot) Update protobuf-java to 3.14.0 #358 (47erbot) Update sbt to 1.4.3 #356 (47erbot) Update scalacheck to 1.15.0 #355 (47erbot) Update sbt to 1.4.2 #354 (47erbot) Update sbt to 1.4.1 #353 (47erbot) Update scalafmt-core to 2.7.5 #352 (47erbot) Update protobuf-java to 3.13.0 #350 (47erbot) Update specs2-core, specs2-scalacheck to 4.10.5 #349 (47erbot) Update protobuf-java to 3.12.4 #313 (47erbot) Merged pull requests: Extend package choosing behavior to protobuf files with Enum types #339 (dmarticus) v0.0.27 (2020-10-15) Full Changelog 🚀 Features Adds logical and fixed types to code generation #340 (dzanot) 📈 Dependency updates Update specs2-core, specs2-scalacheck to 4.10.4 #348 (47erbot) v0.0.26 (2020-10-09) Full Changelog 🚀 Features Investigate allowing optional package parameters when parsing Protobuf schema #318 📈 Dependency updates Update sbt-tpolecat to 0.1.14 #347 (47erbot) Update scalameta to 4.3.24 #346 (47erbot) Update scalafmt-core to 2.7.4 #345 (47erbot) Update sbt to 1.4.0 #344 (47erbot) Update sbt-mdoc to 2.2.9 #335 (47erbot) Update scalafmt-core to 2.7.1 #334 (47erbot) Update sbt-mdoc to 2.2.8 #333 (47erbot) Update scalameta to 4.3.22 #332 (47erbot) Update sbt-mdoc to 2.2.7 #331 (47erbot) Update scala-collection-compat to 2.2.0 #330 (47erbot) Update cats-effect to 2.2.0 #329 (47erbot) Update scalafmt-core to 2.7.0 #328 (47erbot) Update cats-core, cats-laws to 2.2.0 #327 (47erbot) Update sbt-mdoc to 2.2.6 #326 (47erbot) v0.0.25 (2020-08-31) Full Changelog 🚀 Features Add documentation #162 📈 Dependency updates Update scalameta to 4.3.21 #325 (47erbot) Update specs2-core, specs2-scalacheck to 4.10.3 #320 (47erbot) Update sbt-mdoc to 2.2.5 #319 (47erbot) Update sbt-mdoc to 2.2.4 #317 (47erbot) Update specs2-core, specs2-scalacheck to 4.10.2 #316 (47erbot) Update sbt-scalafmt to 2.4.2 #315 (47erbot) Update specs2-core, specs2-scalacheck to 4.10.1 #314 (47erbot) Update scalafmt-core to 2.6.4 #312 (47erbot) Update scalameta to 4.3.20 #311 (scala-steward) Update cats-effect to 2.1.4 #310 (scala-steward) Update scalameta to 4.3.19 #309 (scala-steward) Update scalafmt-core to 2.6.3 #308 (scala-steward) Update scalafmt-core to 2.6.2 #307 (scala-steward) Merged pull requests: Respect java\\_package option in protobuf files #324 (dmarticus) Adding documentation for the proto2 incompatibility #321 (dmarticus) v0.0.24 (2020-07-03) Full Changelog 🐛 Bug Fixes Unescaped ‘type’ keyword in package (directory) name #288 📈 Dependency updates Update scalameta to 4.3.18 #304 (scala-steward) Update avro, avro-compiler to 1.10.0 #302 (scala-steward) Update sbt to 1.3.13 #301 (scala-steward) Update sbt-tpolecat to 0.1.13 #300 (scala-steward) Update scalameta to 4.3.17 #299 (scala-steward) Update scalameta to 4.3.16 #298 (scala-steward) Update sbt-mdoc to 2.2.3 #297 (scala-steward) Update specs2-core, specs2-scalacheck to 4.10.0 #294 (scala-steward) Update scalameta to 4.3.15 #291 (scala-steward) Update scalafmt-core to 2.5.3 #290 (scala-steward) Update sbt-mdoc to 2.2.2 #289 (scala-steward) Update sbt-tpolecat to 0.1.12 #286 (scala-steward) Update cats-scalacheck to 0.3.0 #285 (scala-steward) Update sbt to 1.3.11 #284 (scala-steward) Update protobuf-java to 3.12.0 #281 (scala-steward) Update circe-yaml to 0.13.1 #280 (scala-steward) Merged pull requests: Escaping ‘type’ keyword in package (directory) names #305 (juanpedromoreno) Update scalafmt-core to 2.6.1 #296 (BenFradet) Update scalafmt-core to 2.6.0 #293 (BenFradet) Prepare repository for next .github release and SBT build improvements #287 (juanpedromoreno) v0.0.23 (2020-05-04) Full Changelog 📈 Dependency updates Update scalameta to 4.3.10 #272 (scala-steward) Update specs2-core, specs2-scalacheck to 4.9.4 #269 (scala-steward) Merged pull requests: Update scalafmt-core to 2.5.1 #276 (BenFradet) Don’t add @option when generating Mu code #275 (cb372) Update sbt-github to allow overriding organization #268 (alejandrohdezma) Update scala-collection-compat to 2.1.6 #266 (scala-steward) Update circe-yaml to 0.13.0 #264 (scala-steward) Update cats-effect to 2.1.3 #263 (scala-steward) Update scala-collection-compat to 2.1.5 #262 (scala-steward) Update sbt to 1.3.10 #261 (scala-steward) Wrap streaming responses in effect type #260 (cb372) Update specs2-core, specs2-scalacheck to 4.9.3 #259 (scala-steward) Update sbt to 1.3.9 #258 (scala-steward) Update sbt-microsites to 1.1.5 #257 (scala-steward) Update sbt-org-policies to 0.13.3 #256 (scala-steward) Update sbt-microsites to 1.1.4 #255 (scala-steward) Update sbt-org-policies to 0.13.2 #254 (scala-steward) Update sbt-microsites to 1.1.3 #253 (scala-steward) Update discipline-specs2 to 1.1.0 #252 (scala-steward) Update protoc-jar to 3.11.4 #251 (scala-steward) Update specs2-core, specs2-scalacheck to 4.9.2 #250 (scala-steward) Update cats-effect to 2.1.2 #249 (scala-steward) Update specs2-core, specs2-scalacheck to 4.9.1 #248 (scala-steward) Update sbt-mima-plugin to 0.7.0 #246 (scala-steward) Improve conversion from Avro protocol to Mu protocol #245 (cb372) Update cats-core, cats-laws to 2.1.1 #243 (scala-steward) Update sbt-tpolecat to 0.1.11 #242 (scala-steward) Update sbt-org-policies to 0.13.1 #241 (scala-steward) v0.0.22 (2020-02-24) Full Changelog Merged pull requests: Remove use of sbt-org-policies dependency syntax #244 (cb372) Releases 0.0.22 #240 (juanpedromoreno) Update scalafmt-core to 2.4.2 #239 (scala-steward) Update sbt-microsites to 1.1.2 #237 (scala-steward) Cross compilation Scala 2.13 #236 (juanpedromoreno) v0.0.21 (2020-02-21) Full Changelog Closed issues: Fix protobuf enums representation in generated scala. #100 Merged pull requests: Fixes GPG issues #235 (juanpedromoreno) Release 0.0.21 #233 (cb372) Update scalafmt-core to 2.4.1 #232 (scala-steward) Update scalafmt-core to 2.4.0 #231 (scala-steward) Update protobuf-java to 3.11.4 #230 (scala-steward) Mergify: configuration update #229 (angoglez) Update avro to 1.9.2 #228 (scala-steward) Update sbt-mima-plugin to 0.6.4 #227 (scala-steward) Update circe-core, circe-parser, … to 0.13.0 #226 (scala-steward) Update cats-effect to 2.1.1 #225 (scala-steward) Update sbt to 1.3.8 #224 (scala-steward) Update protobuf-java to 3.11.3 #222 (scala-steward) Update cats-effect to 2.1.0 #220 (scala-steward) Remove @message annotation from generated case classes #219 (cb372) Update sbt-org-policies to 0.12.3 #217 (scala-steward) Update sbt to 1.3.7 #216 (scala-steward) Remove deprecated object #215 (cb372) v0.0.20 (2020-01-15) Full Changelog Merged pull requests: Release 0.0.20 #214 (cb372) Support for protobuf signed/unsigned/fixed-width integers #213 (cb372) Update ammonite to 2.0.4 #212 (scala-steward) Update sbt-microsites to 1.1.0 #211 (scala-steward) Update ammonite to 2.0.3 #210 (scala-steward) Update ammonite to 2.0.2 #209 (scala-steward) Update specs2-core, specs2-scalacheck to 4.8.3 #208 (scala-steward) Update specs2-core, specs2-scalacheck to 4.8.2 #207 (scala-steward) Support protobuf nested messages and enums #206 (cb372) v0.0.19 (2020-01-02) Full Changelog Merged pull requests: Use scalameta for codegen #205 (cb372) Release 0.0.19 #204 (cb372) Fix a couple of compilation issues in the generated code #203 (cb372) Fix broken docs #201 (cb372) Includes docs subfolders #200 (juanpedromoreno) v0.0.18 (2020-01-02) Full Changelog Merged pull requests: Release v0.0.18 #199 (cb372) Wrap coproducts in Option #198 (cb372) Generate a shapeless Coproduct instead of a placeholder type #197 (cb372) Update ammonite to 2.0.1 #196 (scala-steward) Update ammonite to 1.9.9 #195 (scala-steward) Update sbt to 1.3.6 #194 (scala-steward) Update ammonite to 1.9.8 #193 (scala-steward) Update ammonite to 1.9.2 #191 (scala-steward) Update ammonite to 1.9.1 #190 (scala-steward) Update cats-core, cats-laws to 2.1.0 #189 (BenFradet) Add pbIndex annotation to generated Scala code #187 (cb372) Update scalacheck to 1.14.3 #186 (scala-steward) Update sbt to 1.3.5 #185 (scala-steward) Use enumeratum IntEnum for generated enums #184 (cb372) Protobuf: make generated Scala code more hygienic #183 (cb372) Adds twitter-card.png and removes nav title in small size view #182 (AntonioMateoGomez) Update protoc-jar to 3.11.1 #181 (scala-steward) Update sbt-microsites to 1.0.2 #180 (scala-steward) v0.0.17 (2019-12-05) Full Changelog 🐛 Bug Fixes Add a new combinator to escape Scala reserved words #114 Merged pull requests: Release v0.0.17 #179 (cb372) Avoid generating invalid Scala identifiers when printing schemas and protocols #178 (cb372) add descriptions front #177 (pepegar) Travis microsite stage requisites update #176 (calvellido) Rewrite the doc examples slightly for consistency #173 (cb372) v0.0.16 (2019-12-04) Full Changelog 🐛 Bug Fixes Fix proto imports cannot see other proto folders #111 Merged pull requests: Releases 0.0.16 #175 (juanpedromoreno) Updates microsite using sbt-microsites v1.0.1 #174 (AntonioMateoGomez) Update protobuf-java to 3.11.1 #172 (scala-steward) Update protoc-jar to 3.10.1 #170 (scala-steward) Update sbt-tpolecat to 0.1.10 #169 (scala-steward) Update sbt-tpolecat to 0.1.9 #168 (scala-steward) Update protobuf-java to 3.11.0 #167 (scala-steward) Update ammonite to 1.8.2 #165 (scala-steward) Update sbt to 1.3.4 #164 (scala-steward) Update sbt-scoverage to 1.6.1 #163 (scala-steward) Update droste-core, droste-macros to 0.8.0 #161 (scala-steward) Update specs2-core, specs2-scalacheck to 4.8.1 #160 (scala-steward) Update circe-yaml to 0.12.0 #159 (scala-steward) Update ammonite to 1.8.1 #158 (scala-steward) Update ammonite to 1.8.0 #157 (scala-steward) Update circe-core, circe-parser, … to 0.12.3 #156 (scala-steward) Update specs2-core, specs2-scalacheck to 4.8.0 #155 (scala-steward) Update sbt-updates to 0.5.0 #154 (scala-steward) Update sbt to 1.3.3 #153 (scala-steward) Update circe-core, circe-parser, … to 0.12.2 #152 (scala-steward) Update sbt-updates to 0.4.3 #151 (scala-steward) Update sbt-microsites to 0.9.7 #150 (scala-steward) Update ammonite to 1.7.4 #149 (scala-steward) Update scalacheck to 1.14.2 #148 (scala-steward) Update sbt-microsites to 0.9.6 #147 (scala-steward) v0.0.15 (2019-09-24) Full Changelog Merged pull requests: Upgrades to droste 0.7.0 #146 (juanpedromoreno) Use import root to compile Protobuf files #115 (bilki) v0.0.14 (2019-09-23) Full Changelog Merged pull requests: Upgrades skeuomorph Build #145 (juanpedromoreno) Update sbt to 1.3.2 #139 (scala-steward) Update partial-unification to 1.1.2 #138 (scala-steward) Update sbt-tpolecat to 0.1.8 #135 (scala-steward) Update cats-scalacheck to 0.1.1 #134 (scala-steward) Update sbt-updates to 0.4.2 #132 (scala-steward) Update better-monadic-for to 0.3.1 #131 (scala-steward) Update ammonite to 1.6.7 #130 (scala-steward) Update protobuf-java to 3.10.0 #129 (scala-steward) Open api parser #123 (BeniVF) v0.0.13 (2019-09-16) Full Changelog Closed issues: Open-API Schemas: Client Generation #108 Merged pull requests: Release 0.0.13 #125 (juanpedromoreno) v0.0.12 (2019-09-11) Full Changelog Closed issues: Tut readme out of sync #117 Protobuf oneOf to generate coproducts without Option-ality #113 Support for compression in avro/proto #86 The proto and avro printers need to receive a new param for setting the idiomatic gRPC to true #85 Merged pull requests: Release 0.0.12 #124 (juanpedromoreno) Normalize and more support for open api spec #121 (BeniVF) Removes the optionality in Coproducts #119 (rafaparadela) Fix readme/microsite #118 (dzanot) Nested object #116 (BeniVF) Handle imports from nested folders #112 (bilki) Print encoders #109 (BeniVF) Ff/print multiple responses #102 (BeniVF) v0.0.11 (2019-07-05) Full Changelog 🚀 Features Rename Schemas to Types #31 Closed issues: Improve Option support for Proto2 #89 Merged pull requests: Release 0.0.11 #107 (fedefernandez) CompressionType and useIdiomaticEndpoints for Protobuf and Avro #101 (fedefernandez) Support for compression #99 (AlvaroCaste) Ff/print open api #97 (BeniVF) v0.0.10 (2019-05-30) Full Changelog Merged pull requests: Remove fs2 prefix #96 (noelmarkham) Set version back to 0.0.10-SNAPSHOT following Sonatype issues #95 (noelmarkham) v0.0.9.1 (2019-05-28) Full Changelog Merged pull requests: Create point release for Sonatype being down earlier #94 (noelmarkham) v0.0.9 (2019-05-28) Full Changelog 🚀 Features Decoders for OpenAPI #79 Merged pull requests: Bump version to release optional fields work #93 (noelmarkham) Make fields optional for non-primitive protobuf fields #92 (noelmarkham) Ff/open api to mu #90 (BeniVF) Fix scaladoc generation #88 (vil1) Append the fs2 prefix to the Stream #84 (fedefernandez) Revert “Use the fully qualified name for fs2 Stream” #83 (fedefernandez) Introduce schema comparison #82 (vil1) Adds the parameters to an operation #81 (fedefernandez) v0.0.8 (2019-03-19) Full Changelog Merged pull requests: Fixes scoverage transitive issues #78 (juanpedromoreno) v0.0.7 (2019-03-18) Full Changelog Closed issues: Update dependency org.scalacheck:scalacheck:test #57 Update dependency org.specs2:specs2-scalacheck:test #56 Update dependency org.specs2:specs2-core:test #55 Update dependency org.spire-math:kind-projector:plugin-&gt;default(compile) #33 Parse proto files #30 Merged pull requests: Upgrades codecov plugin #77 (juanpedromoreno) v0.0.6 (2019-03-18) Full Changelog Closed issues: Stop merging dependent messages from imports #74 Merged pull requests: Release skeuomorph 0.0.6 #76 (rafaparadela) Propagates imports instead of cloning dependent messages #75 (rafaparadela) Pushes codecov reports #73 (juanpedromoreno) v0.0.5 (2019-02-28) Full Changelog Closed issues: Update dependency org.typelevel:cats-laws:test #70 Update dependency org.typelevel:cats-effect #69 Update dependency org.typelevel:cats-core #68 Update dependency com.github.os72:protoc-jar #67 Update dependency io.circe:circe-core #54 Merged pull requests: CI Process &amp; Build Upgrade #72 (juanpedromoreno) v0.0.4 (2019-02-19) Full Changelog Merged pull requests: Releases Skeuomorph 0.0.4 #71 (juanpedromoreno) Protobuf compilation: schema and protocol #66 (rafaparadela) add metals &amp; bloop foldesrs to gitignore #53 (pepegar) :tada: use @deriveTraverse annotation in our ADTs #27 (pepegar) make code more readable by using contramapN #25 (pepegar) v0.0.3 (2018-12-12) Full Changelog 🚀 Features Improve package and directory structure #40 Closed issues: Fix README #51 Update dependency org.typelevel:cats-laws:test #45 Update dependency org.typelevel:cats-core #44 Update dependency io.higherkindness:droste-macros #43 Update dependency io.higherkindness:droste-core #42 finish move to higherkindness #18 Merged pull requests: Build Fixes and Release 0.0.3 #52 (juanpedromoreno) Move code under higherkindness/skeuomorph dirs #50 (JesusMtnez) Update droste to 0.6.0 #49 (JesusMtnez) Update cats to 1.5.0 #48 (JesusMtnez) Update Scala to 2.12.8 #47 (JesusMtnez) Move under higherkindness package #46 (JesusMtnez) Update sbt to 1.2.7 #41 (JesusMtnez) Update CHANGELOG.md #39 (juanpedromoreno) v0.0.2 (2018-11-22) Full Changelog 🚀 Features Update versions #23 (JesusMtnez) Closed issues: make sure tut compiles README #22 Optimizations: Optimize known coproducts to named types such as Option or Either #12 Update dependency org.typelevel:cats-core #6 Update dependency org.specs2:specs2-scalacheck:test #5 Update dependency org.specs2:specs2-core:test #4 Merged pull requests: Fixes Readme and tut settings #37 (juanpedromoreno) Revert 0.0.2 release #36 (juanpedromoreno) fix packaging #35 (pepegar) release 0.0.2 #34 (pepegar) change the organization to io.higherkindness #32 (pepegar) Readme compiles with printer instance #29 (rlmark) Migrate freestyle to mu #26 (JesusMtnez) Add some more tests #24 (pepegar) Readme skeuomorph example compiles and uses printSchema variable. #21 (rlmark) Use sbt-org-policies plugin #20 (franciscodr) Optimize known coproducts #19 (franciscodr) New approach for pretty printers #17 (pepegar) Adds new entries in gitignore file #16 (franciscodr) fix typo in optimizations documentation #15 (franciscodr) Frees &amp; Avro schema fixes #13 (pepegar) Swagger schema #10 (pepegar) fix microsite baseurl _again_ #9 (pepegar) Fixes Release Process #8 (juanpedromoreno) v0.0.1 (2018-08-02) Full Changelog Merged pull requests: release 0.0.1 version #7 (pepegar) CI Config #3 (juanpedromoreno) Migrate from turtles to droste #2 (pepegar) Docs #1 (pepegar) * This Changelog was automatically generated by github_changelog_generator"
    } ,      
    {
      "title": "Intro",
      "url": "/skeuomorph/docs/",
      "content": "Skeuomorph Skeuomorph is a library for transforming different schemas in Scala. It provides schema definitions as non-recursive ADTs, and transformations &amp; optimizations via recursion schemes. This library is primarily intended to be used at mu, but it’s completely independent from it, so anybody can use it. Skeuomorph depends heavily on cats and droste. Schemas Currently skeuomorph supports 3 different schemas: Avro Protobuf mu And provides conversions between them. This means that you can get a org.apache.avro.Schema value, and convert it to protobuf, for example. Or to a mu service description. Installation You can install skeuomorph as follows: libraryDependencies += \"io.higherkindness\" %% \"skeuomorph\" % \"0.2.2\" Examples Parsing an Avro schema and converting it into Scala code Given an Avro .avpr schema: val definition = \"\"\" { \"namespace\": \"example.avro\", \"type\": \"record\", \"name\": \"User\", \"fields\": [ { \"name\": \"name\", \"type\": \"string\" }, { \"name\": \"favorite_number\", \"type\": [ \"int\", \"null\" ] }, { \"name\": \"favorite_color\", \"type\": [ \"string\", \"null\" ] } ] } \"\"\" We can parse it, transform it into a Mu schema and then convert it into Scala code like this: import org.apache.avro.{Protocol =&gt; AvroProtocol, _} import higherkindness.skeuomorph.mu.Transform.transformAvro import higherkindness.skeuomorph.mu.MuF import higherkindness.skeuomorph.mu.codegen import higherkindness.skeuomorph.avro.AvroF.fromAvro import higherkindness.droste._ import higherkindness.droste.data._ import higherkindness.droste.data.Mu._ import cats.implicits._ import scala.meta._ val avroSchema: Schema = new Schema.Parser().parse(definition) val toMuSchema: Schema =&gt; Mu[MuF] = scheme.hylo(transformAvro[Mu[MuF]].algebra, fromAvro) val printSchemaAsScala: Mu[MuF] =&gt; Either[String, String] = codegen.schema(_).map(_.syntax) (toMuSchema &gt;&gt;&gt; println)(avroSchema) println(\"=====\") (toMuSchema &gt;&gt;&gt; printSchemaAsScala &gt;&gt;&gt; println)(avroSchema) It would generate the following output: Mu(TProduct(User,Some(example.avro),List(Field(name,Mu(TString()),None), Field(favorite_number,Mu(TCoproduct(NonEmptyList(Mu(TSimpleInt(_32)), Mu(TNull())))),None), Field(favorite_color,Mu(TCoproduct(NonEmptyList(Mu(TString()), Mu(TNull())))),None)),List(),List())) Right(final case class User(name: root.java.lang.String, favorite_number: root.scala.Option[root.scala.Int], favorite_color: root.scala.Option[root.java.lang.String])) Protobuf Parsing a proto3 .proto file and converting into Scala code Given the proto file below: user.proto syntax = \"proto3\"; package example.proto; message User { string name = 1; int64 favorite_number = 2; string favorite_color = 3; } We can parse it, transform it into a Mu protocol and then convert it into Scala code like this: import cats.effect.IO import cats.effect.unsafe.implicits.global import higherkindness.skeuomorph.mu import higherkindness.skeuomorph.mu.{CompressionType, MuF} import higherkindness.skeuomorph.protobuf._ import higherkindness.droste.data.Mu import higherkindness.droste.data.Mu._ import cats.implicits._ import scala.meta._ val source = ParseProto.ProtoSource(\"user.proto\", new java.io.File(\".\").getAbsolutePath ++ \"/microsite/protobuf\") val protobufProtocol: Protocol[Mu[ProtobufF]] = ParseProto.parseProto[IO, Mu[ProtobufF]].parse(source).unsafeRunSync() val toMuProtocol: Protocol[Mu[ProtobufF]] =&gt; mu.Protocol[Mu[MuF]] = { p: Protocol[Mu[ProtobufF]] =&gt; mu.Protocol.fromProtobufProto(CompressionType.Identity)(p) } val printProtocolAsScala: mu.Protocol[Mu[MuF]] =&gt; Either[String, String] = { p =&gt; val streamCtor: (Type, Type) =&gt; Type.Apply = { case (f, a) =&gt; t\"_root_.fs2.Stream[$f, $a]\" } mu.codegen.protocol(p, streamCtor).map(_.syntax) } (toMuProtocol &gt;&gt;&gt; println)(protobufProtocol) println(\"=====\") (toMuProtocol &gt;&gt;&gt; printProtocolAsScala &gt;&gt;&gt; println)(protobufProtocol) It would generate the following output: Protocol(Some(user),Some(example.proto),List(),List(Mu(TProduct(User,None,List(Field(name,Mu(TString()),Some(List(1))), Field(favorite_number,Mu(TProtobufInt(_64,List())),Some(List(2))), Field(favorite_color,Mu(TString()),Some(List(3)))),List(),List()))),List(),List()) Right(package example.proto import root.higherkindness.mu.rpc.protocol._ object user { final case class User(@root.pbdirect.pbIndex(1) name: root.java.lang.String, @root.pbdirect.pbIndex(2) favorite_number: root.scala.Long, @root.pbdirect.pbIndex(3) favorite_color: root.java.lang.String) }) Proto2 Incompatibility Please note that the design of Skeuomorph supports Proto3, and while it can still generate Scala code using Proto2, not all fields will be supported (most notably optional fields). For more details on this incompatibility, please see the schema notes. For this reason, we strongly encourage only using Skeuomorph with Proto3 schemas. Skeuomorph in the wild If you wish to add your library here please consider a PR to include it in the list below. Name Description mu purely functional library for building RPC endpoint based services with support for RPC and HTTP/2 Copyright Skeuomorph is designed and developed by 47 Degrees Copyright (C) 2018-2019 47 Degrees. http://47deg.com"
    } ,        
    {
      "title": "Optimizations",
      "url": "/skeuomorph/docs/optimizations/",
      "content": "Optimizations The technique we use to model recursive data throughout skeuomorph is called recursion schemes. Recursion schemes allows us to model our data as non recursive and substitute direct recursion by a call to a type parameter in the declaration. One of the techniques that we use from recursion schemes is microoptimizations. We’re able to transform ASTs by just describing the optimization we want as a function, and the library provides mechanisms to apply that function to the AST correctly. Let’s see namedTypes as an example: NamedTypes We found that when we wanted to render a schema to its string representation and the schema had nested product types, the rendering was not correct because it was printing the definition of the product everywhere: case class Product(field1: String, field2: case class OtherField()) ^---------------------^ // see how this is not valid scala code, it should be: case class Product(field1: String, field2: OtherField) We solve this by substituting nested product types by its name when they’re inside a product themselves. And we do this with the namedTypes combinator (in skeuomorph.mu.Optimize): def nestedNamedTypesTrans[T](implicit T: Basis[MuF, T]): Trans[MuF, MuF, T] = Trans { case TProduct(name, namespace, fields, np, nc) =&gt; def nameTypes(f: Field[T]): Field[T] = f.copy(tpe = namedTypes(T)(f.tpe)) TProduct[T](name, namespace, fields.map(nameTypes), np, nc) case other =&gt; other } def namedTypesTrans[T]: Trans[MuF, MuF, T] = Trans { case TProduct(name, ns, _, _, _) =&gt; TNamedType[T](ns.toList, name) case TSum(name, _) =&gt; TNamedType[T](Nil, name) case other =&gt; other } def namedTypes[T: Basis[MuF, *]]: T =&gt; T = scheme.cata(namedTypesTrans.algebra) def nestedNamedTypes[T: Basis[MuF, *]]: T =&gt; T = scheme.cata(nestedNamedTypesTrans.algebra) and then apply the namedTypes combinator to the AST: def ast = Mu(TNull[Mu[MuF]]()) val optimization = Optimize.namedTypes[Mu[MuF]] // optimization: Mu[MuF] =&gt; Mu[MuF] = &lt;function1&gt; optimization(ast) // res0: Mu[MuF] = Default(fmf = TNull())"
    } ,    
    {
      "title": "Schemas",
      "url": "/skeuomorph/docs/schemas/",
      "content": "Schemas Currently in skeuomorph there are schemas defined for different cases: Avro Protobuf mu Schema conversions from\\to Avro Protobuf mu Avro     avro.transCata(fromAvro) Protobuf     protobuf.transCata(fromProtobuf) mu mu.transCata(fromMu) mu.transCata(fromMu)   Schema Incompatibilities Currently, Skeuomorph only supports proto3 compliance, and the recommended approach when using skeuomorph with mu is to use proto3 for all gRPC communications. While it is still possible to generate valid Scala code from a proto2 spec, Skeuomorph will not generate case classes for optional fields. For example, given a hello.proto schema that looks like this: syntax = \"proto2\"; package src.main; message SayHelloRequest { optional string name = 1; } message SayHelloResponse { optional string message = 1; } service HelloWorldService { rpc SayHello (SayHelloRequest) returns (SayHelloResponse) {} } Skeuomorph (with mu and default plugin options) will generate the following Scala code: object hello { final case class SayHelloRequest(name: _root_.java.lang.String) final case class SayHelloResponse(message: _root_.java.lang.String) @service(Protobuf, compressionType = Identity, namespace = Some(\"src.main\")) trait HelloWorldService[F[_]] { def SayHello(req: _root_.src.main.hello.SayHelloRequest): F[_root_.src.main.hello.SayHelloResponse] } } As you can see, even though the parameters for the proto2 schema are option strings, the generated code is of type String."
    } ,      
  ];

  idx = lunr(function () {
    this.ref("title");
    this.field("content");

    docs.forEach(function (doc) {
      this.add(doc);
    }, this);
  });

  docs.forEach(function (doc) {
    docMap.set(doc.title, doc.url);
  });
}

// The onkeypress handler for search functionality
function searchOnKeyDown(e) {
  const keyCode = e.keyCode;
  const parent = e.target.parentElement;
  const isSearchBar = e.target.id === "search-bar";
  const isSearchResult = parent ? parent.id.startsWith("result-") : false;
  const isSearchBarOrResult = isSearchBar || isSearchResult;

  if (keyCode === 40 && isSearchBarOrResult) {
    // On 'down', try to navigate down the search results
    e.preventDefault();
    e.stopPropagation();
    selectDown(e);
  } else if (keyCode === 38 && isSearchBarOrResult) {
    // On 'up', try to navigate up the search results
    e.preventDefault();
    e.stopPropagation();
    selectUp(e);
  } else if (keyCode === 27 && isSearchBarOrResult) {
    // On 'ESC', close the search dropdown
    e.preventDefault();
    e.stopPropagation();
    closeDropdownSearch(e);
  }
}

// Search is only done on key-up so that the search terms are properly propagated
function searchOnKeyUp(e) {
  // Filter out up, down, esc keys
  const keyCode = e.keyCode;
  const cannotBe = [40, 38, 27];
  const isSearchBar = e.target.id === "search-bar";
  const keyIsNotWrong = !cannotBe.includes(keyCode);
  if (isSearchBar && keyIsNotWrong) {
    // Try to run a search
    runSearch(e);
  }
}

// Move the cursor up the search list
function selectUp(e) {
  if (e.target.parentElement.id.startsWith("result-")) {
    const index = parseInt(e.target.parentElement.id.substring(7));
    if (!isNaN(index) && (index > 0)) {
      const nextIndexStr = "result-" + (index - 1);
      const querySel = "li[id$='" + nextIndexStr + "'";
      const nextResult = document.querySelector(querySel);
      if (nextResult) {
        nextResult.firstChild.focus();
      }
    }
  }
}

// Move the cursor down the search list
function selectDown(e) {
  if (e.target.id === "search-bar") {
    const firstResult = document.querySelector("li[id$='result-0']");
    if (firstResult) {
      firstResult.firstChild.focus();
    }
  } else if (e.target.parentElement.id.startsWith("result-")) {
    const index = parseInt(e.target.parentElement.id.substring(7));
    if (!isNaN(index)) {
      const nextIndexStr = "result-" + (index + 1);
      const querySel = "li[id$='" + nextIndexStr + "'";
      const nextResult = document.querySelector(querySel);
      if (nextResult) {
        nextResult.firstChild.focus();
      }
    }
  }
}

// Search for whatever the user has typed so far
function runSearch(e) {
  if (e.target.value === "") {
    // On empty string, remove all search results
    // Otherwise this may show all results as everything is a "match"
    applySearchResults([]);
  } else {
    const tokens = e.target.value.split(" ");
    const moddedTokens = tokens.map(function (token) {
      // "*" + token + "*"
      return token;
    })
    const searchTerm = moddedTokens.join(" ");
    const searchResults = idx.search(searchTerm);
    const mapResults = searchResults.map(function (result) {
      const resultUrl = docMap.get(result.ref);
      return { name: result.ref, url: resultUrl };
    })

    applySearchResults(mapResults);
  }

}

// After a search, modify the search dropdown to contain the search results
function applySearchResults(results) {
  const dropdown = document.querySelector("div[id$='search-dropdown'] > .dropdown-content.show");
  if (dropdown) {
    //Remove each child
    while (dropdown.firstChild) {
      dropdown.removeChild(dropdown.firstChild);
    }

    //Add each result as an element in the list
    results.forEach(function (result, i) {
      const elem = document.createElement("li");
      elem.setAttribute("class", "dropdown-item");
      elem.setAttribute("id", "result-" + i);

      const elemLink = document.createElement("a");
      elemLink.setAttribute("title", result.name);
      elemLink.setAttribute("href", result.url);
      elemLink.setAttribute("class", "dropdown-item-link");

      const elemLinkText = document.createElement("span");
      elemLinkText.setAttribute("class", "dropdown-item-link-text");
      elemLinkText.innerHTML = result.name;

      elemLink.appendChild(elemLinkText);
      elem.appendChild(elemLink);
      dropdown.appendChild(elem);
    });
  }
}

// Close the dropdown if the user clicks (only) outside of it
function closeDropdownSearch(e) {
  // Check if where we're clicking is the search dropdown
  if (e.target.id !== "search-bar") {
    const dropdown = document.querySelector("div[id$='search-dropdown'] > .dropdown-content.show");
    if (dropdown) {
      dropdown.classList.remove("show");
      document.documentElement.removeEventListener("click", closeDropdownSearch);
    }
  }
}
