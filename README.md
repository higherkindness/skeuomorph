
[![codecov.io](http://codecov.io/gh/higherkindness/skeuomorph/branch/master/graph/badge.svg)](http://codecov.io/gh/higherkindness/skeuomorph) [![Maven Central](https://img.shields.io/badge/maven%20central-0.0.23-green.svg)](https://oss.sonatype.org/#nexus-search;gav~io.higherkindness~skeuomorph*) [![Latest version](https://img.shields.io/badge/skeuomorph-0.0.22-green.svg)](https://index.scala-lang.org/higherkindness/skeuomorph) [![License](https://img.shields.io/badge/license-Apache%202-blue.svg)](https://raw.githubusercontent.com/higherkindness/skeuomorph/master/LICENSE) [![Join the chat at https://gitter.im/higherkindness/skeuomorph](https://badges.gitter.im/higherkindness/skeuomorph.svg)](https://gitter.im/higherkindness/skeuomorph?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![GitHub Issues](https://img.shields.io/github/issues/higherkindness/skeuomorph.svg)](https://github.com/higherkindness/skeuomorph/issues)

# skeuomorph

Skeuomorph is a library for transforming different schemas in Scala.
It provides schema definitions as non-recursive ADTs, and
transformations & optimizations via recursion schemes.

This library is primarily intended to be used at [mu][], but
it's completely independent from it, so anybody can use it.

Skeuomorph depends heavily on [cats][] and [droste][].

More information can be found at the [microsite][].

## Documentation

For installing this library, add the following line to your `build.sbt` file:

```scala
libraryDependencies += "io.higherkindness" %% "skeuomorph" % "0.0.23"
```

The full documentation is available at the [skeuomorph](https://higherkindness.io/skeuomorph/) site.

## NOTICE

The following files `api-with-examples.yaml`, `petstore-expanded.yaml`, `callback-example.yaml`, `petstore.yaml`, `link-example.yaml` and `uspto.yaml` inside the folder (`test/resources/openapi/yaml`) were copied from [**OpenAPI Specification**](https://github.com/OAI/OpenAPI-Specification/) project under the terms of the licence [*Apache License Version 2.0, January 2004*](https://github.com/OAI/OpenAPI-Specification/blob/master/LICENSE).

## Skeuomorph in the wild

If you wish to add your library here please consider a PR to include
it in the list below.

| **Name**                                       | **Description**                                                                                    |
|------------------------------------------------|----------------------------------------------------------------------------------------------------|
| [**mu**](https://higherkindness.github.io/mu/) | purely functional library for building RPC endpoint based services with support for RPC and HTTP/2 |

[Avro]: https://avro.apache.org/
[Protobuf]: https://developers.google.com/protocol-buffers/
[mu]: https://higherkindness.github.io/mu/
[cats]: https://typelevel.org/cats
[droste]: https://github.com/andyscott/droste
[microsite]: https://higherkindness.io/skeuomorph/

# Copyright

skeuomorph is designed and developed by 47 Degrees Open Source

Copyright (C)  2018-2020 47 Degrees Open Source <https://www.47deg.com>
