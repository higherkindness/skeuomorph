
[comment]: # (Start Badges)

[![Build Status](https://travis-ci.org/frees-io/skeuomorph.svg?branch=master)](https://travis-ci.org/frees-io/skeuomorph) [![codecov.io](http://codecov.io/github/frees-io/skeuomorph/coverage.svg?branch=master)](http://codecov.io/github/frees-io/skeuomorph?branch=master) [![Maven Central](https://img.shields.io/badge/maven%20central-0.0.1-green.svg)](https://oss.sonatype.org/#nexus-search;gav~io.frees~skeuomorph*) [![Latest version](https://img.shields.io/badge/skeuomorph-0.0.1-green.svg)](https://index.scala-lang.org/frees-io/skeuomorph) [![License](https://img.shields.io/badge/license-Apache%202-blue.svg)](https://raw.githubusercontent.com/frees-io/skeuomorph/master/LICENSE) [![Join the chat at https://gitter.im/frees-io/skeuomorph](https://badges.gitter.im/frees-io/skeuomorph.svg)](https://gitter.im/frees-io/skeuomorph?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![GitHub Issues](https://img.shields.io/github/issues/frees-io/skeuomorph.svg)](https://github.com/frees-io/skeuomorph/issues)

[comment]: # (End Badges)

# Skeuomorph

Skeuomorph is a library for transforming different schemas in Scala.
It provides schema definitions as non-recursive ADTs, and
transformations & optimizations via recursion schemes.

This library is primarily intended to be used at [mu-rpc][], but
it's completely independent from it, so anybody can use it.

Skeuomorph depends heavily on [cats][] and [droste][].

## Schemas

Currently skeuomorph supports 3 different schemas:
- [Avro][]
- [Protobuf][]
- [mu-rpc][]

And provides conversions between them.  This means that you can get a
`org.apache.avro.Schema` value, and convert it to protobuf, for
example.  Or to a mu service description.


## Installation

You can install skeuomorph as follows:

[comment]: # (Start Replace)

```scala
libraryDependencies += "io.higherkindness" %% "skeuomorph" % "0.0.1"
```

[comment]: # (End Replace)

## Examples

### parsing an avro schema and then converting it to scala:

```tut

import org.apache.avro._
import skeuomorph._
import skeuomorph.mu.Transform.transformAvro
import skeuomorph.mu.MuF
import skeuomorph.mu.MuF.render
import skeuomorph.avro.AvroF.fromAvro
import qq.droste._
import qq.droste.data._
import qq.droste.implicits._
import cats.implicits._


val definition = """
{
  "namespace": "example.avro",
  "type": "record",
  "name": "User",
  "fields": [
    {
      "name": "name",
      "type": "string"
    },
    {
      "name": "favorite_number",
      "type": [
        "int",
        "null"
      ]
    },
    {
      "name": "favorite_color",
      "type": [
        "string",
        "null"
      ]
    }
  ]
}
  """

val schema: Schema = new Schema.Parser().parse(definition)

val parseAvro: Schema => Mu[MuF] =
  scheme.hylo(transformAvro[Mu[MuF]].algebra, fromAvro)
val printSchema: Mu[MuF] => String =
  scheme.cata(render)

(parseAvro >>> print)(schema)
(printSchema >>> print)(parseAvro(schema))
```


## Skeuomorph in the wild

If you wish to add your library here please consider a PR to include
it in the list below.

| **Name**                                           | **Description**                                                                                    |
|----------------------------------------------------|----------------------------------------------------------------------------------------------------|
| [**mu-rpc**](https://higherkindness.github.io/mu/) | purely functional library for building RPC endpoint based services with support for RPC and HTTP/2 |

[Avro]: https://avro.apache.org/
[Protobuf]: https://developers.google.com/protocol-buffers/
[mu-rpc]: https://higherkindness.github.io/mu/
[cats]: http://typelevel.org/cats
[droste]: http://github.com/andyscott/droste

[comment]: # (Start Copyright)
# Copyright

Skeuomorph is designed and developed by 47 Degrees

Copyright (C) 2018 47 Degrees. <http://47deg.com>

[comment]: # (End Copyright)
