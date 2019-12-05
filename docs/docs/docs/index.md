---
layout: docs
title: Intro
permalink: docs/
---


# Skeuomorph

Skeuomorph is a library for transforming different schemas in Scala.
It provides schema definitions as non-recursive ADTs, and
transformations & optimizations via recursion schemes.

This library is primarily intended to be used at [mu][], but
it's completely independent from it, so anybody can use it.

Skeuomorph depends heavily on [cats][] and [droste][].

## Schemas

Currently skeuomorph supports 3 different schemas:
- [Avro][]
- [Protobuf][]
- [mu][]

And provides conversions between them.  This means that you can get a
`org.apache.avro.Schema` value, and convert it to protobuf, for
example.  Or to a mu service description.


## Installation

You can install skeuomorph as follows:

[comment]: # (Start Replace)

```scala
libraryDependencies += "io.higherkindness" %% "skeuomorph" % "0.0.9.1"
```

[comment]: # (End Replace)

## Examples

### parsing an avro schema and then converting it to scala:

```scala mdoc:silent
import org.apache.avro.{Protocol => AvroProtocol, _}
import higherkindness.skeuomorph.mu.Transform.transformAvro
import higherkindness.skeuomorph.mu.MuF
import higherkindness.skeuomorph.mu.{print => muprint}
import higherkindness.skeuomorph.avro.AvroF.fromAvro
import higherkindness.droste._
import higherkindness.droste.data._
import higherkindness.droste.data.Mu._
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

val avroSchema: Schema = new Schema.Parser().parse(definition)

val parseAvro: Schema => Mu[MuF] =
  scheme.hylo(transformAvro[Mu[MuF]].algebra, fromAvro)
val printAsScala: Mu[MuF] => String =
  muprint.schema.print _
(parseAvro >>> println)(avroSchema)
(printAsScala >>> println)(parseAvro(avroSchema))
```

```scala mdoc:passthrough
println("```scala")
(parseAvro >>> println)(avroSchema)
(printAsScala >>> println)(parseAvro(avroSchema))
println("```")
```


## Protobuf

### Parsing `.proto` file and converting into Scala code

Given these proto file below:

_user.proto_

```protobuf
syntax = "proto3";
package com.acme;

message User {
    string name = 1;
    int64 favorite_number = 2;
    string favorite_color = 3;
}
```


We can parse and convert them into Scala code as:

```scala mdoc:silent
  import cats.effect.IO
  import higherkindness.skeuomorph.mu
  import mu.{CompressionType, MuF}
  import higherkindness.skeuomorph.protobuf._
  import higherkindness.droste.data.Mu
  import Mu._

  val source = ParseProto.ProtoSource("user.proto", new java.io.File(".").getAbsolutePath ++ "/docs/protobuf")

  val protobufProtocol: Protocol[Mu[ProtobufF]] = ParseProto.parseProto[IO, Mu[ProtobufF]].parse(source).unsafeRunSync()

  val parseProtocol: Protocol[Mu[ProtobufF]] => mu.Protocol[Mu[MuF]] = { p: Protocol[Mu[ProtobufF]] =>
    mu.Protocol.fromProtobufProto(CompressionType.Identity, true)(p)
  }

  val printProtocol: mu.Protocol[Mu[MuF]] => String = { p: mu.Protocol[Mu[MuF]] =>
    higherkindness.skeuomorph.mu.print.proto.print(p)
  }

 (parseProtocol andThen printProtocol)(protobufProtocol)
```

It would generate:


```scala mdoc:passthrough
println("```scala")
(parseProtocol andThen printProtocol andThen println)(protobufProtocol)
println("```")
```

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

[comment]: # (Start Copyright)
# Copyright

Skeuomorph is designed and developed by 47 Degrees

Copyright (C) 2018-2019 47 Degrees. <http://47deg.com>

[comment]: # (End Copyright)
