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
libraryDependencies += "io.higherkindness" %% "skeuomorph" % "@VERSION@"
```

[comment]: # (End Replace)

## Examples

### Parsing an Avro schema and converting it into Scala code

Given an Avro schema:

```scala mdoc:silent
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
```

We can parse it, transform it into a Mu schema and then convert it into Scala code like this:

```scala mdoc:silent
import org.apache.avro.{Protocol => AvroProtocol, _}
import higherkindness.skeuomorph.mu.Transform.transformAvro
import higherkindness.skeuomorph.mu.MuF
import higherkindness.skeuomorph.mu.codegen
import higherkindness.skeuomorph.avro.AvroF.fromAvro
import higherkindness.droste._
import higherkindness.droste.data._
import higherkindness.droste.data.Mu._
import cats.implicits._
import scala.meta._

val avroSchema: Schema = new Schema.Parser().parse(definition)

val toMuSchema: Schema => Mu[MuF] =
  scheme.hylo(transformAvro[Mu[MuF]].algebra, fromAvro)

val printSchemaAsScala: Mu[MuF] => Either[String, String] =
  codegen.schema(_).map(_.syntax)

(toMuSchema >>> println)(avroSchema)
println("=====")
(toMuSchema >>> printSchemaAsScala >>> println)(avroSchema)
```

It would generate the following output:

```scala mdoc:passthrough
println("```scala")
(toMuSchema >>> println)(avroSchema)
println("=====")
(toMuSchema >>> printSchemaAsScala >>> println)(avroSchema)
println("```")
```

## Protobuf

### Parsing a `.proto` file and converting into Scala code

Given the proto file below:

_user.proto_

```protobuf
syntax = "proto3";
package example.proto;

message User {
    string name = 1;
    int64 favorite_number = 2;
    string favorite_color = 3;
}
```

We can parse it, transform it into a Mu protocol and then convert it into Scala code like this:

```scala mdoc:silent
import cats.effect.IO
import higherkindness.skeuomorph.mu
import higherkindness.skeuomorph.mu.{CompressionType, MuF}
import higherkindness.skeuomorph.protobuf._
import higherkindness.droste.data.Mu
import higherkindness.droste.data.Mu._
import cats.implicits._
import scala.meta._

val source = ParseProto.ProtoSource("user.proto", new java.io.File(".").getAbsolutePath ++ "/docs/protobuf")

val protobufProtocol: Protocol[Mu[ProtobufF]] = ParseProto.parseProto[IO, Mu[ProtobufF]].parse(source).unsafeRunSync()

val toMuProtocol: Protocol[Mu[ProtobufF]] => mu.Protocol[Mu[MuF]] = { p: Protocol[Mu[ProtobufF]] =>
  mu.Protocol.fromProtobufProto(CompressionType.Identity, true)(p)
}

val printProtocolAsScala: mu.Protocol[Mu[MuF]] => Either[String, String] = { p =>
  val streamCtor: (Type, Type) => Type.Apply = {
    case (f, a) => t"_root_.fs2.Stream[$f, $a]"
  }
  mu.codegen.protocol(p, streamCtor).map(_.syntax)
}

(toMuProtocol >>> println)(protobufProtocol)
println("=====")
(toMuProtocol >>> printProtocolAsScala >>> println)(protobufProtocol)
```

It would generate the following output:


```scala mdoc:passthrough
println("```scala")
(toMuProtocol >>> println)(protobufProtocol)
println("=====")
(toMuProtocol >>> printProtocolAsScala >>> println)(protobufProtocol)
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