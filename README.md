
[comment]: # (Start Badges)

[![Build Status](https://travis-ci.org/higherkindness/skeuomorph.svg?branch=master)](https://travis-ci.org/higherkindness/skeuomorph) [![codecov.io](http://codecov.io/gh/higherkindness/skeuomorph/branch/master/graph/badge.svg)](http://codecov.io/gh/higherkindness/skeuomorph) [![Maven Central](https://img.shields.io/badge/maven%20central-0.0.9.1-green.svg)](https://oss.sonatype.org/#nexus-search;gav~io.higherkindness~skeuomorph*) [![Latest version](https://img.shields.io/badge/skeuomorph-0.0.9.1-green.svg)](https://index.scala-lang.org/higherkindness/skeuomorph) [![License](https://img.shields.io/badge/license-Apache%202-blue.svg)](https://raw.githubusercontent.com/higherkindness/skeuomorph/master/LICENSE) [![Join the chat at https://gitter.im/higherkindness/skeuomorph](https://badges.gitter.im/higherkindness/skeuomorph.svg)](https://gitter.im/higherkindness/skeuomorph?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![GitHub Issues](https://img.shields.io/github/issues/higherkindness/skeuomorph.svg)](https://github.com/higherkindness/skeuomorph/issues)

[comment]: # (End Badges)

# Skeuomorph

Skeuomorph is a library for transforming different schemas in Scala.
It provides schema definitions as non-recursive ADTs, and
transformations & optimizations via recursion schemes.

This library is primarily intended to be used at [mu][], but
it's completely independent from it, so anybody can use it.

Skeuomorph depends heavily on [cats][] and [droste][].

## NOTICE
The following files `api-with-examples.yaml`, `petstore-expanded.yaml`, `callback-example.yaml`, `petstore.yaml`, `link-example.yaml` and `uspto.yaml` inside the folder (`test/resources/openapi/yaml`) were copied from [**OpenAPI Specification**](https://github.com/OAI/OpenAPI-Specification/) project under the terms of the licence [*Apache License Version 2.0, January 2004*](https://github.com/OAI/OpenAPI-Specification/blob/master/LICENSE).

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

```scala
import org.apache.avro._
import higherkindness.skeuomorph.mu.Transform.transformAvro
import higherkindness.skeuomorph.mu.MuF
import higherkindness.skeuomorph.mu.print
import higherkindness.skeuomorph.avro.AvroF.fromAvro
import qq.droste._
import qq.droste.data._
import qq.droste.data.Mu._
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
  print.schema.print _
(parseAvro >>> println)(avroSchema)
(printAsScala >>> println)(parseAvro(avroSchema))
```

```
Mu(TProduct(User,List(Field(name,Mu(TString())), Field(favorite_number,Mu(TCoproduct(NonEmptyList(Mu(TInt()), Mu(TNull()))))), Field(favorite_color,Mu(TCoproduct(NonEmptyList(Mu(TString()), Mu(TNull()))))))))
@message final case class User(name: String, favorite_number: Cop[Int :: Null:: TNil], favorite_color: Cop[String :: Null:: TNil])
```


## Protobuf

### Parsing nested `.proto` files and converting into Scala code

Given these proto files below:

_author.proto_

```protobuf
syntax = "proto3";
package com.acme;

message Author {
    string name = 1;
    string nick = 2;
}
```

_book.proto_

```protobuf
syntax = "proto3";
package com.acme;
import "author.proto";

message Book {
    reserved 4, 8;
    reserved 12 to 15;
    int64 isbn = 1;
    string title = 2;
    repeated Author author = 3;
    BindingType binding_type = 9;
}

message GetBookRequest {
    int64 isbn = 1;
}

message GetBookViaAuthor {
    Author author = 1;
}

message BookStore {
    string name = 1;
    map<int64, string> books = 2;
    repeated Genre genres = 3;

    oneof payment_method {
        int64 credit_card_number = 4;
        int32 cash = 5;
        string iou_note = 6;
        Book barter = 7;
    }
}

enum Genre {
    option allow_alias = true;
    UNKNOWN = 0;
    SCIENCE_FICTION = 1;
    SPECULATIVE_FICTION = 1;
    POETRY = 2;
    SCI_FI = 1;
}

enum BindingType {
    HARDCOVER = 0;
    PAPERBACK = 1;
}

service BookService {
    rpc GetBook (GetBookRequest) returns (Book) {}
    rpc GetBooksViaAuthor (GetBookViaAuthor) returns (stream Book) {}
    rpc GetGreatestBook (stream GetBookRequest) returns (Book) {}
    rpc GetBooks (stream GetBookRequest) returns (stream Book) {}
}
```

We can parse and convert them into Scala code as:

```scala
val source = ParseProto.ProtoSource("book.proto", "resources")
val nativeDescriptor: NativeFile = parseProto[IO].parse(source).unsafeRunSync()

val parseNative: NativeFile => Protocol[Mu[ProtobufF]] = Protocol.fromProto(_)

val parseProtocol: Protocol[Mu[ProtobufF]] => mu.Protocol[Mu[MuF]] =
{ p: Protocol[Mu[ProtobufF]] => mu.Protocol.fromProtobufProto(p) }

val printProtocol: mu.Protocol[Mu[MuF]] => String = { p: mu.Protocol[Mu[MuF]] =>
  higherkindness.skeuomorph.mu.print.proto.print(p)
}

(parseNative andThen parseProtocol andThen printProtocol)(nativeDescriptor)
```

It would generate:

```scala
package com.acme

object book {

  @message final case class Author(name: String, nick: String)
  @message final case class Book(isbn: Long, title: String, author: List[Author], binding_type: BindingType)
  @message final case class GetBookRequest(isbn: Long)
  @message final case class GetBookViaAuthor(author: Author)
  @message final case class BookStore(name: String, books: Map[Long, String], genres: List[Genre], payment_method: Cop[Long :: Int :: String :: Book:: TNil])

  sealed trait Genre
  object Genre {
    case object UNKNOWN extends Genre
    case object SCIENCE_FICTION extends Genre
    case object POETRY extends Genre
  }

  sealed trait BindingType
  object BindingType {
    case object HARDCOVER extends BindingType
    case object PAPERBACK extends BindingType
  }

  @service(Protobuf) trait BookService[F[_]] {
    def GetBook(req: GetBookRequest): F[Book]
    def GetBooksViaAuthor(req: GetBookViaAuthor): Stream[F, Book]
    def GetGreatestBook(req: Stream[F, GetBookRequest]): F[Book]
    def GetBooks(req: Stream[F, GetBookRequest]): Stream[F, Book]
  }
}
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
[cats]: http://typelevel.org/cats
[droste]: http://github.com/andyscott/droste

[comment]: # (Start Copyright)
# Copyright

Skeuomorph is designed and developed by 47 Degrees

Copyright (C) 2018-2019 47 Degrees. <http://47deg.com>

[comment]: # (End Copyright)