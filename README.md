
[comment]: # (Start Badges)

[![Build Status](https://travis-ci.org/frees-io/skeuomorph.svg?branch=master)](https://travis-ci.org/frees-io/skeuomorph) [![codecov.io](http://codecov.io/github/frees-io/skeuomorph/coverage.svg?branch=master)](http://codecov.io/github/frees-io/skeuomorph?branch=master) [![Maven Central](https://img.shields.io/badge/maven%20central-0.0.1-green.svg)](https://oss.sonatype.org/#nexus-search;gav~io.frees~skeuomorph*) [![Latest version](https://img.shields.io/badge/skeuomorph-0.0.1-green.svg)](https://index.scala-lang.org/frees-io/skeuomorph) [![License](https://img.shields.io/badge/license-Apache%202-blue.svg)](https://raw.githubusercontent.com/frees-io/skeuomorph/master/LICENSE) [![Join the chat at https://gitter.im/frees-io/skeuomorph](https://badges.gitter.im/frees-io/skeuomorph.svg)](https://gitter.im/frees-io/skeuomorph?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![GitHub Issues](https://img.shields.io/github/issues/frees-io/skeuomorph.svg)](https://github.com/frees-io/skeuomorph/issues)

[comment]: # (End Badges)

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
libraryDependencies += "io.higherkindness" %% "skeuomorph" % "0.0.1"
```

[comment]: # (End Replace)

## Examples

### parsing an avro schema and then converting it to scala:

```scala
scala> import org.apache.avro._
<console>:12: warning: Unused import
       import org.apache.avro._
                              ^
import org.apache.avro._

scala> import skeuomorph.mu.Transform.transformAvro
<console>:10: warning: Unused import
       import org.apache.avro._
                              ^
<console>:15: warning: Unused import
       import skeuomorph.mu.Transform.transformAvro
                                      ^
import skeuomorph.mu.Transform.transformAvro

scala> import skeuomorph.mu.MuF
<console>:10: warning: Unused import
       import org.apache.avro._
                              ^
<console>:12: warning: Unused import
       import skeuomorph.mu.Transform.transformAvro
                                      ^
<console>:16: warning: Unused import
       import skeuomorph.mu.MuF
                            ^
import skeuomorph.mu.MuF

scala> import skeuomorph.mu.print
<console>:10: warning: Unused import
       import org.apache.avro._
                              ^
<console>:12: warning: Unused import
       import skeuomorph.mu.Transform.transformAvro
                                      ^
<console>:13: warning: Unused import
       import skeuomorph.mu.MuF
                            ^
<console>:17: warning: Unused import
       import skeuomorph.mu.print
                            ^
import skeuomorph.mu.print

scala> import skeuomorph.avro.AvroF.fromAvro
<console>:10: warning: Unused import
       import org.apache.avro._
                              ^
<console>:12: warning: Unused import
       import skeuomorph.mu.Transform.transformAvro
                                      ^
<console>:13: warning: Unused import
       import skeuomorph.mu.MuF
                            ^
<console>:14: warning: Unused import
       import skeuomorph.mu.print
                            ^
<console>:18: warning: Unused import
       import skeuomorph.avro.AvroF.fromAvro
                                    ^
import skeuomorph.avro.AvroF.fromAvro

scala> import qq.droste._
<console>:10: warning: Unused import
       import org.apache.avro._
                              ^
<console>:12: warning: Unused import
       import skeuomorph.mu.Transform.transformAvro
                                      ^
<console>:13: warning: Unused import
       import skeuomorph.mu.MuF
                            ^
<console>:14: warning: Unused import
       import skeuomorph.mu.print
                            ^
<console>:15: warning: Unused import
       import skeuomorph.avro.AvroF.fromAvro
                                    ^
<console>:19: warning: Unused import
       import qq.droste._
                        ^
import qq.droste._

scala> import qq.droste.data._
<console>:10: warning: Unused import
       import org.apache.avro._
                              ^
<console>:12: warning: Unused import
       import skeuomorph.mu.Transform.transformAvro
                                      ^
<console>:13: warning: Unused import
       import skeuomorph.mu.MuF
                            ^
<console>:14: warning: Unused import
       import skeuomorph.mu.print
                            ^
<console>:15: warning: Unused import
       import skeuomorph.avro.AvroF.fromAvro
                                    ^
<console>:17: warning: Unused import
       import qq.droste._
                        ^
<console>:22: warning: Unused import
       import qq.droste.data._
                             ^
import qq.droste.data._

scala> import qq.droste.data.Mu._
<console>:10: warning: Unused import
       import org.apache.avro._
                              ^
<console>:12: warning: Unused import
       import skeuomorph.mu.Transform.transformAvro
                                      ^
<console>:13: warning: Unused import
       import skeuomorph.mu.MuF
                            ^
<console>:14: warning: Unused import
       import skeuomorph.mu.print
                            ^
<console>:15: warning: Unused import
       import skeuomorph.avro.AvroF.fromAvro
                                    ^
<console>:17: warning: Unused import
       import qq.droste._
                        ^
<console>:20: warning: Unused import
       import qq.droste.data._
                             ^
<console>:25: warning: Unused import
       import qq.droste.data.Mu._
                                ^
import qq.droste.data.Mu._

scala> import cats.implicits._
<console>:10: warning: Unused import
       import org.apache.avro._
                              ^
<console>:12: warning: Unused import
       import skeuomorph.mu.Transform.transformAvro
                                      ^
<console>:13: warning: Unused import
       import skeuomorph.mu.MuF
                            ^
<console>:14: warning: Unused import
       import skeuomorph.mu.print
                            ^
<console>:15: warning: Unused import
       import skeuomorph.avro.AvroF.fromAvro
                                    ^
<console>:17: warning: Unused import
       import qq.droste._
                        ^
<console>:20: warning: Unused import
       import qq.droste.data._
                             ^
<console>:23: warning: Unused import
       import qq.droste.data.Mu._
                                ^
<console>:28: warning: Unused import
       import cats.implicits._
                             ^
import cats.implicits._

scala> val definition = """
     | {
     |   "namespace": "example.avro",
     |   "type": "record",
     |   "name": "User",
     |   "fields": [
     |     {
     |       "name": "name",
     |       "type": "string"
     |     },
     |     {
     |       "name": "favorite_number",
     |       "type": [
     |         "int",
     |         "null"
     |       ]
     |     },
     |     {
     |       "name": "favorite_color",
     |       "type": [
     |         "string",
     |         "null"
     |       ]
     |     }
     |   ]
     | }
     |   """
<console>:10: warning: Unused import
       import org.apache.avro._
                              ^
<console>:12: warning: Unused import
       import skeuomorph.mu.Transform.transformAvro
                                      ^
<console>:13: warning: Unused import
       import skeuomorph.mu.MuF
                            ^
<console>:14: warning: Unused import
       import skeuomorph.mu.print
                            ^
<console>:15: warning: Unused import
       import skeuomorph.avro.AvroF.fromAvro
                                    ^
<console>:17: warning: Unused import
       import qq.droste._
                        ^
<console>:20: warning: Unused import
       import qq.droste.data._
                             ^
<console>:23: warning: Unused import
       import qq.droste.data.Mu._
                                ^
<console>:26: warning: Unused import
       import cats.implicits._
                             ^
definition: String =
"
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
  "

scala> val avroSchema: Schema = new Schema.Parser().parse(definition)
<console>:12: warning: Unused import
       import skeuomorph.mu.Transform.transformAvro
                                      ^
<console>:13: warning: Unused import
       import skeuomorph.mu.MuF
                            ^
<console>:14: warning: Unused import
       import skeuomorph.mu.print
                            ^
<console>:15: warning: Unused import
       import skeuomorph.avro.AvroF.fromAvro
                                    ^
<console>:17: warning: Unused import
       import qq.droste._
                        ^
<console>:20: warning: Unused import
       import qq.droste.data._
                             ^
<console>:23: warning: Unused import
       import qq.droste.data.Mu._
                                ^
<console>:26: warning: Unused import
       import cats.implicits._
                             ^
avroSchema: org.apache.avro.Schema = {"type":"record","name":"User","namespace":"example.avro","fields":[{"name":"name","type":"string"},{"name":"favorite_number","type":["int","null"]},{"name":"favorite_color","type":["string","null"]}]}

scala> val parseAvro: Schema => Mu[MuF] =
     |   scheme.hylo(transformAvro[Mu[MuF]].algebra, fromAvro)
<console>:14: warning: Unused import
       import skeuomorph.mu.print
                            ^
<console>:26: warning: Unused import
       import cats.implicits._
                             ^
parseAvro: org.apache.avro.Schema => qq.droste.data.Mu[skeuomorph.mu.MuF] = <function1>

scala> val printAsScala: Mu[MuF] => String = 
     |   print.schema.print _
<console>:10: warning: Unused import
       import org.apache.avro._
                              ^
<console>:12: warning: Unused import
       import skeuomorph.mu.Transform.transformAvro
                                      ^
<console>:15: warning: Unused import
       import skeuomorph.avro.AvroF.fromAvro
                                    ^
<console>:17: warning: Unused import
       import qq.droste._
                        ^
<console>:26: warning: Unused import
       import cats.implicits._
                             ^
printAsScala: qq.droste.data.Mu[skeuomorph.mu.MuF] => String = $$Lambda$6007/667054060@3ae4c39f

scala> (parseAvro >>> println)(avroSchema)
<console>:10: warning: Unused import
       import org.apache.avro._
                              ^
<console>:12: warning: Unused import
       import skeuomorph.mu.Transform.transformAvro
                                      ^
<console>:13: warning: Unused import
       import skeuomorph.mu.MuF
                            ^
<console>:14: warning: Unused import
       import skeuomorph.mu.print
                            ^
<console>:15: warning: Unused import
       import skeuomorph.avro.AvroF.fromAvro
                                    ^
<console>:17: warning: Unused import
       import qq.droste._
                        ^
<console>:20: warning: Unused import
       import qq.droste.data._
                             ^
<console>:23: warning: Unused import
       import qq.droste.data.Mu._
                                ^
Mu(TProduct(User,List(Field(name,Mu(TString())), Field(favorite_number,Mu(TCoproduct(NonEmptyList(Mu(TInt()), Mu(TNull()))))), Field(favorite_color,Mu(TCoproduct(NonEmptyList(Mu(TString()), Mu(TNull()))))))))

scala> (printAsScala >>> println)(parseAvro(avroSchema))
<console>:10: warning: Unused import
       import org.apache.avro._
                              ^
<console>:12: warning: Unused import
       import skeuomorph.mu.Transform.transformAvro
                                      ^
<console>:13: warning: Unused import
       import skeuomorph.mu.MuF
                            ^
<console>:14: warning: Unused import
       import skeuomorph.mu.print
                            ^
<console>:15: warning: Unused import
       import skeuomorph.avro.AvroF.fromAvro
                                    ^
<console>:17: warning: Unused import
       import qq.droste._
                        ^
<console>:20: warning: Unused import
       import qq.droste.data._
                             ^
<console>:23: warning: Unused import
       import qq.droste.data.Mu._
                                ^
@message final case class User(name: String, favorite_number: Cop[Int :: Null:: TNil], favorite_color: Cop[String :: Null:: TNil])
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

Copyright (C) 2018 47 Degrees. <http://47deg.com>

[comment]: # (End Copyright)
