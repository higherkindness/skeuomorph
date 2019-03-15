/*
 * Copyright 2018-2019 47 Degrees, LLC. <http://www.47deg.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// package higherkindness.skeuomorph.avro

// import higherkindness.skeuomorph.uast.types._
// import higherkindness.skeuomorph.uast.derivation._
// import higherkindness.skeuomorph.compdata.Ann
// import higherkindness.skeuomorph.instances._

// import org.apache.avro.Schema
// import org.scalacheck._
// import org.specs2._
// import qq.droste._

// import scala.collection.JavaConverters._

// class AvroSpec extends Specification with ScalaCheck {

//   def is = s2"""
//   Avro Schema

//   It should be possible to create a Schema from org.apache.Schema. $convertSchema

//   It should be possible to create a Protocol from org.apache.Protocol. $convertProtocol
//   """

//   def convertSchema = Prop.forAll { (schema: Schema) =>
//     val test = scheme.hylo(checkSchema(schema), Type.fromAvro)

//     test(schema)
//   }

//   def convertProtocol = todo

//   def checkSchema(sch: Schema): Algebra[Type, Boolean] = Algebra {
//     case InjNull(_)      => sch.getType should_== Schema.Type.NULL
//     case InjBoolean(_)   => sch.getType should_== Schema.Type.BOOLEAN
//     case InjInt(_)       => sch.getType should_== Schema.Type.INT
//     case InjLong(_)      => sch.getType should_== Schema.Type.LONG
//     case InjFloat(_)     => sch.getType should_== Schema.Type.FLOAT
//     case InjDouble(_)    => sch.getType should_== Schema.Type.DOUBLE
//     case InjByteArray(_) => sch.getType should_== Schema.Type.BYTES
//     case InjString(_)    => sch.getType should_== Schema.Type.STRING

//     case InjNamedType(_)                      => false
//     case InjList(_)                           => sch.getType should_== Schema.Type.ARRAY
//     case InjMap(_)                            => sch.getType should_== Schema.Type.MAP
//     case InjAvroRecord(Ann(TRecord(_, _), _)) =>
//       // (sch.getName should_== name)
//       //   .and(sch.getFields.asScala.toList.map(f => (f.name, f.schema.getType)) should_== fields.map(f =>
//       //     (f.name, f.doc.getOrElse(""))))
//       true

//     case InjEnum(_)  => true
//     case InjUnion(_) => true
//     case InjFixed(_) => true
//   }
// }
