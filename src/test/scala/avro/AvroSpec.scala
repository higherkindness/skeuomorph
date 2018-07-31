/*
 * Copyright 2018 47 Degrees, LLC. <http://www.47deg.com>
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

package skeuomorph
package avro

import scala.collection.JavaConverters._

import org.specs2._
import org.scalacheck._
import org.apache.avro.{Schema => AvroSchema}

import turtles.Algebra
import turtles.data.Mu
import turtles.implicits._

class AvroSpec extends Specification with ScalaCheck {

  import instances._

  def is = s2"""
  Avro Schema

  It should be possible to create a Schema from org.apache.avro.Schema. $convertSchema

  It should be possible to create a Protocol from org.apache.avro.Protocol. $convertProtocol
  """

  def convertSchema = Prop.forAll { (schema: AvroSchema) =>
    schema
      .ana[Mu[Schema]](util.fromAvro)
      .cata(checkSchema(schema))
  }

  def convertProtocol = todo

  def checkSchema(sch: AvroSchema): Algebra[Schema, Boolean] = {
    case Schema.TNull()    => sch.getType should_== AvroSchema.Type.NULL
    case Schema.TBoolean() => sch.getType should_== AvroSchema.Type.BOOLEAN
    case Schema.TInt()     => sch.getType should_== AvroSchema.Type.INT
    case Schema.TLong()    => sch.getType should_== AvroSchema.Type.LONG
    case Schema.TFloat()   => sch.getType should_== AvroSchema.Type.FLOAT
    case Schema.TDouble()  => sch.getType should_== AvroSchema.Type.DOUBLE
    case Schema.TBytes()   => sch.getType should_== AvroSchema.Type.BYTES
    case Schema.TString()  => sch.getType should_== AvroSchema.Type.STRING

    case Schema.TNamedType(_) => false
    case Schema.TArray(_)     => sch.getType should_== AvroSchema.Type.ARRAY
    case Schema.TMap(_)       => sch.getType should_== AvroSchema.Type.MAP
    case Schema.TRecord(name, namespace, _, doc, fields) =>
      (sch.getName should_== name)
        .and(sch.getNamespace should_== namespace.getOrElse(""))
        .and(sch.getDoc should_== doc.getOrElse(""))
        .and(sch.getFields.asScala.toList.map(f => (f.name, f.doc)) should_== fields.map(f =>
          (f.name, f.doc.getOrElse(""))))

    case Schema.TEnum(_, _, _, _, _) => true
    case Schema.TUnion(_)            => true
    case Schema.TFixed(_, _, _, _)   => true
  }
}
