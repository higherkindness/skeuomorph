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

package higherkindness.skeuomorph.protobuf

import higherkindness.skeuomorph.protobuf.ProtobufF.{TMessage, TRepeated}
import qq.droste.{scheme, Basis, Trans}

object Optimize {

  /**
   * Micro optimization to convert repeated fields (necessary for
   * protobuf support) into their proper list type, TRepeated. Doing this in
   * initial parsing pass creates infinite loop problems.
   */
  def repeatedFieldToListTrans[T](implicit T: Basis[ProtobufF, T]): Trans[ProtobufF, ProtobufF, T] = Trans {
    case message: TMessage[T] =>
      val transformedMessage: T = repeatedTypes(T)(T.algebra(message))
      T.coalgebra(transformedMessage)
    case other => other
  }

  def repeatedTypes[T: Basis[ProtobufF, ?]]: T => T = scheme.cata(repeatedTypesTrans.algebra)

  def repeatedTypesTrans[T](implicit T: Basis[ProtobufF, T]): Trans[ProtobufF, ProtobufF, T] = Trans {
    case TMessage(n, fields, reserved, oneOfs) =>
      val listFields: List[FieldF[T]] = fields.map(field =>
        field match {
          case f: ProtobufF.Field[T] =>
            if (f.isRepeated && !f.isMapField) // Map fields cannot be repeated according to the proto spec
              ProtobufF.Field(f.name, T.algebra(TRepeated(f.tpe)), f.position, f.options, f.isRepeated, f.isMapField)
            else f
          case other => other
      })
      TMessage(n, listFields, reserved, oneOfs)
    case other => other
  }

  /**
   * This optimization includes the protobuf "OneOf" list as fields, so
   * that it can be interpreted into correct scala code as a member of a case
   * class.
   * */
  def combineFields[T: Basis[ProtobufF, ?]]: T => T = scheme.cata(oneOfsAsFieldsTrans.algebra)

  def oneOfsAsFieldsTrans[T](implicit T: Basis[ProtobufF, T]): Trans[ProtobufF, ProtobufF, T] = Trans {
    case TMessage(name, messageFields, reserved, oneOfs) => {

      val oneOfAsField: Seq[ProtobufF.SimpleField[T]] = oneOfs.map(
        oneOf =>
          ProtobufF.SimpleField(
            oneOf.name,
            T.algebra(oneOf)
        ))

      TMessage(name, messageFields ++ oneOfAsField, reserved, List())
    }
    case other => other
  }
}
