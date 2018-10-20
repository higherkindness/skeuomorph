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
package freestyle

import qq.droste.Algebra
import qq.droste.macros.deriveTraverse
import cats.data.NonEmptyList

@deriveTraverse sealed trait FreesF[A]
object FreesF {
  final case class TNull[A]()                                  extends FreesF[A]
  final case class TDouble[A]()                                extends FreesF[A]
  final case class TFloat[A]()                                 extends FreesF[A]
  final case class TInt[A]()                                   extends FreesF[A]
  final case class TLong[A]()                                  extends FreesF[A]
  final case class TBoolean[A]()                               extends FreesF[A]
  final case class TString[A]()                                extends FreesF[A]
  final case class TByteArray[A]()                             extends FreesF[A]
  final case class TNamedType[A](name: String)                 extends FreesF[A]
  final case class TOption[A](value: A)                        extends FreesF[A]
  final case class TList[A](value: A)                          extends FreesF[A]
  final case class TMap[A](value: A)                           extends FreesF[A]
  final case class TGeneric[A](generic: A, params: List[A])    extends FreesF[A]
  final case class TRequired[A](value: A)                      extends FreesF[A]
  final case class TCoproduct[A](invariants: NonEmptyList[A])  extends FreesF[A]
  final case class TSum[A](name: String, fields: List[String]) extends FreesF[A]
  final case class TProduct[A](name: String, fields: List[A])  extends FreesF[A]
  final case class Field[A](name: String, tpe: A)              extends FreesF[A]

  def render: Algebra[FreesF, String] = Algebra {
    case TNull()                   => "Null"
    case TDouble()                 => "Double"
    case TFloat()                  => "Float"
    case TInt()                    => "Int"
    case TLong()                   => "Long"
    case TBoolean()                => "Boolean"
    case TString()                 => "String"
    case TByteArray()              => "Array[Byte]"
    case TNamedType(name)          => name
    case TOption(value)            => s"Option[$value]"
    case TMap(value)               => s"Map[String, $value]"
    case TGeneric(generic, params) => s"""$generic[${params.mkString(", ")}]"""
    case TList(value)              => s"List[$value]"
    case TRequired(value)          => value
    case TCoproduct(invariants) =>
      invariants.toList.mkString("Cop[", " :: ", ":: TNil]")
    case TSum(name, fields) =>
      val printFields = fields.map(f => s"case object $f extends $name").mkString("\n  ")
      s"""
      |sealed trait $name
      |object $name {
      |  $printFields
      |}
      """.stripMargin
    case TProduct(name, fields) =>
      val printFields = fields.mkString(", ")
      s"@message final case class $name($printFields)"
    case Field(name, tpe) =>
      s"$name: $tpe"
  }

}
