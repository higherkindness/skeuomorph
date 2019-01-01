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

import higherkindness.skeuomorph.protobuf.ProtobufF.TMessage
import higherkindness.skeuomorph.protobuf.Optimize._
import org.scalacheck.Prop
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}
import qq.droste.data.Mu
import qq.droste.data.Mu._
import qq.droste.{scheme, Algebra, Project}

class OptimizeSpec extends Specification with ScalaCheck {

  import higherkindness.skeuomorph.instances._

  override def is: SpecStructure =
    s2"""
      Protobuf Optimize

      It should convert a Field with repeated flag set into a TRepeated. $convertRepeatedField
    """

  def convertRepeatedField = Prop.forAll(protobufFMessageWithRepeatFields[Mu[ProtobufF]](true)) {
    message: TMessage[Mu[ProtobufF]] =>
      val transformation: Mu[ProtobufF] = repeatedFieldToListTrans[Mu[ProtobufF]].algebra.run(message)

      val test = scheme.hylo(checkRepeatedValue, Project[ProtobufF, Mu[ProtobufF]].coalgebra)

      test(transformation)
  }

  def checkRepeatedValue: Algebra[ProtobufF, Boolean] = Algebra {
    case ProtobufF.TMessage(_, fields, _, _) => fields.forall(field => field.tpe)
    case ProtobufF.TRepeated(_)              => true
    case _                                   => false
  }
}
