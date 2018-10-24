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

import org.scalacheck.Prop
import org.specs2.{ScalaCheck, Specification}
import qq.droste._
import qq.droste.data._
import skeuomorph.freestyle.FreesF.TCoproduct
import skeuomorph.freestyle.Optimize._

class OptimizeSpec extends Specification with ScalaCheck {

  import skeuomorph.avro.instances._

  def is =
    s2"""
  freestyle Optimize

  It should convert a TCoproduct into a TOption. $convertCoproduct2Option

  It should convert a TCoproduct into a TEither. $convertCoproduct2Either
  """

  def convertCoproduct2Option: Prop = Prop.forAll(freestyleCoproductWithTNullGen[Mu[FreesF]]) {
    coproduct: TCoproduct[Mu[FreesF]] =>
      val transformation: Mu[FreesF] = knownCoproductTypesTrans[Mu[FreesF]].algebra.run(coproduct)

      val test = scheme.hylo(checkOptionalValue, Project[FreesF, Mu[FreesF]].coalgebra)

      test(transformation)
  }

  def convertCoproduct2Either: Prop = Prop.forAll(freestyleCoproductWithoutTNullGen[Mu[FreesF]]) {
    coproduct: TCoproduct[Mu[FreesF]] =>
      val transformation: Mu[FreesF] = knownCoproductTypesTrans[Mu[FreesF]].algebra.run(coproduct)

      val test = scheme.hylo(checkEitherValue, Project[FreesF, Mu[FreesF]].coalgebra)

      test(transformation)
  }

  def checkEitherValue: Algebra[FreesF, Boolean] = Algebra[FreesF, Boolean] {
    case FreesF.TEither(_, _) => true
    case _                    => false
  }

  def checkOptionalValue: Algebra[FreesF, Boolean] = Algebra[FreesF, Boolean] {
    case FreesF.TOption(_) => true
    case _                 => false
  }
}
