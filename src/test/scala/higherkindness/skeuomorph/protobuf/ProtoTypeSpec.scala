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

package higherkindness.skeuomorph
package protobuf

import cats.instances.option._
import cats.instances.int._
import cats.instances.set._
import cats.laws.discipline.TraverseTests

import org.typelevel.discipline.specs2.Discipline
import org.specs2._

import instances._

class ProtoSchemaSpec extends Specification with ScalaCheck with Discipline {

  def is = s2"""
  $traverse
  """

  val traverseTests = TraverseTests[Type]
  val tests         = traverseTests.traverse[Int, Int, Int, Set[Int], Option, Option]
  val traverse      = checkAll("Traverse[protobuf.Type]", tests)
}
