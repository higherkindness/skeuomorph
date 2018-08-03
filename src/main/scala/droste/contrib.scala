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
package droste

import qq.droste._


/**
  * to be contributed back to droste.  Maybe it's easier to use @typeclass there... dunno :)
  */
object contrib {

  trait EmbedOps[F[_], T] {
    def tc: Embed[F, T]
    def self: F[T]

    def embed: T = tc.algebra(self)
  }

  trait ProjectOps[F[_], T] {
    def tc: Project[F, T]
    def self: T

    def project: F[T] = tc.coalgebra(self)
  }

  implicit def toEmbedOps[F[_], T](t: F[T])(implicit Embed: Embed[F, T]): EmbedOps[F, T] =
    new EmbedOps[F, T] {
      def tc   = Embed
      def self = t
    }

  implicit def toProjectOps[F[_], T](t: T)(implicit Project: Project[F, T]): ProjectOps[F, T] =
    new ProjectOps[F, T] {
      def tc   = Project
      def self = t
    }

}
