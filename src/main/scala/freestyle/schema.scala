package skeuomorph
package freestyle

import cats.Functor

sealed trait Schema[A]
object Schema {
  case class Field[A](name: String, tpe: A)

  case class TDouble[A]()                                      extends Schema[A]
  case class TFloat[A]()                                       extends Schema[A]
  case class TInt[A]()                                         extends Schema[A]
  case class TLong[A]()                                        extends Schema[A]
  case class TBoolean[A]()                                     extends Schema[A]
  case class TString[A]()                                      extends Schema[A]
  case class TByteArray[A]()                                   extends Schema[A]
  case class TNamedType[A](name: String)                       extends Schema[A]
  case class TOption[A](value: A)                              extends Schema[A]
  case class TList[A](value: A)                                extends Schema[A]
  case class TRequired[A](value: A)                            extends Schema[A]
  case class TSum[A](name: String, fields: List[String])       extends Schema[A]
  case class TProduct[A](name: String, fields: List[Field[A]]) extends Schema[A]

  implicit val schemaFunctor: Functor[Schema] = new Functor[Schema] {
    def map[A, B](fa: Schema[A])(f: A => B): Schema[B] = fa match {
      case TDouble()              => TDouble()
      case TFloat()               => TFloat()
      case TInt()                 => TInt()
      case TLong()                => TLong()
      case TBoolean()             => TBoolean()
      case TString()              => TString()
      case TByteArray()           => TByteArray()
      case TNamedType(name)       => TNamedType(name)
      case TOption(value)         => TOption(f(value))
      case TList(value)           => TList(f(value))
      case TRequired(value)       => TRequired(f(value))
      case TSum(name, fields)     => TSum(name, fields)
      case TProduct(name, fields) => TProduct(name, fields.map(field => field.copy(tpe = f(field.tpe))))
    }
  }

}
