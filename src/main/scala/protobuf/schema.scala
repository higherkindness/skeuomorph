package skeuomorph
package protobuf

import cats.Functor

sealed trait Schema[A]

object Schema {
  case class Field[A](name: String, tpe: Schema[A], position: Int, options: List[Option])
  case class Option(name: String, value: String)

  case class TDouble[A]()           extends Schema[A]
  case class TFloat[A]()            extends Schema[A]
  case class TInt32[A]()            extends Schema[A]
  case class TInt64[A]()            extends Schema[A]
  case class TUint32[A]()           extends Schema[A]
  case class TUint64[A]()           extends Schema[A]
  case class TSint32[A]()           extends Schema[A]
  case class TSint64[A]()           extends Schema[A]
  case class TFixed32[A]()          extends Schema[A]
  case class TFixed64[A]()          extends Schema[A]
  case class TSfixed32[A]()         extends Schema[A]
  case class TSfixed64[A]()         extends Schema[A]
  case class TBool[A]()             extends Schema[A]
  case class TString[A]()           extends Schema[A]
  case class TBytes[A]()            extends Schema[A]
  case class TRequired[A](value: A) extends Schema[A]
  case class TOptional[A](value: A) extends Schema[A]
  case class TRepeated[A](value: A) extends Schema[A]
  case class TEnum[A](symbols: List[(String, Int)], options: List[Option], aliases: List[(String, String)])
      extends Schema[A]
  case class TMessage[A](fields: List[Field[A]], reserved: List[List[String]]) extends Schema[A]

  implicit val schemaFunctor: Functor[Schema] = new Functor[Schema] {
    def map[A, B](fa: Schema[A])(f: A => B): Schema[B] = fa match {
      case TDouble()                        => TDouble()
      case TFloat()                         => TFloat()
      case TInt32()                         => TInt32()
      case TInt64()                         => TInt64()
      case TUint32()                        => TUint32()
      case TUint64()                        => TUint64()
      case TSint32()                        => TSint32()
      case TSint64()                        => TSint64()
      case TFixed32()                       => TFixed32()
      case TFixed64()                       => TFixed64()
      case TSfixed32()                      => TSfixed32()
      case TSfixed64()                      => TSfixed64()
      case TBool()                          => TBool()
      case TString()                        => TString()
      case TBytes()                         => TBytes()
      case TRequired(value)                 => TRequired(f(value))
      case TOptional(value)                 => TOptional(f(value))
      case TRepeated(value)                 => TRepeated(f(value))
      case TEnum(symbols, options, aliases) => TEnum(symbols, options, aliases)
      case TMessage(fields, reserved) =>
        TMessage(
          fields.map(field => field.copy(tpe = map(field.tpe)(f))),
          reserved
        )

    }
  }
}
