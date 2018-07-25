package skeuomorph

import cats.Functor
import cats.data.NonEmptyList

import turtles._
import turtles.implicits._

object schema {

  type TypeName = String

  sealed trait Order
  object Order {
    case object Ascending  extends Order
    case object Descending extends Order
    case object Ignore     extends Order
  }

  case class Field[A](
      name: String,
      aliases: List[String],
      doc: Option[String],
      order: Option[Order],
      tpe: Type[A]
//      default: Option[Value[Type[A]]]
  )

  sealed trait Type[A]
  object Type {

    /* primitive types */
    case class TNull[A]()                    extends Type[A]
    case class TBoolean[A]()                 extends Type[A]
    case class TInt[A]()                     extends Type[A]
    case class TLong[A]()                    extends Type[A]
    case class TFloat[A]()                   extends Type[A]
    case class TDouble[A]()                  extends Type[A]
    case class TBytes[A]()                   extends Type[A]
    case class TString[A]()                  extends Type[A]
    case class TNamedType[A](name: TypeName) extends Type[A]
    case class TArray[A](item: A)            extends Type[A]
    case class TMap[A](values: A)            extends Type[A]
    case class TRecord[A](
        name: TypeName,
        namespace: Option[String],
        aliases: List[TypeName],
        doc: Option[String],
        order: Option[Order],
        fields: List[Field[A]])
        extends Type[A]
    case class TEnum[A](
        name: TypeName,
        namespace: Option[String],
        aliases: List[TypeName],
        doc: Option[String],
        symbols: List[String],
        symbolLookup: Int => Option[String])
        extends Type[A]
    case class TUnion[A](options: NonEmptyList[A], unionLookup: Int => Option[A])                       extends Type[A]
    case class TFixed[A](name: TypeName, namespace: Option[String], aliases: List[TypeName], size: Int) extends Type[A]
  }

  implicit val typeFunctor: Functor[Type] = new Functor[Type] {
    def map[A, B](fa: Type[A])(fn: A => B): Type[B] = fa match {
      case Type.TNull()          => Type.TNull()
      case Type.TBoolean()       => Type.TBoolean()
      case Type.TInt()           => Type.TInt()
      case Type.TLong()          => Type.TLong()
      case Type.TFloat()         => Type.TFloat()
      case Type.TDouble()        => Type.TDouble()
      case Type.TBytes()         => Type.TBytes()
      case Type.TString()        => Type.TString()
      case Type.TNamedType(name) => Type.TNamedType(name)
      case Type.TArray(item)     => Type.TArray(fn(item))
      case Type.TMap(values)     => Type.TMap(fn(values))
      case Type.TRecord(name, namespace, aliases, doc, order, fields) =>
        Type.TRecord(name, namespace, aliases, doc, order, fields.map(field => field.copy(tpe = map(field.tpe)(fn))))
      case Type.TEnum(name, namespace, aliases, doc, symbols, symbolLookup) =>
        Type.TEnum(name, namespace, aliases, doc, symbols, symbolLookup)
      case Type.TUnion(options, unionLookup)           => Type.TUnion(options.map(fn), unionLookup.andThen(x => x.map(fn)))
      case Type.TFixed(name, namespace, aliases, size) => Type.TFixed(name, namespace, aliases, size)
    }
  }

  import Type._

  def typeName: Algebra[Type, String] = {
    case TNull()                       => "null"
    case TBoolean()                    => "boolean"
    case TInt()                        => "int"
    case TLong()                       => "long"
    case TFloat()                      => "float"
    case TDouble()                     => "double"
    case TBytes()                      => "bytes"
    case TString()                     => "string"
    case TArray(_)                     => "array"
    case TMap(_)                       => "map"
    case TNamedType(name)              => name
    case TUnion(NonEmptyList(x, _), _) => x
    case TEnum(name, _, _, _, _, _)    => name
    case TRecord(name, _, _, _, _, _)  => name
    case TFixed(name, _, _, _)         => name
  }

  def `Null | Boolean`[T](implicit T: Corecursive.Aux[T, Type]): T =
    TUnion[T](NonEmptyList(TNull[T]().embed, List(TBoolean[T]().embed)), {
      case 0 => Some(TNull[T]().embed)
      case 1 => Some(TBoolean[T]().embed)
      case _ => None
    }).embed

  def field[T](name: String, tpe: Type[T]): Field[T] =
    Field[T](name, Nil, None, None, tpe)

  def HelloResponse[T](implicit T: Corecursive.Aux[T, Type]): T =
    TRecord[T](
      "HelloResponse",
      None,
      Nil,
      None,
      None,
      List(
        field[T]("arg1", TString[T]()),
        field[T]("arg2", TUnion[T](NonEmptyList(TNull[T]().embed, List(TBoolean[T]().embed)), {
          case 0 => Some(TNull[T]().embed)
          case 1 => Some(TBoolean[T]().embed)
          case _ => None
        }))
      )
    ).embed
}
