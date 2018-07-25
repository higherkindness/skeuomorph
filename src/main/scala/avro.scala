package skeuomorph

import Function.const

import cats.instances.vector._
import cats.instances.either._
import cats.syntax.either._
import cats.syntax.traverse._

import cats.data.NonEmptyList
import turtles._
import turtles.implicits._
import io.circe._
import io.circe.syntax._

import schema._

object avro {

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

  implicit def avroTypeDecoder[T](implicit T: Corecursive.Aux[T, Type]): Decoder[T] = Decoder.instance { c =>
    c.value.fold(
      DecodingFailure("null not expected", Nil).asLeft[T],
      const(DecodingFailure("boolean not expected", Nil).asLeft[T]),
      const(DecodingFailure("number not expected", Nil).asLeft[T]),
      _ match {
        case "null"    => T.embed(TNull[T]()).asRight[DecodingFailure]
        case "boolean" => T.embed(TBoolean[T]()).asRight[DecodingFailure]
        case "int"     => T.embed(TInt[T]()).asRight[DecodingFailure]
        case "long"    => T.embed(TLong[T]()).asRight[DecodingFailure]
        case "float"   => T.embed(TFloat[T]()).asRight[DecodingFailure]
        case "double"  => T.embed(TDouble[T]()).asRight[DecodingFailure]
        case "bytes"   => T.embed(TBytes[T]()).asRight[DecodingFailure]
        case "string"  => T.embed(TString[T]()).asRight[DecodingFailure]
        case somename  => T.embed(TNamedType[T](somename)).asRight[DecodingFailure]
      },
      vect =>
        if (vect.length > 0) {
          vect.traverse(_.as[T]).map(ts => T.embed(TUnion[T](NonEmptyList.fromListUnsafe(ts.toList), const(None))))
        } else {
          DecodingFailure("type union expected to have at least one field", Nil).asLeft[T],
      },
      obj =>
        for {
          logicalType <- c.getOrElse[Option[String]]("logicaltype")(None)
          tpe         <- c.get[String]("type")
          x <- tpe match {
            case "map" => c.get[String]("values").flatMap(_.asJson.as[T].map(TMap[T](_).embed))
            case "array" => c.get[String]("items").flatMap(_.asJson.as[T].map(TMap[T](_).embed))
            case "record" => for {
              
            }
          }
        } yield x
    )
  }

}
