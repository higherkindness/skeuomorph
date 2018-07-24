package skeuomorph

object App {
  sealed trait FieldOption
  case object Repeated extends FieldOption
  case object Optional extends FieldOption

  sealed trait Type
  case class TString(value: String)                       extends Type
  case class TInt(value: Int)                             extends Type
  case class TLong(value: Long)                           extends Type
  case class TDouble(value: Double)                       extends Type
  case class TFloat(value: Float)                         extends Type
  case class TBool(value: Boolean)                        extends Type
  case class TBytes(value: Vector[Byte])                  extends Type
  case class Message(name: String, fields: Vector[Field]) extends Type

  case class Field(name: String, options: FieldOption, tpe: Type)
  case class Endpoint(name: String, request: Field, response: Type)
  case class Service(name: String, endpoints: Vector[Endpoint])
  case class Schema(messages: List[Message], services: List[Service])
}
