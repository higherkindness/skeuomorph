import higherkindness.skeuomorph.mu._
import MuF._
import SerializationType._
import Service._
import CompressionType._
Protocol(
  None,
  Some(foo.bar),
  List(),
  List(
    TByteArray(),
    TProduct(Fixed,List(Field(fix,TByteArray(),None)),List(),List())
  ),
  List(
    Service(
      Fixed,
      Avro,
      Identity,
      IdiomaticEndpoints(Some(foo.bar),true),
      List(
        Operation(
          identity,
          OperationType(TNamedType(List(foo, bar),Fixed),false),
          OperationType(TNamedType(List(foo, bar),Fixed),false)
        )
      )
    )
  ),
  List()
)

Protocol(
  None,
  Some(foo.bar),
  List(),
  List(
    TNamedType(List(foo.bar), TestFixed),
    TProduct(
      Fixed,
      List(Field(fix, TNamedType(List(foo.bar), TestFixed), None)),
      List(),
      List())
  ),
  List(Service(Fixed, Avro, Identity, IdiomaticEndpoints(Some(foo.bar), true), List(Operation(identity, OperationType(TNamedType(List(foo, bar), Fixed), false), OperationType(TNamedType(List(foo, bar), Fixed), false))))),
  List())
Protocol(
  None,
  Some(foo.bar),
  List(),
  List(
    TProduct(
      HelloRequest,
      List(Field(arg1,TString(),None), Field(arg2,TCoproduct(NonEmptyList(TNull(), TString())),None), Field(arg3,TList(TString()),None)),
      List(),
      List()
    ),
    TProduct(HelloResponse,List(Field(arg1,TString(),None), Field(arg2,TCoproduct(NonEmptyList(TNull(), TString())),None), Field(arg3,TList(TString()),None)),List(),List())
  )
  ,List(Service(MyGreeterService,Avro,Identity,IdiomaticEndpoints(Some(foo.bar),true),List(Operation(sayHelloAvro,OperationType(TNamedType(List(foo, bar),HelloRequest),false),OperationType(TNamedType(List(foo, bar),HelloResponse),false)), Operation(sayNothingAvro,OperationType(TNull(),false),OperationType(TNull(),false))))),List())