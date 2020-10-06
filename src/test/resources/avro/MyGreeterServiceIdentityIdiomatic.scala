package foo.bar
import _root_.higherkindness.mu.rpc.protocol._
final case class HelloRequest(arg1: _root_.java.lang.String, arg2: _root_.scala.Option[_root_.java.lang.String], arg3: _root_.scala.List[_root_.java.lang.String])
final case class HelloResponse(arg1: _root_.java.lang.String, arg2: _root_.scala.Option[_root_.java.lang.String], arg3: _root_.scala.List[_root_.java.lang.String])
@service(Avro, Identity, namespace = Some("foo.bar"), methodNameStyle = Capitalize) trait MyGreeterService[F[_]] {
  def sayHelloAvro(req: _root_.foo.bar.HelloRequest): F[_root_.foo.bar.HelloResponse]
  def sayNothingAvro(req: _root_.higherkindness.mu.rpc.protocol.Empty.type): F[_root_.higherkindness.mu.rpc.protocol.Empty.type]
}