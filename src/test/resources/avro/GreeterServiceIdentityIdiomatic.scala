package foo.bar
import _root_.higherkindness.mu.rpc.protocol._
final case class LogicalIdl(dec: _root_.shapeless.tag.@@[_root_.scala.math.BigDecimal, (("precision", 20), ("scale", 8))], maybeDec: _root_.scala.Option[_root_.shapeless.tag.@@[_root_.scala.math.BigDecimal, (("precision", 20), ("scale", 8))]], ts: _root_.java.time.Instant, dt: _root_.java.time.LocalDate)
final case class HelloRequest(arg1: _root_.java.lang.String, arg2: _root_.scala.Option[_root_.java.lang.String], arg3: _root_.scala.List[_root_.java.lang.String])
final case class HelloResponse(arg1: _root_.java.lang.String, arg2: _root_.scala.Option[_root_.java.lang.String], arg3: _root_.scala.List[_root_.java.lang.String])
@service(Avro, Identity, namespace = Some("foo.bar"), methodNameStyle = Capitalize) trait MyGreeterService[F[_]] {
  def sayHelloAvro(req: _root_.foo.bar.HelloRequest): F[_root_.foo.bar.HelloResponse]
  def sayNothingAvro(req: _root_.higherkindness.mu.rpc.protocol.Empty.type): F[_root_.higherkindness.mu.rpc.protocol.Empty.type]
}