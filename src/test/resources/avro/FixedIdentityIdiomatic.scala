package foo.bar
import _root_.higherkindness.mu.rpc.protocol._
object TestFixed {
  type TestFixed = _root_.scala.Array[Byte]
  val length: 16 = 16
}
final case class Fixed(fix: _root_.foo.bar.TestFixed.TestFixed)
@service(Avro, Identity, namespace = Some("foo.bar"), methodNameStyle = Capitalize) trait Fixed[F[_]] { def identity(req: _root_.foo.bar.Fixed): F[_root_.foo.bar.Fixed] }