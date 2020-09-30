package foo.bar
import _root_.higherkindness.mu.rpc.protocol._
final case class LogicalTypes(fix: _root_.scala.Array[Byte])
@service(Avro, Identity, namespace = Some("foo.bar"), methodNameStyle = Capitalize) trait Fixed[F[_]] { def identity(req: _root_.foo.bar.Fixed): F[_root_.foo.bar.Fixed] }