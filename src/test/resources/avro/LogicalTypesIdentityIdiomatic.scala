package foo.bar
import _root_.higherkindness.mu.rpc.protocol._
final case class LogicalTypes(dec: _root_.shapeless.tag.@@[_root_.scala.math.BigDecimal, (("precision", 20), ("scale", 8))], maybeDec: _root_.scala.Option[_root_.shapeless.tag.@@[_root_.scala.math.BigDecimal, (("precision", 20), ("scale", 8))]], ts: _root_.java.time.Instant, dt: _root_.java.time.LocalDate)
@service(Avro, Identity, namespace = Some("foo.bar"), methodNameStyle = Capitalize) trait LogicalTypes[F[_]] { def identity(req: _root_.foo.bar.LogicalTypes): F[_root_.foo.bar.LogicalTypes] }