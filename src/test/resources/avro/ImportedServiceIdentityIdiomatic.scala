package baz.qux
import _root_.higherkindness.mu.rpc.protocol._
final case class Response(arg1: _root_.java.lang.String, arg2: _root_.foo.bar.UserName)
@service(Avro, Identity, namespace = Some("baz.qux"), methodNameStyle = Capitalize) trait ImportedService[F[_]] { def login(req: _root_.foo.bar.LoginCredentials): F[_root_.baz.qux.Response] }