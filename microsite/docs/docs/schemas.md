---
layout: docs
title: Schemas
permalink: docs/schemas/
---

# Schemas

Currently in skeuomorph there are schemas defined for different cases:

- [Avro][]
- [Protobuf][]
- [mu][]

## Schema conversions


| from\to       | **Avro**                  | **Protobuf**           | **mu**                             |
|---------------|---------------------------|------------------------|------------------------------------|
| **Avro**      |                           |                        | `avro.transCata(fromAvro)`         |
| **Protobuf**  |                           |                        | `protobuf.transCata(fromProtobuf)` |
| **mu**        | `mu.transCata(fromMu)`    | `mu.transCata(fromMu)` |                                    |

### Schema Incompatibilities

Currently, Skeuomorph only supports proto3 compliance, and the recommended approach when using skeuomorph with [mu][]
is to use proto3 for all gRPC communications.  While it is still possible to generate valid Scala code from a proto2 spec,
Skeuomorph will _not_ generate case classes for optional fields.  For example, given a schema that looks like this:

```protobuf mdoc:silent
syntax = "proto2";

package src.main.hello;

message SayHelloRequest {
  optional string name = 1;
}
message SayHelloResponse {
  optional string message = 1;
}

service HelloWorldService {
  rpc SayHello (SayHelloRequest) returns (SayHelloResponse) {}
}
```

Skeuomorph (with mu) will generate the following Scala code:

```scala mdoc:silent
object hello {
  final case class SayHelloRequest(name: String)
  final case class SayHelloResponse(message: String)
  @service(Protobuf, Identity, namespace = Some("src.main.hello"), methodNameStyle = Capitalize) 
  trait HelloWorldService[F[_]] { 
    def SayHello(req: src.main.hello.hello.SayHelloRequest): F[src.main.hello.hello.SayHelloResponse] 
  }
}  
```

As you can see, even though the parameters for the proto2 schema are `option string`s, the generated code is of type 
`String`.  

[Avro]: https://avro.apache.org/
[Protobuf]: https://developers.google.com/protocol-buffers/
[mu]: https://higherkindness.github.io/mu/
