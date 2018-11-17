---
layout: page
title: Schemas
position: 1
---

# Schemas

Currently in skeuomorph there are schemas defined for different cases:

- [Avro][]
- [Protobuf][]
- [mu-rpc][]

## Schema conversions


| from\to       | **Avro**                  | **Protobuf**           | **mu-rpc**                         |
|---------------|---------------------------|------------------------|------------------------------------|
| **Avro**      |                           |                        | `avro.transCata(fromAvro)`         |
| **Protobuf**  |                           |                        | `protobuf.transCata(fromProtobuf)` |
| **mu-rpc**    | `mu.transCata(fromMu)`    | `mu.transCata(fromMu)` |                                    |


[Avro]: https://avro.apache.org/
[Protobuf]: https://developers.google.com/protocol-buffers/
[mu-rpc]: https://higherkindness.github.io/mu/
