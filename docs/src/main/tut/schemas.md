---
layout: page
title: Schemas
position: 1
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


[Avro]: https://avro.apache.org/
[Protobuf]: https://developers.google.com/protocol-buffers/
[mu]: https://higherkindness.github.io/mu/
