---
layout: page
title: Schemas
position: 1
---

# Schemas

Currently in skeuomorph there are schemas defined for different cases:

- [Avro][]
- [Protobuf][]
- [freestyle-rpc][]

## Schema conversions


| from\to           | **Avro**                         | **Protobuf**                     | **freestyle-rpc**                  |
|-------------------|----------------------------------|----------------------------------|------------------------------------|
| **Avro**          |                                  |                                  | `avro.transCata(fromAvro)`         |
| **Protobuf**      |                                  |                                  | `protobuf.transCata(fromProtobuf)` |
| **freestyle-rpc** | `frees.transCata(fromFreestyle)` | `frees.transCata(fromFreestyle)` |                                    |


[Avro]: https://avro.apache.org/
[Protobuf]: https://developers.google.com/protocol-buffers/
[freestyle-rpc]: http://frees.io/docs/rpc/quickstart
