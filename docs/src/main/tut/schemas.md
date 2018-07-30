---
layout: page
title: Schemas
position: 1
---

# Schemas

The following table expects you to have the following imports in
place:

```tut:silent
import turtles.data._
import turtles.implicits._
import skeuomorph._
```

Currently in skeuomorph there are some schemas defined for different
cases:

- [Avro][]
- [Protobuf][]
- [freestyle-rpc][]

## Schema conversions


| from\to           | **Avro**                         | **Protobuf**                     | **freestyle-rpc**                  |
|-------------------|----------------------------------|----------------------------------|------------------------------------|
| **Avro**          |                                  |                                  | `avro.transCata(fromAvro)`         |
| **Protobuf**      |                                  |                                  | `protobuf.transCata(fromProtobuf)` |
| **freestyle-rpc** | `frees.transCata(fromFreestyle)` | `frees.transCata(fromFreestyle)` |                                    |


```tut
val asdf = 2
```

[Avro]: https://avro.apache.org/
[Protobuf]: https://developers.google.com/protocol-buffers/
[freestyle-rpc]: http://frees.io/docs/rpc/quickstart
