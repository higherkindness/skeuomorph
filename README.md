# Skeuomorph

Skeuomorph is for schema transformation.

## Schemas

Currently skeuomorph supports 3 schemas:
- Avro
- Protobuf
- Freestyle

and has conversions between them.


## Examples

### parsing an avro schema and then converting it to scala:

```scala
import java.io.File
import org.apache.avro._
import skeuomorph._
import skeuomorph.freestyle.service._
import turtles._
import turtles.data.Mu
import turtles.implicits._

val schema: Schema = new Schema.Parser().parse(new File("user.avsc"));

schema.ana[Mu[avro.Schema]](avro.util.fromAvro) // org.apache.avro.Schema => skeuomorph.avro.Schema
      .transCata[Mu[freestyle.Schema]](freestyle.utils.transformAvro) // skeuomorph.avro.Schema => skeuomorph.freestyle.Schema
      .cata(freestyle.utils.render) // skeuomorph.freestyle.Schema => String
```
