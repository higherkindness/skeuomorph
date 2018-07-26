# Skeuomorph

Skeuomorph is for schema transformation.

```scala
scala> import skeuomorph._
import skeuomorph._

scala> import turtles.data.Mu
import turtles.data.Mu

scala> import turtles.implicits._
import turtles.implicits._

scala> protobuf.util.searchRequest[Mu[protobuf.Schema]].cata(protobuf.util.render)
res0: String =
"
message SearchRequest {

  required string query = 1;
  optional int32 page_number = 2;
  optional int32 results_per_page = 3 [default = 10];
  optional Corpus corpus = 4 [default = UNIVERSAL];
}
"

scala> freestyle.utils.fromProtoRender[Mu[protobuf.Schema], Mu[freestyle.Schema]](protobuf.util.searchRequest[Mu[protobuf.Schema]])
res1: String =
"
@message case class SearchRequest(query: String, page_number: Option[Int], results_per_page: Option[Int], corpus: Option[Corpus])
"
```
