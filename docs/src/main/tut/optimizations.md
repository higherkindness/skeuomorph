---
layout: page
title: Optimizations
position: 2
---

# Optimizations

The technique we use to model recursive data throughout `skeuomorph`
is called recursion schemes.  Recursion schemes allows us to model our
data as non recursive and substitute direct recursion by a call to a
type parameter in the declaration.

One of the techniques that we use from recursion schemes is
microoptimizations.  We're able to transform ASTs by just describing
the optimization we want as a function, and the library provides
mechanisms to apply that function to the AST correctly.  Let's see
`namedTypes` as an example:

## NamedTypes

```tut:invisible
import turtles._
import turtles.data._
import turtles.implicits._
import skeuomorph.freestyle._
import skeuomorph.freestyle.Schema._
```

We found that when we wanted to render a schema to its string
representation and the schema had nested product types, the rendering
was not correct because it was printing the definition of the product
everywhere:

```
case class Product(field1: String, field2: case class OtherField())
                                           ^---------------------^
// see how this is not valid scala code, it should be:

case class Product(field1: String, field2: OtherField)
```

We solve this by substituting nested product types by it's name when
they're inside a product themselves.  And we do this with the
`namedTypes` combinator:

```tut:silent
def namedTypes[T](t: T)(implicit T: Birecursive.Aux[T, Schema]): T =
  t.project match {
    case TProduct(name, fields) =>
      TProduct[T](
        name,
        fields.map { f: Field[T] =>
          f.copy(tpe = f.tpe.transCataT(_.project match {
            case TProduct(name, _) => TNamedType[T](name).embed
            case TSum(name, _)     => TNamedType[T](name).embed
            case other             => other.embed
          }))
        }
      ).embed
    case other => other.embed
  }
```

and then apply the `namedTypes` combinator to the AST:

```tut:invisible
def ast[T](implicit T: Birecursive.Aux[T, Schema]): T = TNull[T]().embed
```

```tut
ast[Mu[Schema]].transCataT(namedTypes)
```
