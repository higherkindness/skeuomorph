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
import higherkindness.droste._
import higherkindness.droste.data._
import higherkindness.droste.implicits._

import higherkindness.skeuomorph.mu._
import higherkindness.skeuomorph.mu.MuF._
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

We solve this by substituting nested product types by its name when
they're inside a product themselves.  And we do this with the
`namedTypes` combinator (in `skeuomorph.mu.Optimize`):

```scala
def nestedNamedTypesTrans[T](implicit T: Basis[MuF, T]): Trans[MuF, MuF, T] = Trans {
  case TRecord(name, fields) =>
    def nameTypes(f: Field[T]): Field[T] = f.copy(tpe = namedTypes(T)(f.tpe))
    TRecord[T](
      name,
      fields.map(nameTypes)
    )
  case other => other
}
  
def namedTypesTrans[T]: Trans[MuF, MuF, T] = Trans {
  case TRecord(name, _) => TNamedType[T](name)
  case TSum(name, _)     => TNamedType[T](name)
  case other             => other
}

def namedTypes[T: Basis[MuF, ?]]: T => T       = scheme.cata(namedTypesTrans.algebra)
def nestedNamedTypes[T: Basis[MuF, ?]]: T => T = scheme.cata(nestedNamedTypesTrans.algebra)

```

and then apply the `namedTypes` combinator to the AST:

```tut:invisible
def ast = Mu(TNull[Mu[MuF]]())
```

```tut
val optimization = Optimize.namedTypes[Mu[MuF]]

optimization(ast)
```
