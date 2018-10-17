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
import qq.droste._
import qq.droste.data._
import qq.droste.implicits._

import skeuomorph.freestyle._
import skeuomorph.freestyle.FreesF._
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
`namedTypes` combinator (in `skeuomorph.freestyle.Optimize`):

```scala
def nestedNamedTypesTrans[T](implicit T: Basis[FreesF, T]): Trans[FreesF, FreesF, T] = Trans {
  case TProduct(name, fields) =>
    def nameTypes(f: Field[T]): Field[T] = f.copy(tpe = namedTypes(T)(f.tpe))
    TProduct[T](
      name,
      fields.map(nameTypes)
    )
  case other => other
}
  
def namedTypesTrans[T]: Trans[FreesF, FreesF, T] = Trans {
  case TProduct(name, _) => TNamedType[T](name)
  case TSum(name, _)     => TNamedType[T](name)
  case other             => other
}

def namedTypes[T: Basis[FreesF, ?]]: T => T       = scheme.cata(namedTypesTrans.algebra)
def nestedNamedTypes[T: Basis[FreesF, ?]]: T => T = scheme.cata(nestedNamedTypesTrans.algebra)

```

and then apply the `namedTypes` combinator to the AST:

```tut:invisible
def ast = Mu(TNull[Mu[FreesF]]())
```

```tut
val optimization = Optimize.namedTypes[Mu[FreesF]]

optimization(ast)
```
