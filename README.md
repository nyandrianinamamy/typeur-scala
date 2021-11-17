## Type Inference System

Simply typed lambda-calculus with let-polymorphism and imperative traits.

### Implemented terms and types

```scala
Term := Var | App | Abs | Nat | Lst | Cons | EOL | Letin
Operator := Izte | Iete | Fix | Head | Tail | Add | Diff | Ref | Deref | Assign | Void
Type := TVar | Arrow | N | TLst | EmptyLst | Forall | Tref | TVoid
```

### Tests
`TypeurTest` shows a bunch of tests for the type inference system.

```scala
...
@Test def `Ref x: Ref x`: Unit =
  
@Test def `let f = (lambda x.x) in let g = (lambda xy.x) in g (f 1) (f t): N`: Unit =
  
@Test def `lambda xyz.(xz)(yz) : ('a -> ('b -> 'c)) -> (('a -> 'b) -> ('a -> 'c))`(): Unit =
...
```

You can run it by launching `sbt`, then `testOnly TypeurTest`.

### Installation
#### Requirements
Java 14+  
Scala 3  
sbt 1.x

Install `sbt` [https://www.scala-sbt.org/1.x/docs/Setup.html](https://www.scala-sbt.org/1.x/docs/Setup.html)

You can compile code with `sbt compile`

You can run all tests with `sbt test`

You can run the app with `sbt run`