# scalog
Prolog Implementation in Scala

So far, something like this is working

```scala
import de.csmath.scalog._

val parser = Parser()

val dbString =
  """
    |add(X,zero,X).
    |add(X,succ(Y),succ(Z)) :- add(X,Y,Z).
  """.stripMargin

val queryString =
  """
    |:- add(succ(succ(zero)),succ(zero),Y).
  """.stripMargin


val subs = for {
  db    <- parser.parseDb(dbString)
  query <- parser.parseQuery(queryString)
} yield SLDResolver().resolve(query,1)(db)

subs.get foreach println
//
// { Y <- succ(succ(succ(zero))) }
//
```
