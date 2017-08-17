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

### Backtracking

This example shows how to get not more than 3 solutions.

```scala
val dbString =
  """
    |append([],X,X).
    |append([W|X],Y,[W|Z]) :- append(X,Y,Z).
  """.stripMargin
val db = parser.parse(parser.database,dbString).get

val queryString =
  """
    |:- append(X,Y,[1,2,3,4]).
  """.stripMargin
val query = parser.parse(parser.query,queryString).get

val subs = SLDResolver().resolve(query,3)(db)

subs foreach println
// { Y <- [1,2,3,4], X <- [] }
// { Y <- [2,3,4], X <- [1] }
// { Y <- [3,4], X <- [1,2] }
```

The next example shows how to get all solutions.

```scala
val dbString =
  """
    |permutation([],[]).
    |permutation(L,[E|R]) :- scratch(E,L,L2), permutation(L2,R).
    |scratch(E,[E|R],R).
    |scratch(E,[F|R],[F|Rwo]) :- scratch(E,R,Rwo).
  """.stripMargin
val db = parser.parse(parser.database,dbString).get

val queryString =
  """
    |:- permutation([1,2,3,4],X).
  """.stripMargin
val query = parser.parse(parser.query,queryString).get

val resolver = SLDResolver()

var subs = resolver.resolve(query)(db)

while (subs.nonEmpty) {
  subs foreach println
  subs = resolver.backTrack()
}
```