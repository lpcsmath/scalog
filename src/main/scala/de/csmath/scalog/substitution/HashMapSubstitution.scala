package de.csmath.scalog.substitution

import de.csmath.scalog.Types.{Term, Var}

import scala.collection.immutable.HashMap

class HashMapSubstitution(hashMap: Map[Var,Term] = HashMap.empty) extends Substitution {

  def compose(other: Substitution) = {
    val othermap = other.mapping.map {
      case (k,Var(x)) if (hashMap.contains(Var(x))) =>
       (k,hashMap(Var(x)))
      case other => other
    }
    new HashMapSubstitution(hashMap ++ othermap)
  }

  def restrict(vars: Set[Var]) =
    new HashMapSubstitution(hashMap.filter{ case (k,v) => vars.contains(k)})

  def mapping = hashMap

}
