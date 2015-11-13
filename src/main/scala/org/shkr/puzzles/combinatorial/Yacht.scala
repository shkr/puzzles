package org.shkr.puzzles.combinatorial

import org.shkr.puzzles.combinatorial.Daughter.Daughter
import org.shkr.puzzles.combinatorial.Father.Father

object Yacht {

  val fathers = Father.values
  val daughters = Daughter.values
  val yachts = daughters

  case class Family(father: Father, daughter: Daughter, yacht: Daughter)

  val perms = for {
    father <- fathers
    daughter <- daughters
    yacht <- yachts if yacht != daughter
  } yield Family(father, daughter, yacht)

  import Daughter._
  import Father._

  val assertion1: PartialFunction[Family, Boolean] = {
    case someFamily: Family => someFamily.daughter!=someFamily.yacht
  }

  val assertion2: PartialFunction[Family, Boolean] = {
    case Family(Hood, daughter, yacht) => yacht==Gabrielle
  }

  val assertion3: PartialFunction[Family, Boolean] = {
    case Family(Moore, daughter, yacht) => yacht==Lorna
  }

  val assertion4: PartialFunction[Family, Boolean] = {
    case Family(Hall, daughter, yacht) => yacht==Rosalind
  }

  val assertion5: PartialFunction[Family, Boolean] = {
    case Family(Downing, daughter, yacht) => yacht==Melissa
  }

  val assertion6: PartialFunction[Family, Boolean] = {
    case Family(Hood, daughter, yacht) => daughter==Melissa
  }

  val assertion7: PartialFunction[Family, Boolean] = {
    case Family(Moore, daughter, yacht) => daughter==MaryAnne
  }

  //Used as List[Family].withFilter(assertion*.orElse(defaultAssertion))
  val defaultAssertion: PartialFunction[Family, Boolean] = {
    case _ => true
  }
  val valid = perms.
    filter(p => p.daughter != MaryAnne || p.father == Moore).
    filter(p => p.yacht != Gabrielle || p.father == Hood).
    filter(p => p.yacht != Lorna || p.father == Moore).
    filter(p => p.yacht != Rosalind || p.father == Hall).
    filter(p => p.yacht != Melissa || p.father == Downing).
    filter(p => p.daughter != Melissa || p.father == Hood)

  def isValid(families: List[Family]): Boolean = {
    families.filter(_.daughter == Gabrielle).head.yacht ==
      families.filter(_.father == Parker).head.daughter
  }

  val combos = valid.toList.combinations(5)
  val validCombos = combos.
    filter(_.map(_.father).toSet.size == 5).
    filter(_.map(_.daughter).toSet.size == 5).
    filter(_.map(_.yacht).toSet.size == 5).
    filter(isValid)

  def main(args: Array[String]): Unit={
    
    println(validCombos.next().filter(_.daughter == Lorna).head.father)
  }
}