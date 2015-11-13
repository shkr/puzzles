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

//  val whiteAssertions = {
//    case Family(_, d, y) => d!=y
//    case Family(Hood, _, Gabrielle) => true
//    case Family(Moore, _, Lorna) => true
//    case Family(Hall, _, Rosalind) => true
//    case Family(Downing, _, Melissa) => true
//    case Family(Hood, Melissa, _) => true
//    case Family(Moore, MaryAnne, _) => true
//  }

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