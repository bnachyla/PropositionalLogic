package propositional

import annotation.tailrec
import propositional.{¬, Formula, Atom}
import scala.Symbol

/**
 * Created by IntelliJ IDEA.
 * User: Asus
 * Date: 13.05.13
 * Time: 09:41
 * To change this template use File | Settings | File Templates.
 */

object DPLLAlgorithm {
   type Leaf = Set[Formula]
//
    def buildAtomsInvSet(formulas: Map[Int,List[Leaf]]):  Map[Formula, List[Int]]={
        def merge(rec:List[(Formula, Int)], f: (Int,List[Leaf])): List[(Formula, Int)]=
            (rec++atomListForFormula(f._2,f._1)).distinct

        def atomListForFormula(f: List[Leaf],k: Int): List[(Formula, Int)] =
            f.flatten.map(a => (a, k))

        val groupedMap=formulas.toList.foldLeft(List[(Formula, Int)]()) (merge) groupBy (x => x._2)
        groupedMap.mapValues(pairs => pairs.map(numFormPair => numFormPair._2))
    }

    def getLiterals(atoms :List[Formula]): Set[scala.Symbol]=
        atoms.map( f => f match{
            case Atom(s) => s
            case ¬(Atom(s)) => s
        }).toSet

    def getPureLiterals(formulas: List[List[Leaf]],allLiterals: Set[scala.Symbol]): Set[scala.Symbol]={
        val formulasAtoms=formulas.flatten.flatten
        allLiterals.filterNot((s: Symbol) => (formulasAtoms contains (Atom(s))) && (formulasAtoms contains (¬(Atom(s)))))
    }
    

//
//    @tailrec
    def unitPropagation(k: Int,formulas: List[(Int,List[Leaf])],invSet: Map[Formula,List[Int]]): List[Formula]={
        val unitL: Formula=formulas(k).head
        val negUnitL: Formula= ¬(formulas(k).head)
        invSet(unitL).foreach({ind =>

        })

        formulas.map{ (p: (Int, List[Leaf])) =>
            p._1 match {
                case _ if(invSet(negUnitL).contains(_) && p._2!=Nil) => (p._1,p._2.filterNot(Set(unitL)))
                case _ if (invSet(unitL).contains(_)) => ((p._1),Nil)
                case _ => p
            }

        }
    }
    
    def dpll(formulas: List[Formula]): Boolean={

        val formIdx=formulas.zipWithIndex.map(x => (x._2,x._1))
        val atomsMap=buildAtomsInvSet(formulasMap)
        val literals=getLiterals(atomsMap.keySet.toList);

        formulas.foreach( f =>
            if(f._2.length==0) false
            else if(f._2.length==1){
                formulas=unitPropagation(k,formulas);
            }
        )


    }
}
