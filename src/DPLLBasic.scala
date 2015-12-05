package propositional

import propositional.Atom._
import propositional.{¬, Atom, Formula}
import propositional.¬._

/**
 * Created by IntelliJ IDEA.
 * User: Asus
 * Date: 22.06.13
 * Time: 18:23
 * To change this template use File | Settings | File Templates.
 */

object DPLLBasic {
    type Clause = Set[Formula]

    def getLiterals(atoms: List[Formula]): Set[scala.Symbol] =
        atoms.map(f => f match {
            case Atom(s) => s
            case ¬(Atom(s)) => s
        }).toSet

    implicit def formulaToString(f: Formula): String ={
        FormulaInOut.formulaToString(f)
    }
    def printFormulas(formulas: List[Clause]){
        formulas.foreach{ fSet=>
            print("(")
            print(fSet.mkString(" ∨ "))
            println(") ∧")
        }
        println()
    }
    def negate(f: Formula): Formula={
        ¬(f) match{
            case ¬(¬(at)) => at
            case at => at
        }
    }
    def unitPropagation(unitL: Formula,formulas: List[Clause]): List[Clause]={
//        println("----------------------------------")
//        println("unit propagation of "+ unitL)
//        println("Before:")
//        printFormulas(formulas)

        if(formulas.isEmpty) return formulas

        formulas.head match {
            case f if (f contains unitL)  =>{
                //println("--> "+ f+ " contains "+ unitL)
                unitPropagation(unitL,formulas.tail)
            }
            case f if (f contains negate(unitL)) => {
                //println("--> "+ f+ " contains "+ ¬(unitL))
                val reduced=f.filterNot(lit=> lit==(negate(unitL)))

                if(reduced.isEmpty){
                    Set[Formula]()+:unitPropagation(unitL,formulas.tail)
                }
                else{
                    reduced+:unitPropagation(unitL,formulas.tail)
                }

            }
            case f => f+:unitPropagation(unitL,formulas.tail)
        }

    }

    def selectLiteral[Atom <: Formula](formulas: List[Clause]): Formula ={
        val atoms: List[Formula]=formulas.flatten
        val atomsCounts=atoms groupBy (a => a) map(el => (el._1,el._2.length))

        val lit=atomsCounts.maxBy(_._2)._1
        lit match{
            case (¬(¬(Atom(s)))) => Atom(s)
            case _ => lit
        }
    }


    def dpll(formulas: List[Clause],inferredVal: Map[Formula,Boolean]=Map[Formula,Boolean]()): Option[Map[Formula,Boolean]]={
        formulas.find(cl => cl.size==1) match{
            case Some(f) => {
               // println("---------> Unit found: "+f)

                f.toList.head match{
                    case at@Atom(s) => return dpll(unitPropagation(at,formulas),inferredVal+(Atom(s)->true))
                    case at@(¬(Atom(s))) => return dpll(unitPropagation(at,formulas),inferredVal+(Atom(s)->false))
                }
            }
            case None =>{
                if(formulas==Nil || formulas.isEmpty) return Some(inferredVal);
                if(formulas.exists((cl: Clause) => cl.isEmpty))
                     return None;
                val literal: Formula=selectLiteral(formulas);
              //  println("---------> Literal selected: "+literal)
                dpll(formulas:+Set(literal),inferredVal) match{
                    case valuation@Some(_) => return valuation
                    case _ => {
                        val negated: Formula= negate(literal)
                        return dpll(formulas:+Set(negated),inferredVal)
                    }
                }



            } 
        }
    }
}
