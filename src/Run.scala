/**
 * Created by IntelliJ IDEA.
 * User: Asus
 * Date: 24.03.13
 * Time: 19:42
 * To change this template use File | Settings | File Templates.
 */
package propositional

import examples._
import Formulas._
import nqueens.{PrintChessboard, NQueensConstrains}
import propositional.DPLLWithJavaUnitPropagation._

import scala.Tuple5
import com.sun.istack.internal.Pool.Impl
import collection.JavaConversions._


object Run {


    def main(args:Array[String]):Unit = {
        

//        val formula1: Formula = (¬('r) ∧ 'q) ∧ (¬('p ∧ 'q) ↔ (¬('p) ∨ ¬('q) ))
//        val formula2 = ¬('p ∧ 'q) ↔ (¬('p) ∨ ¬('q) )
//     //   println(FormulaInOut.formulaToString(formula1))
//        val clause1: List[Set[Formula]]=semanticTableauDisjunctive(formula1)
//        val clause2=semanticTableauDisjunctive(formula2)
//        println(dpll(clause1,Map[Formula,Boolean]()))
        
        //val atoms=buildAtomsInvSet(List(clause1,clause2) )
        //val allLiterals=getLiterals(atoms.keys.toList)
        //println("atoms: " + atoms.mkString(", "))
        //println("pure literals: "+ getPureLiterals(List(clause1,clause2),allLiterals).mkString(", "))
     //   println(isSatisfiable(formula1) + "," + isValid(formula1)) // true, false
//
//        val formula2 = ¬('p ∧ 'q) ↔ (¬('p) ∨ ¬('q) )
//        
//        println(isSatisfiable(formula2) + "," + isValid(formula2)) // true, true

       // testFormulaFromStr
       // testTautology()

        testNQueens(8);//args(0).toInt)
    }





    def testNQueens(fieldSize: Int){
        val constraints: List[Set[Formula]]= NQueensConstrains.getConstrains(fieldSize)
//        constraints.foreach{ cl =>
//            println(FormulaInOut.formulaToString(cl))
//
//        }

        println("Field size:"+fieldSize)
        val start=System.currentTimeMillis()
        val solution= dpll(constraints)
        val workTime=System.currentTimeMillis()-start;
        solution match{
            case Some(m) => PrintChessboard.printSolution(m,fieldSize)
            case None => print("No solution found")
        }
        println("Work time = " + workTime+" ms")
    }
}