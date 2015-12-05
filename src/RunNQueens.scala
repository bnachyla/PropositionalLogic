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


object RunNQueens {


    def main(args:Array[String]):Unit = {

        testNQueens(args(0).toInt)
    }





    def testNQueens(fieldSize: Int){
        val constraints: List[Set[Formula]]= NQueensConstrains.getConstrains(fieldSize)

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