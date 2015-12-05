package propositional.nqueens


import propositional.{Formula,Atom}
import scala.util.matching.Regex


/**
 * Created by IntelliJ IDEA.
 * User: Asus
 * Date: 27.06.13
 * Time: 02:57
 * To change this template use File | Settings | File Templates.
 */

object PrintChessboard {

   def getQueensPos(valuation: List[(Formula,Boolean)]): List[(Int, Int)] ={
       val v=valuation.head
       if(v._2){
           v._1 match{
               case Atom(s) =>{
                   val pattern: Regex = """\d+""".r
                   val it=pattern.findAllIn(s.toString())
                   val p: (Int,Int) = (it.next.toInt,it.next.toInt)
                   print(p+" ")
                   if (!valuation.tail.isEmpty) return p +: getQueensPos(valuation.tail)
                   else  return List(p)
               }
           }
       }

       if(!valuation.tail.isEmpty) getQueensPos(valuation.tail)
       else Nil
   } 
    
   def printSolution(valuation: Map[Formula,Boolean],fieldSize: Int) {
      val queensPos=getQueensPos(valuation.toList);
       println()
       for(r <- (1 to fieldSize)){
           print("|")
          (1 to fieldSize).foreach{c=>
              if(queensPos contains (r,c)) print(" x |")
              else print(" o |")
          }
          println() 
       }
   }
}
