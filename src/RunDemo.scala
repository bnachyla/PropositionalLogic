import propositional._
import propositional.Formulas._
import propositional.{FormulaInOut, FormulaParser, Formula}
import scala.Predef._

/**
 * Created by beata on 05.12.15.
 */
object RunDemo {

  def main(args:Array[String]):Unit = {

    val formula1: Formula = (¬('r) ∧ 'q) ∧ (¬('p ∧ 'q) ↔ (¬('p) ∨ ¬('q) ))
    print(FormulaInOut.formulaToString(formula1)+":")
    isSatisfiable(formula1) match{ //true
      case true => print(" is Satisfable")
      case _ => print(" is Not Satisfable")
    }
    print(" and")
    isValid(formula1) match{ //false
      case true => println(" is Valid")
      case _ => println(" is Not Valid")
    }


    val formula2 = ¬('p ∧ 'q) ↔ (¬('p) ∨ ¬('q) )
    val clause2=semanticTableauDisjunctive(formula2)

    print(FormulaInOut.formulaToString(formula2)+":")
    isSatisfiable(formula2) match{ //true
      case true => print(" is Satisfable")
      case _ => print(" is Not Satisfable")
    }
    print(" and")
    isValid(formula2) match{ //true
      case true => println(" is Valid")
      case _ => println(" is Not Valid")
    }



  }



}
