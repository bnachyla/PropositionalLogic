package propositional

import propositional.utils.FormulaCollection
import collection.JavaConversions._
import collection.JavaConverters

/**
 * Created by IntelliJ IDEA.
 * User: Asus
 * Date: 22.06.13
 * Time: 18:23
 * To change this template use File | Settings | File Templates.
 */

object DPLLWithJavaUnitPropagation {
    type Clause = Set[Formula]

    def getLiterals(atoms: List[Formula]): Set[scala.Symbol] =
        atoms.map(f => f match {
            case Atom(s) => s
            case ¬(Atom(s)) => s
        }).toSet

    implicit def formulaToString(f: Formula): String = {
        FormulaInOut.formulaToString(f)
    }

    def printFormulas(formulas: List[Clause]) {
        formulas.foreach {
            fSet =>
                print("(")
                print(fSet.mkString(" ∨ "))
                println(") ∧")
        }
        println()
    }

    def negate(f: Formula): Formula = {
        ¬(f) match {
            case ¬(¬(at)) => at
            case at => at
        }
    }


    def selectLiteral[Atom <: Formula](formulas: List[Clause]): Formula = {
        val atoms: List[Formula] = formulas.flatten
        val atomsCounts = atoms groupBy (a => a) map (el => (el._1, el._2.length))

        val lit = atomsCounts.maxBy(_._2)._1
        lit match {
            case (¬(¬(Atom(s)))) => Atom(s)
            case _ => lit
        }
    }

    implicit def boolMapJavaToScala(javamap: Map[Formula, java.lang.Boolean]): Map[Formula, Boolean]={

        return javamap.foldLeft(Map[Formula, Boolean]()){(m: Map[Formula, Boolean],f: (Formula,java.lang.Boolean)) => m+(f._1 -> Boolean2boolean(f._2))}
    }

    def dpllWithJavaPropagation(formCollect: propositional.utils.FormulaCollection): Option[Map[Formula, Boolean]] = {
        if (formCollect.isEmpty()) return Some(JavaConverters.mapAsScalaMapConverter(formCollect.getInferredValues).asScala.toMap);
        if (formCollect.hasEmptyFormula())
            return None;
        val literal: Formula = formCollect.selectLiteral();

        dpllWithJavaPropagation(formCollect.addUnit(literal)) match {
            case valuation@Some(_) => return valuation
            case _ => {
                val negated: Formula = negate(literal)
                return dpllWithJavaPropagation(formCollect.addUnit(negated))
            }
        }
    }

    def dpll(formulas: List[Set[Formula]]): Option[Map[Formula, Boolean]] = {
        val formCollect: propositional.utils.FormulaCollection = new propositional.utils.FormulaCollection(formulas)
        return dpllWithJavaPropagation(formCollect);
    }
}
