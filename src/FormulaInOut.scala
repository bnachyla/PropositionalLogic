package propositional

import propositional._


/**
 * Created by IntelliJ IDEA.
 * User: Asus
 * Date: 01.04.13
 * Time: 23:31
 * To change this template use File | Settings | File Templates.
 */

object FormulaInOut {
    type Clause = Set[Formula]

    implicit def clauseToFormula(cl: Clause): Formula = {
       if(cl.size==1) return cl.head
       else return cl.head ∨ clauseToFormula(cl.tail)
    }

    implicit def formulaToString(cl: Clause): String={
        return formulaToString(clauseToFormula(cl))
    }

    implicit def formulaToString(f: Formula): String =
        f match {
            case Atom(s) => s.toString()

            case ¬(Atom(s)) => "-"+s.toString()

            case ¬(a) => "-("+ formulaToString(a)+ ")"

            case Conjunction(a,b) => "( "+ formulaToString(a) + " & " + formulaToString(b) + ")"

            case Disjunction(a,b) => "( "+ formulaToString(a) + " | "+  formulaToString(b) + ")"

            case (Implication(a,b)) => "( "+ formulaToString(a) + " => " + formulaToString(b) + ")"

            case Equivalence(a,b) => "( "+ formulaToString(a) + " <=> "  + formulaToString(b) + ")"

        }


//    implicit def formulaFromString(s: String): Formula =
//        var ret: List[Formula]
//        s match {
//
//            case "&" => ret= Conjunction(ret.head,
//            case Conjunction(a,b) => "( "+ formulaToString(a) + " & " + formulaToString(b) + ")"
//
//            case Disjunction(a,b) => "( "+ formulaToString(a) + " | "+  formulaToString(b) + ")"
//
//            case (Implication(a,b)) => "( "+ formulaToString(a) + " => " + formulaToString(b) + ")"
//
//            case Equivalence(a,b) => "( "+ formulaToString(a) + " <=> "  + formulaToString(b) + ")"
//
//            case Atom(s) => s.toString()
//
//            case ¬(Atom(s)) => "-"+s.toString()
//
//            case ¬(a) => "-("+ formulaToString(a)+ ")"
//
//        }

}
