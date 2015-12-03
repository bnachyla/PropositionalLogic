package propositional

import collection.JavaConversions._
import collection.JavaConverters

/**
 * Created by IntelliJ IDEA.
 * User: Asus
 * Date: 29.07.13
 * Time: 11:15
 * To change this template use File | Settings | File Templates.
 */
sealed abstract class Formula {
    def ∧(q:Formula) = Conjunction(this, q)

    def ∨(q:Formula) = Disjunction(this,q)

    def →(q:Formula) = Implication(this,q)

    def ↔(q:Formula) = Equivalence(this,q)

    def ⊕(q:Formula) = Xor(this, q)
}
case class Atom(symbol:Symbol) extends Formula

object Atom{
    def isNegatedAtom(a: Formula):Boolean = {
        a match{
            case ¬(Atom(_)) => true
            case _ => false
        }
    }
    def negate(a: Formula):Formula = {
        a match{
            case ¬(f) => f
            case f => ¬(f)
        }
    }
    
    def duplicate(a: Formula): Formula ={
        a match{
            case a@Atom(s) =>  a.copy(s)
            case (¬(a@Atom(s))) => ¬(a.copy(s))
        }
    }

    def duplicate(f: java.util.Set[Formula]): Set[Formula] ={
        duplicateScalaSet(f);
    }

    def duplicateScalaSet(f: scala.collection.mutable.Set[Formula]): Set[Formula] ={
        if(f.isEmpty) return Set[Formula]()
        else {
            val headCp = f.head match{
                case a@Atom(s) =>  a.copy(s)
                case (¬(a@Atom(s))) => ¬(a.copy(s))
            }

            return duplicateScalaSet(f.tail) + headCp
        }
    }


    def filterFrom(a: Formula,f: java.util.Set[Formula]): java.util.Set[Formula] ={
        if(f==null)
            return f;

        return f.filterNot(lit => lit==a)
    }

    def toScalaBool(b: java.lang.Boolean): Boolean={
        if(b==java.lang.Boolean.TRUE) return true;
        else return false;
    }



}

case class Conjunction(p:Formula, q:Formula) extends Formula
case class Disjunction(p:Formula, q:Formula) extends Formula
case class Implication(p:Formula, q:Formula) extends Formula
case class Equivalence(p:Formula, q:Formula) extends Formula
case class Xor(p:Formula,q:Formula) extends Formula
case class ¬(p:Formula) extends Formula

