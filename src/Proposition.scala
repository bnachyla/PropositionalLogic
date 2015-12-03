/**
 * Created by IntelliJ IDEA.
 * User: Asus
 * Date: 24.03.13
 * Time: 19:37
 * To change this template use File | Settings | File Templates.
 */

package propositional

import scala.annotation.tailrec


object Formulas {
    implicit def symbolToAtom(sym:Symbol) = Atom(sym)
    type Leaf = Set[Formula]


    def applyRule2DisjunctiveNormal(f:Formula):List[Leaf] =
        f match {
            case f if isLiteral(f) => List(Set(f))

            case ¬(¬(a)) => List(Set(a))

            case Conjunction(a,b) => List(Set(a,b))

            case ¬(Disjunction(a,b)) => List(Set(¬(a), ¬(b)))

            case ¬(Implication(a,b)) => List(Set(a, ¬(b)))

            case Disjunction(a,b) => List(Set(a), Set(b))

            case ¬(Conjunction(a,b)) => List(Set(¬(a)), Set(¬(b)))

            case (Implication(a,b)) => List(Set(¬(a)), Set(b))

            case Equivalence(a,b) => List( Set(a,b) , Set(¬(a), ¬(b)))

            case ¬(Equivalence(a,b)) => List(Set(a,¬(b)),Set(¬(a), b))
        }

    def applyRule2ConjunctiveNormal(f:Formula):List[Leaf] =
        f match {
            case f if isLiteral(f) => List(Set(f))

            case ¬(¬(a)) => List(Set(a))

            case Conjunction(a,b) => List(Set(a),Set(b))

            case ¬(Disjunction(a,b)) => List(Set(¬(a)), Set(¬(b)))

            case ¬(Implication(a,b)) => List(Set(a), Set(¬(b)))

            case Disjunction(a,b) => List(Set(a,b))

            case ¬(Conjunction(a,b)) => List(Set(¬(a),¬(b)))

            case (Implication(a,b)) => List(Set(¬(a),b))

            case Equivalence(a,b) => List( Set(a,¬(b)) , Set(¬(a),b ))

            case ¬(Equivalence(a,b)) => List(Set(a,b),Set(¬(a), ¬(b)))
    }

    def isLiteral(f:Formula):Boolean =
        f match {
            case Atom(_) => true
            case ¬(Atom(_)) => true
            case _ => false
        }

    def semanticTableauConjunctive(f:Formula):List[Leaf] = {

        def combine(rec:List[Leaf], f:Formula):List[Leaf] =
            for ( a <- applyRule2ConjunctiveNormal(f); b <- rec) yield (a ++ b)

        def openLeaf(leaf:Leaf):List[Leaf] =
            if (leaf forall isLiteral)
                List(leaf)
            else
                leaf.foldLeft(List(Set.empty:Leaf))(combine) flatMap(openLeaf)

        openLeaf(Set(f))
    }

    def semanticTableauDisjunctive(f:Formula):List[Leaf] = {

        def combine(rec:List[Leaf], f:Formula):List[Leaf] =
            for ( a <- applyRule2DisjunctiveNormal(f); b <- rec)
                yield (a ++ b)

        def openLeaf(leaf:Leaf):List[Leaf] =
            if (leaf forall isLiteral)
                List(leaf)
            else
                leaf.foldLeft(List(Set.empty:Leaf))(combine) flatMap(openLeaf)

        openLeaf(Set(f))
    }

    @tailrec
    def isClosedLeaf(f:Leaf):Boolean = {
        if (f.isEmpty) false
        else {
            (f.head match {
                case Atom(_) => f.tail.exists( _ == ¬(f.head))
                case ¬(Atom(a)) => f.tail.exists( _ == Atom(a))
                case _ => false
            }) || isClosedLeaf(f.tail)
        }
    }

    def isOpenLeaf(f:Leaf) = !isClosedLeaf(f)

    def isValid(f:Formula):Boolean = semanticTableauConjunctive(f) forall isClosedLeaf

    def isSatisfiable(f:Formula):Boolean = semanticTableauDisjunctive(f) exists isOpenLeaf


}