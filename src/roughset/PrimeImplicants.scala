package roughset

import propositional._


/**
 * Created by IntelliJ IDEA.
 * User: Asus
 * Date: 31.07.13
 * Time: 16:15
 * To change this template use File | Settings | File Templates.
 */

object PrimeImplicants {
    type Clause = Set[Formula]

    implicit def clauseToFormula(cl: Clause): Formula = {
        if(cl.size==1) return cl.head
        else return cl.head ∨ clauseToFormula(cl.tail)
    }
    
    implicit def formulaToString(f: Formula): String ={
        return FormulaInOut.formulaToString(f)
    }
    
    implicit def atomToString(a: Atom): String={
        return a.symbol.name
    }

    implicit def listToClause(l: List[Clause]): Formula = {
        if(l.isEmpty) return Atom(Symbol(""))
        if(l.size==1) return clauseToFormula(l.head)
        else return clauseToFormula(l.head) ∧ listToClause(l.tail)
    }

    def investigateDiffSet(hd: Clause,tl: List[Clause]): Boolean={
        if(tl.isEmpty) return true;
        tl.foreach{f =>
            if(hd subsetOf f){
                sys.error(hd+ " is Subset of "+ f)
                return false
            }
            else{
                return investigateDiffSet(tl.head,tl.tail)
            }
        }
        return false;

    }

    def primeImplicantsToReducts(primeImplicants: List[Clause]): List[Clause]={
       return Formulas.semanticTableauDisjunctive(primeImplicants)
    }

    def getDifferFunction(dataset: List[(Clause,Atom)]): List[Clause] = {
        if(dataset.isEmpty) return Nil

        val differents=findDifferents(dataset.head,dataset.tail)
        val diffSet=getDifferFunction(dataset.tail)

       val filteredDiffset=diffSet.filterNot(f => differents.exists((s: Clause) => (s subsetOf f) & s!=f)).distinct

        if(filteredDiffset.size>=2)
            investigateDiffSet(diffSet.head,diffSet.tail)
        //return filteredDiffset ++ differents
        return (filteredDiffset ++ differents.filterNot(f => filteredDiffset.exists((s: Clause) => (s subsetOf f) & s!=f))).distinct
    }

    def implicationsToImplicants(implCl: Clause): Clause={

        return implCl.map{f => f
            f match{
                case Implication(prec,succ) => prec
                case _ => f
                }
            }
    }

    def reductToString(reduct: Set[Formula]): String ={

        return (reduct.map{ at=>  formulaToString(at)}).mkString(" & ")
    }
    def findDifferents(f: (Clause,Atom),latter: List[(Clause,Atom)]): List[Clause] ={
        def reduceToMinimal(diffSet: List[Clause]): List[Clause]={
            if(diffSet.size<=1) return diffSet
            val red=diffSet.tail.filterNot(f => (diffSet.head subsetOf f) & (f!=diffSet.head))

            return diffSet.head +: reduceToMinimal(red)
        }
        
        if(latter.isEmpty) return Nil
        if(latter.head._2==f._2) return findDifferents(f,latter.tail)
        else{  //classes are different
            val diffF: Clause=implicationsToImplicants(f._1.diff(latter.head._1) ++ latter.head._1.diff(f._1))

            val nextDifferents = findDifferents(f,latter.tail)
            if(diffF.isEmpty){ return nextDifferents}//sys.error("inconsistent dataset: "+ latter.head +" and " + f)}
//            println(RuleGenerator.ruleToString(f)+"-"+RuleGenerator.ruleToString(latter.head)+"="+)
            if(nextDifferents.exists(s => (s subsetOf diffF) & s!=diffF )) {
                return reduceToMinimal(nextDifferents)
            }
            return reduceToMinimal(diffF +: nextDifferents).distinct
        }
    }
}
