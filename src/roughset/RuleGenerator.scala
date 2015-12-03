package roughset

import propositional.{FormulaInOut, Implication, Formula, Atom}


/**
 * Created by IntelliJ IDEA.
 * User: Asus
 * Date: 03.08.13
 * Time: 18:29
 * To change this template use File | Settings | File Templates.
 */

object RuleGenerator {
    type Clause = Set[Formula]
    
    def generateRules(dataset: List[(Clause,Atom)],reduct: Set[Formula]): List[(Clause,Atom)] ={
        if(dataset.isEmpty) return Nil
        
        val rule: Clause=dataset.head._1.filter{(f: Formula) =>
            f match {
                case Implication(id@Atom(_),Atom(Symbol(value))) =>
                    reduct.contains(id)
                case _ => false
                }
            }

        return (rule,dataset.head._2) +: generateRules(dataset.tail,reduct)
    }
    
    def printRules(rules: List[(Clause,Atom)]){
        rules.foreach( r =>{
                print("[ ")
                r._1.foreach( (attr: Formula) =>
                    attr match{
                        case Implication(prec,succ) => print(FormulaInOut.formulaToString(prec)+":"+FormulaInOut.formulaToString(succ)+" ")
                    }
                )
                print("] -> ")
                println(FormulaInOut.formulaToString(r._2))
            })

    }
    
    implicit def ruleToString(rule: (Clause,Atom)): String ={
        return "[ " +
        rule._1.map( (attr: Formula) =>
            attr match{
                case Implication(prec,succ) => FormulaInOut.formulaToString(prec)+":"+FormulaInOut.formulaToString(succ)+" "
            }
        ) +
        "] -> " +
        FormulaInOut.formulaToString(rule._2)
    }
    def applyRule(record: Clause,reduct: Clause): Clause={
        return record.filter{(f: Formula) =>
            f match {
                case Implication(id@Atom(_),Atom(_)) =>
                    reduct.contains(id)
                case _ => false
            }
        }
    }
    
    def validateReduct(dataset: List[(Clause,Atom)],reduct: Set[Formula]): Boolean= {
        if(dataset.size<=1) return true;
        val hReduced=applyRule(dataset.head._1,reduct)
        dataset.tail.foreach{ d =>
            if(dataset.head._2!=d._2){
                val dReduced=applyRule(d._1,reduct)
                if(hReduced==dReduced & dataset.head._1!=d._1){
                    sys.error("Different classes for the same rule: "+ ruleToString((hReduced,dataset.head._2))+", "+ruleToString((dReduced,d._2)))
                }
            }
        }

        return validateReduct(dataset.tail,reduct)
    }


}
