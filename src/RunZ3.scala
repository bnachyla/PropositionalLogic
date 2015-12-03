import propositional.files.ReadDataset
import propositional.{Implication, Atom, Formula, FormulaInOut}
import roughset.{RuleGenerator, PrimeImplicants}

/**
 * Created by IntelliJ IDEA.
 * User: Asus
 * Date: 31.07.13
 * Time: 14:24
 * To change this template use File | Settings | File Templates.
 */

object RunZ3 {
    type Clause = Set[Formula]

    def main(args: Array[String]): Unit = {
        val dataset=ReadDataset.read(args(0),false);
        val primeImpl= PrimeImplicants.getDifferFunction(dataset)

        println("Prime implicants:")

        primeImpl.foreach{ d=>
            println(FormulaInOut.formulaToString(d));
        }
        

        val reducts: List[Set[Formula]]= PrimeImplicants.primeImplicantsToReducts(primeImpl)

        reducts.foreach{r =>
            println("--------------------------------------")
            println("rules for reduct: "+PrimeImplicants.reductToString(r))
            RuleGenerator.validateReduct(dataset,r)
            val rules=RuleGenerator.generateRules(dataset,r).distinct

            RuleGenerator.printRules(rules)
        }

    }


    def reductsValidation(reduct: Clause,dataset: List[(Clause,Atom)]) {
        println("-------------------------------")
        println(FormulaInOut.formulaToString(reduct))
        dataset.foreach{  r =>
            if(reduct subsetOf r._1)
                println(FormulaInOut.formulaToString(r._2))
            
        }
    }

}
