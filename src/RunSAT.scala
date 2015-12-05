import propositional.files.{ReadLogicPropositions, ReadDataset}
import propositional.Formulas._
import propositional.{FormulaInOut, FormulaParser, Formula}
import scala.Predef._

/**
 * Created by IntelliJ IDEA.
 * User: Asus
 * Date: 04.08.13
 * Time: 12:30
 * To change this template use File | Settings | File Templates.
 */

object RunSAT {
    type Clause = Set[Formula]

    def main(args: Array[String]): Unit = {
        val coding= args.size>1 match{
            case true => args(1)
            case _ => "iso-8859-1"
        }

        val formulasToCheck=ReadLogicPropositions.read(args(0),coding);

        formulasToCheck.foreach(f =>{
            println (FormulaInOut.formulaToString(f ))
            isSatisfiable(f) match{
                case true => println("- is Satisfable")
                case _ => println("- is Not Satisfable")
            }

            isValid(f) match{
                case true => println("- is Valid")
                case _ => println("- is Not Valid")
            }
            println()
        })

    }

}