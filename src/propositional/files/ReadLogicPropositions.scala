package propositional.files

import propositional.{FormulaParser, Formula}
import io.Source


/**
 * Created by IntelliJ IDEA.
 * User: Asus
 * Date: 04.08.13
 * Time: 12:40
 * To change this template use File | Settings | File Templates.
 */

object ReadLogicPropositions {
    
    def read(filePath: String,coding: String): List[Formula]={
        val lines=Source.fromFile(filePath,coding).getLines().toList
        return lines.map{formulaStr =>
            FormulaParser.parse(formulaStr)
        }
    }

}
