package propositional.files

import scala.io.Source
import propositional.{Implication, Formula, Atom}


/**
 * Created by IntelliJ IDEA.
 * User: Asus
 * Date: 31.07.13
 * Time: 14:06
 * To change this template use File | Settings | File Templates.
 */

object ReadDataset {
    type Clause = Set[Formula]

    
    def generateHeader(attrNum: Int): List[String] = (1 to attrNum).foldLeft(List[String]()){(l: List[String], attrId: Int) => l:+ attrId.toString}

    def read(filePath: String,hasHeader: Boolean = true): List[(Clause,Atom)]={
        val lines=Source.fromFile(filePath,"iso-8859-1").getLines().toList

        if(hasHeader){
            val header=lines.head

            return processLines(lines.tail,header.split(" ").tail.dropRight(1).toList)// drop first and last
        }
        else{
            val header=generateHeader(lines.head.length-2)

            return processLines(lines,header)
        }
        
    }

    def processLines(lines: List[String],attrNames: List[String]): List[(Clause,Atom)] ={
        if(lines.isEmpty) return Nil
        else{
            val tokens = lines.head.split(" ")
            if (tokens.length<=1) return  processLines(lines.tail,attrNames)

            val attrs = tokens.tail.dropRight(1).toList;   //skip first token -> the part of an entire text, and the last -> a class

            val category = tokens.toList.last;
            val attrClause=(attrNames zip attrs).foldLeft(Set[Formula]()){(f: Clause, at: (String,String)) => f+Implication(Atom(Symbol(at._1)),Atom(Symbol(at._2)))}

            return (attrClause,Atom(Symbol(category)))+: processLines(lines.tail,attrNames)
        }


    }

}
