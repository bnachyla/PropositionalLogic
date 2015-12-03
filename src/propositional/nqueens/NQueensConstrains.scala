package propositional.nqueens

import propositional._


/**
 * Created by IntelliJ IDEA.
 * User: Asus
 * Date: 24.06.13
 * Time: 14:03
 * To change this template use File | Settings | File Templates.
 */

object NQueensConstrains {
    type Clause = Set[Formula]

    def getConstrains(fieldSize: Int): List[Clause] ={
        getConstrForOnePerRow(fieldSize) :::
        getConstrForOnePerCol(fieldSize) :::
        getConstrAtMostOneInRow(fieldSize) :::
        getConstrAtMostOneInCol(fieldSize) :::
        getConstrAtMostOneInDiagLeft(fieldSize) :::
        getConstrAtMostOneInDiagRight(fieldSize)

    }

    def atLeastOne(row: Int, col: Int): Formula = Atom(Symbol("[%d][%d]".format(row,col)))

    def getConstrForOnePerRow(fieldSize: Int): List[Clause] ={
        (1 to fieldSize).map{r => listToClause(((1 to fieldSize).map{c =>
            atLeastOne(r,c)}) toList)}.toList
    }

    def getConstrForOnePerCol(fieldSize: Int): List[Clause] ={
        (1 to fieldSize).map{c =>
            listToClause(((1 to fieldSize).map{r =>
                atLeastOne(r,c)}) toList)
        }.toList
    }


    def listToClause(l: List[Formula]): Clause = l.toSet


    def atMostOne(p1: (Int, Int),p2: (Int,Int)): Clause =
        Set(¬(Atom(Symbol("[%d][%d]".format(p1._1,p1._2)))), ¬(Atom(Symbol("[%d][%d]".format(p2._1,p2._2)))))


    def getConstrAtMostOne(pos: List[(Int, Int)]): List[Clause] = {
        if(pos.length==2)  List[Clause](atMostOne(pos.head, pos.tail.head))
        else (pos.tail.map(p => atMostOne(pos.head, p))).toList ++ getConstrAtMostOne(pos.tail)
    }
    
    def getConstrAtMostOneInRow(fieldSize: Int): List[Clause] =
        (1 to fieldSize).map{r =>
            getConstrAtMostOne((for (c<-1 to fieldSize) yield (r,c)).toList)
        }.toList.flatten
    

    def getConstrAtMostOneInCol(fieldSize: Int): List[Clause] ={
        (1 to fieldSize).map{c =>
            getConstrAtMostOne((for (r<-1 to fieldSize) yield (r,c)).toList)
        }.toList.flatten
    }

    def getConstrAtMostOneInDiagRight(fieldSize: Int): List[Clause] ={
        (-(fieldSize-2) to fieldSize-2).map{ diff =>
            getConstrAtMostOne((for(r<-(1 to fieldSize);c<-(1 to fieldSize) if(r-c==diff)) yield(r,c)) toList)
        }.toList.flatten
    }

    def getConstrAtMostOneInDiagLeft(fieldSize: Int): List[Clause] ={
        (3 to fieldSize*2-1).map{ sum =>
            getConstrAtMostOne((for(r<-(1 to fieldSize);c<-(1 to fieldSize) if(r+c==sum)) yield(r,c)) toList)
        }.toList.flatten
    }

    def getConstrAtMostOneInDiagLeftRev(fieldSize: Int): List[Clause] ={
        (3 to fieldSize*2-1).map{ sum =>
            getConstrAtMostOne((for(r<-(1 to fieldSize);c<-(1 to fieldSize) if(r+c==sum)) yield(c,r)) toList)
        }.toList.flatten
    }
}
