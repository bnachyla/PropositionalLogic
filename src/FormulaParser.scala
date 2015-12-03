package propositional



object FormulaParser {

    implicit def stringToAtom(sym:String): Symbol = Symbol(sym)

    def getSubformula(fStr: List[String],subfStr: List[String]=Nil,bracketCount: Int=1): (List[String],List[String])={
        fStr.headOption match{
            case None =>{
                sys.error("formula parse error: Bracket is not closed")
            }
            case Some("(") => getSubformula(fStr.tail,subfStr:+"(",bracketCount+1)
            case Some(")") =>{
                if(bracketCount-1==0) {
                     (fStr.tail,subfStr)
                }
                else getSubformula(fStr.tail,subfStr:+")",bracketCount -1)
            }
            case Some(s) => getSubformula(fStr.tail,subfStr:+s,bracketCount)
        }
    }

    def parseToRPN(f: List[String],stack: List[Formula]=List()): Formula={
        f.headOption match{
            case None => stack.head
            case Some("(") => {
                stack.headOption match{
                    case None => {
                        val splitted: (List[String],List[String])=getSubformula(f.tail)
                        if(!splitted._1.isEmpty){
                            parseToRPN(splitted._1,List(parseToRPN(splitted._2)))
                        }
                        else{
                            parseToRPN(splitted._2)
                        }

                    }
                    case Some(Atom(s)) => sys.error("formula parse error: %s(".format(s))
                    case Some(formula) => {
                        val splitted: (List[String],List[String])=getSubformula(f.tail)
                        val subf: Formula = parseToRPN(splitted._2,List())
                        formula match{
                            case ¬(arg1) => parseToRPN(splitted._1, ¬(subf)+:stack.tail)
                            case Conjunction(arg1,arg2) => parseToRPN(splitted._1, Conjunction(arg1,subf)+:stack.tail)
                            case Disjunction(arg1,arg2) => parseToRPN(splitted._1, Disjunction(arg1,subf)+:stack.tail)
                            case Implication(arg1,arg2) => parseToRPN(splitted._1, Implication(arg1,subf)+:stack.tail)
                            case Equivalence(arg1,arg2) => parseToRPN(splitted._1, Equivalence(arg1,subf)+:stack.tail)
                        }
                    }
                }
            }


            case Some(op) if(List("&","|","->","<->").contains(op)) =>{

                stack.headOption match{
                    case Some(formula) =>{
                        op match{
                            case "&" => parseToRPN(f.tail, Conjunction(formula,Atom("_"))+:stack.tail)
                            case "|" => parseToRPN(f.tail, Disjunction(formula,Atom("_"))+:stack.tail)
                            case "->" => parseToRPN(f.tail, Implication(formula,Atom("_"))+:stack.tail)
                            case "<->" => parseToRPN(f.tail, Equivalence(formula,Atom("_"))+:stack.tail)

                        }
                    }
                    case None => sys.error("formula parse error: formula cannot start with %s".format(op))
                }

            }

            case Some("-") =>  parseToRPN(f.tail, ¬(Atom("_"))+:stack.tail)

            case Some(t@_) => {
                stack.headOption match{
                    case Some(formula) =>{
                        formula match{
                            case ¬(arg1) => parseToRPN(f.tail, ¬(Atom(t))+:stack.tail)
                            case Conjunction(arg1,arg2) => parseToRPN(f.tail, Conjunction(arg1,Atom(t))+:stack.tail)
                            case Disjunction(arg1,arg2) => parseToRPN(f.tail, Disjunction(arg1,Atom(t))+:stack.tail)
                            case Implication(arg1,arg2) => parseToRPN(f.tail, Implication(arg1,Atom(t))+:stack.tail)
                            case Equivalence(arg1,arg2) => parseToRPN(f.tail, Equivalence(arg1,Atom(t))+:stack.tail)
                            case Atom(_) => {
                                sys.error("formula parse error: %s and %s has to be connected by an operand".format(FormulaInOut.formulaToString(formula),t))
                            }
                        }
                        
                    }
                    case None => parseToRPN(f.tail, Atom(t)+:stack)
                }
            }


        }
    }
    def split(f: String): List[String]={
        val operators:List[Char]=List('<','-','>','&','|','(',')',' ')
        
        if (f.startsWith("->")) "->"+:split(f.drop(2))
        else if (f.startsWith("<->")) "<->"+:split(f.drop(3))
        else f.headOption match{
            case Some(' ') => split(f.tail)
            case Some('&') => "&"+:split(f.tail)
            case Some('|') => "|"+:split(f.tail)
            case Some('-') => "-"+:split(f.tail)
            case Some('(') => "("+:split(f.tail)
            case Some(')') => ")"+:split(f.tail)
            case None => Nil
            case _ =>  {
                val literalName: String =f.takeWhile(p => !operators.contains(p))
                return literalName+:split(f.drop(literalName.length))
            }     
                
        }
    }
    def parse(f: String): Formula={
        parseToRPN(split(f))
    }
}
