package propositional.examples

/**
 * Created by IntelliJ IDEA.
 * User: Asus
 * Date: 23.06.13
 * Time: 15:52
 * To change this template use File | Settings | File Templates.
 */

trait Tautology {
    def formulaStr: String
}

case object ImplicationTrans extends Tautology{
    def formulaStr(): String = "(alfa -> beta) -> ((beta->gamma)-> ( alfa ->beta))"
}

case object T2 extends Tautology{
    def formulaStr(): String = "(alfa -> (alfa | beta))"
}

case object T3 extends Tautology{
    def formulaStr(): String = "(alfa -> (beta | alfa))"
}

case object T4 extends Tautology{
    def formulaStr(): String = "(alfa & beta)-> (beta)"
}

case object T5 extends Tautology{
    def formulaStr(): String = "(alfa -> gamma)-> ((beta-> gamma)->((alfa | beta)-> gamma))"
}
