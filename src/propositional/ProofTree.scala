package propositional

/**
 * Created by IntelliJ IDEA.
 * User: Asus
 * Date: 24.06.13
 * Time: 18:24
 * To change this template use File | Settings | File Templates.
 */
/* contravariance required so Empty can be used with Nodes ( Nothing <: T ) */
class Tree[+T]{
    case object Empty extends Tree[Nothing]
    case class Node[T](val elem: T, val left: Tree[T], val right: Tree[T]) extends Tree[T]

    def inOrder[T](t: Tree[T]): List[T] = t match {
        case Empty => Nil
        case Node(e, left, right) => inOrder(left) ::: List(e) ::: inOrder(right)
    }

    def preOrder[T](t: Tree[T]): List[T] = t match {
        case Empty => Nil
        case Node(e, left, right) => e :: preOrder(left) ::: preOrder(right)
    }

    def postOrder[T](t: Tree[T]): List[T] = t match {
        case Empty => Nil
        case Node(e, left, right) => postOrder(left) ::: postOrder(right) ::: List(e)
    }

    def size[T](t: Tree[T]): Int = t match {
        case Empty => 0
        case Node(e, left, right) => 1 + size(left) + size(right)
    }

    def leafCount[T](t: Tree[T]): Int = t match {
        case Empty => 0
        case Node(e, Empty, Empty) => 1
        case Node(e, left, right) => leafCount(left) + leafCount(right)
    }

    def leaves[T](t: Tree[T]): List[T] = t match {
        case Empty => Nil
        case Node(e, Empty, Empty) => List(e)
        case Node(e, left, right) => leaves(left) ::: leaves(right)
    }

    def height[T](t: Tree[T]): Int = t match {
        case Empty => 0
        case Node(e, left, right) => 1 + math.max(height(left), height(right))
    }
}
class ProofTree extends Tree[Formula]{

}
