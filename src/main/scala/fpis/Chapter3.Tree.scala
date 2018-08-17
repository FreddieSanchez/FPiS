package fpis.Chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    // EXERCISE 3.25
    // Write a function size that counts the number of nodes (leaves and branches) in a tree.
    def size[A](t:Tree[A]): Int = 
      t match { 
        case Leaf(_) => 1
        case Branch(left, right) => 1 + size(left) + size(right)
      }

    // EXERCISE 3.26
    // Write a function maximum that returns the maximum element in a Tree[Int] . (Note:
    // In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x
    // and y .)
    def max(t:Tree[Int]): Int = 
      t match { 
        case Leaf(x) => x 
        case Branch(left, right) => max(left) max max(right)
      }

    // EXERCISE 3.27
    // Write a function depth that returns the maximum path length from the root of a tree
    // to any leaf.
    def depth[A](t:Tree[A]): Int =
        t match { 
          case Leaf(x) => 0
          case Branch(left, right) => depth(left) max depth(right)
        }

    // EXERCISE 3.28
    // Write a function map , analogous to the method of the same name on List , that modi-
    // fies each element in a tree with a given function.
    def map[A,B](t:Tree[A])(f: A => B):Tree[B] = 
        t match { 
          case Leaf(x) => Leaf(f(x))
          case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        }

    //  EXERCISE 3.29
    //  Generalize size , maximum , depth , and map , writing a new function fold that abstracts
    //  over their similarities. Reimplement them in terms of this more general function. Can
    //  you draw an analogy between this fold function and the left and right folds for List ?
    def fold[A,B](t:Tree[A])(f:A => B)(g:(B,B) => B): B = 
        t match { 
          case Leaf(x) => f(x)
          case Branch(left, right) =>  g(fold(left)(f)(g), fold(right)(f)(g))
        }

    def sizeViaFold[A](t:Tree[A]):Int = 
      fold(t)(a => 1)(1+ _ + _ )

    def maximumViaFold(t:Tree[Int]):Int = 
      fold(t)(a => a)(_ max _)

    def depthViaFold[A](t:Tree[A]):Int = 
      fold(t)(a => 0)(_ max _)

    def mapViaFold[A,B](t:Tree[A])(f: A => B):Tree[B] = 
      fold(t)(x => Leaf(f(x)):Tree[B])(Branch(_,_))
}
    
