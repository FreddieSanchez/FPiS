package fpis.Chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    // EXERCISE 3.25
    // Write a function size that counts the number of nodes (leaves and branches) in a tree.
    def count[A](t:Tree[A]): Int = 
      t match { 
        case Leaf(_) => 1
        case Branch(left, right) => 1 + count(left) + count(right)
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
}
    
