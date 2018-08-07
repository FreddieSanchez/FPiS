package fpis

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail:List[A]) extends List[A]

object List {

  def apply[A](as: A*):List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  def sum(lst: List[Int]): Int  =
    lst match {
      case Nil => 0
      case Cons(x, t) => x + sum(t)
    }
    

  // Exercise 3.1
  // should be 1 + 2 = 3
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  // Exercise 3.2
  // Implement the function tail for removing the firest element of a list. 
  // Note that the function takes constant time. What are the different 
  // choices you would make in your implementation if the LIst is Nil? We'll 
  // return to this question in the next chapter.
  def tail[A](lst: List[A]):List[A] = 
    lst match {
      case Nil => Nil
      case Cons(_, t) => t
    }

  // Exercise 3.3
  // Replace the first element of the list with the new value. 
  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Cons(h,Nil)
      case Cons(_, t) => Cons(h,t)
    }
    

  // Exercise 3.4
  // Generalize tail to a the function drop, which removes the firest n elements
  // from a list. Not ethat this function takes time proportional only to the
  // enumber of elements being dropped - we don't need to make a copy of the entire List. 
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else 
      l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }

  // Exercise 3.5
  // Implement dropWhile, which removes elements fromthe list prefix as 
  // long as they match a predicate
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
    l match { 
      case Nil => Nil
      case Cons(x, t) => 
        if (f(x)) 
          dropWhile(t, f)
        else 
          l
    }

  // Exercise 3.6
  // Implement a function init, that returns a List consisting of all but the last element of alist. 
  def init[A](l: List[A]): List[A] = 
    l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, t) => Cons(x, init(t))
    }


  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // Exercise 3.7
  //Can product , implemented using foldRight , immediately halt the recursion and
  // return 0.0 if it encounters a 0.0 ? Why or why not? Consider how any short-circuiting
  //might work if you call foldRight with a large list. This is a deeper question that we’ll
  //return to in chapter 5.
  //
  //No - the execution cannot be halted if a zero is encountered.
  //
  def product_old(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

 
  def product(ds: List[Double]): Double = 
    foldRight(ds, 1.0)(_*_)

  // Exercise 3.8
  // See what happens when you pass Nil and Cons themselves to foldRight , like this:
  // foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) . What do you think this
  // says about the relationship between foldRight and the data constructors of List ?
  //
  // It behaves he same way. 
  val y = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))


  //EXERCISE 3.9
  //Compute the length of a list using foldRight .
  def length[A](as: List[A]): Int = 
    foldRight(as, 0)((x:A, y:Int) => y + 1)

  // Exercise 3.10
  //
  // Our implementation of foldRight is not tail-recursive and will result in a StackOver-
  // flowError for large lists (we say it’s not stack-safe). Convince yourself that this is the
  // case, and then write another general list-recursion function, foldLeft , that is
  // Recursion over lists and generalizing to higher-order functions
  // tail-recursive, using the techniques we discussed in the previous chapter. Here is its
  // def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = 
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
    }

  // Exercise 3.11
  //Write sum , product , and a function to compute the length of a list using foldLeft .
  def sumFoldLeft(lst: List[Int]): Int  =
    foldLeft(lst, 0)(_+_)

  def productFoldLeft(lst: List[Int]): Int  =
    foldLeft(lst, 1)(_*_)

  def lengthFoldLeft[A](lst: List[A]): Int = 
    foldLeft(lst, 0)((x:Int,y:A) => x + 1)
}

object Chapter3 {
  import fpis.List

  def main(args: Array[String]) = 
  {
    assert(fpis.List.tail(fpis.List(1,2,3)) == fpis.List(2,3))
    assert(fpis.List.tail(fpis.List(1)) == fpis.Nil)
  }


}
