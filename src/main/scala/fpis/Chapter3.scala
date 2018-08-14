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

  // Exercise 3.12 
  // Write a function that returns the reverse of a list (given List(1,2,3) it returns
  // List(3,2,1) ). See if you can write it using a fold.
  def reverse[A](lst: List[A]):List[A] = 
    foldLeft(lst,Nil:List[A])((x:List[A], y:A) => Cons(y,x))

  // Exercise 3.14
  // Implement append in terms of either foldLeft or foldRight .
  def appendFoldRight[A](lst:List[A], a:List[A]) = 
    foldRight(lst,a)(Cons(_,_))

  // Exercise 3.15
  // Hard: Write a function that concatenates a list of lists into a single list. Its runtime
  // should be linear in the total length of all lists. Try to use functions we have already
  // defined.

  def concat[A](lsts:List[List[A]]): List[A] = 
    foldRight(lsts, Nil:List[A])(appendFoldRight)

  // Exercise 3.16
  // Write a function that transforms a list of integers by adding 1 to each element.
  // (Reminder: this should be a pure function that returns a new List !)
  def add1(lst: List[Int]): List[Int] = 
    foldRight(lst, Nil:List[Int])((x:Int, y:List[Int]) => Cons(x+1, y))

  // Exercise 3.17
  // Write a function that turns each value in a List[Double] into a List[String]. You can use
  // the expression d.toString to convert some d: Double to a String .
  def listDoubleToString(lst:List[Double]):List[String] =
    foldRight(lst, Nil:List[String])((x:Double, y: List[String]) => Cons(x.toString, y))

  // Exercise 3.18
  // Write a function map that generalizes modifying each element in a list while maintain-
  // ing the structure of the list. Here is its signature: 
  def map[A,B](as: List[A])(f: A => B): List[B] = 
    foldRight(as, Nil:List[B])((x:A, y: List[B]) => Cons(f(x), y))

  // EXERCISE 3.19
  // Write a function filter that removes elements from a list unless they satisfy a given
  // predicate. Use it to remove all odd numbers from a List[Int] .
  def filterFoldRight[A](as: List[A])(f:A => Boolean): List[A] = 
    foldRight(as, Nil:List[A]) ((x, y) => if (f(x)) Cons(x,y) else y)
  
  def filterExplict[A](as: List[A])(f: A => Boolean): List[A] = 
    as match {
      case Cons(x, xs) => { 
        if (f(x)) Cons(x, filterExplict(xs)(f))
        else filterExplict(xs)(f)
      }
      case _ => Nil
    }


  //  EXERCISE 3.20
  //  Write a function flatMap that works like map except that the function given will return
  //  a list instead of a single result, and that list should be inserted into the final resulting
  //  list. Here is its signature:
  //  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B]
  //  For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in
  //  List(1,1,2,2,3,3) .

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = 
    concat(map(as)(f))

  def flatMapFoldRight[A, B](as: List[A])(f: A => List[B]): List[B] = 
    foldRight(as, Nil:List[B])((x:A, y: List[B]) => concat(List(f(x),y)))

  //EXERCISE 3.21
  //Use flatMap to implement filter .
  def filterFlatMap[A](as:List[A])(f: A => Boolean) = 
    flatMap(as)(a => if(f(a)) List(a) else Nil)

  //EXERCISE 3.22
  //Write a function that accepts two lists and constructs a new list by adding correspond-
  //ing elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9) .
  def addtwoList(a:List[Int], b:List[Int]): List[Int] =
    (a,b) match {
      case (_,Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(x,xs), Cons(y,ys)) => Cons(x + y, addtwoList(xs, ys))
    }

  //EXERCISE 3.23
  //Generalize the function you just wrote so that it’s not specific to integers or addition.
  //Name your generalized function zipWith .
  def zipWith[A,B,C](a:List[A], b:List[B])(f: (A,B) => C): List[C] =
    (a,b) match {
      case (_,Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(x,xs), Cons(y,ys)) => Cons(f(x,y), zipWith(xs, ys)(f))
    }

  //  EXERCISE 3.24
  //  Hard: As an example, implement hasSubsequence for checking whether a List con-
  //  tains another List as a subsequence. For instance, List(1,2,3,4) would have
  //  List(1,2) , List(2,3) , and List(4) as subsequences, among others. You may have
  //  some difficulty finding a concise purely functional implementation that is also effi-
  //  cient. That’s okay. Implement the function however comes most naturally. We’ll
  //  return to this implementation in chapter 5 and hopefully improve on it. Note: Any
  //  two values x and y can be compared for equality in Scala using the expression x == y .

  @annotation.tailrec
  def startsWith[A](sup:List[A], sub:List[A]): Boolean = {
    (sup, sub) match { 
      case (Nil, _) => false
      case (_,Nil) => true
      case (Cons(x,xs), Cons(y,ys)) => if (x==y) startsWith(xs,ys) else false
    }
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =  {
    (sup, sub) match {
      case (Nil, _) => false
      case (_, Nil) => true
      case (xs, ys) if startsWith(xs, ys) => true
      case (Cons(x, xs), ys) => hasSubsequence(xs, ys)
    }
  }
  
}


object Chapter3 {
  import fpis.List

  def main(args: Array[String]) = 
  {
    assert(fpis.List.tail(fpis.List(1,2,3)) == fpis.List(2,3))
    assert(fpis.List.tail(fpis.List(1)) == fpis.Nil)
  }


}
