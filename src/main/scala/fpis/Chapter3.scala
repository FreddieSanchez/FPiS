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
  def drop[A](l: List[A], n: Int): List[A] = ???


  // Exercise 3.5
  def drop[A](l: List[A], n: Int): List[A] = ???

  // Exercise 3.6
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  // Exercise 3.7
  def init[A](l: List[A]): List[A] = ???

  // Exercise 3.9
  def length[A](l: List[A]): Int = ???

  // Exercise 3.10
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???

  // Exercise 3.11
  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}

object Chapter3 {
  import fpis.List

  def main(args: Array[String]) = 
  {
    assert(fpis.List.tail(fpis.List(1,2,3)) == fpis.List(2,3))
    assert(fpis.List.tail(fpis.List(1)) == fpis.Nil)
  }


}
