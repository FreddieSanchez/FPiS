package fpis.Chapter5



case object Empty extends Stream[Nothing]
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]
import Stream._;
trait Stream[+A] {

  // EXERCISE 5.1
  // Write a function to convert a Stream to a List , which will force its evaluation and let
  // you look at it in the REPL . You can convert to the regular List type in the standard
  // library. You can place this and other functions that operate on a Stream inside the
  // Stream trait.
  def toListRecursive: List[A] = 
    this match {
      case Cons(h, t) => h() :: t().toListRecursive
      case _ => List()
    }

  def toList: List[A] =  {
    @annotation.tailrec
    def _toList(stream: Stream[A], acc:List[A]): List[A] =
      stream match {
        case Cons(h, t) => _toList(t(),  h() :: acc)
        case _ => acc
      }
    _toList(this, List()).reverse
  }

  // EXERCISE 5.2
  // Write the function take(n) for returning the first n elements of a Stream , and
  // drop(n) for skipping the first n elements of a Stream .
  @annotation.tailrec
  final def drop(n: Int):Stream[A] = 
    this match {
      case Cons(_, t) if n > 0 => t().drop(n-1)
      case _ => this 
    }
    
  // EXERCISE 5.3
  // Write the function takeWhile for returning all starting elements of a Stream that
  // match the given predicate.
  def takeWhile(p: A => Boolean): Stream[A] = 
    this match {
      case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
      case _ => Empty
    }
}

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))
}
