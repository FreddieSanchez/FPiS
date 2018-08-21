case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

trait Either[+E, +A] {
  // EXERCISE 4.6
  // Implement versions of map , flatMap , orElse , and map2 on Either that operate on the
  // Right value.
  def map[B](f: A => B): Either[E, B] = 
    this match {
      case Right(x) => Right(f(x))
      case Left(y) => Left(y)
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = 
    this match {
      case Right(x) => f(x)
      case Left(y) => Left(y)
    }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = 
    this match {
      case Left(_) => b
      case _ => this
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
  /*  for {
      a <- this
      bb <- b
    } yield f(a,bb)
  */
  this flatMap(a => b.map(bb => f(a,bb)))


}

object Either {


}
