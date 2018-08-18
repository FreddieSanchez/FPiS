package fpis.Chapter4

// EXERCISE 4.1
// Implement all of the preceding functions on Option . As you implement each function,
// try to think about what it means and in what situations you’d use it. We’ll explore when
// to use each of these functions next. Here are a few hints for solving this exercise:
//
// It’s fine to use pattern matching, though you should be able to implement all
// the functions besides map and getOrElse without resorting to pattern matching.
//
// For map and flatMap , the type signature should be enough to determine the
// implementation.
//
// getOrElse returns the result inside the Some case of the Option , or if the Option
// is None , returns the given default value.
//
// orElse returns the first Option if it’s defined; otherwise, it returns the second
// Option .


sealed trait Option[+A] {
  def map[B](f:A => B): Option[B] =
    this match {
      case None => None
      case Some(x) => Some(f(x))
    }

  /* The B >: A says
   * that the B type
   * parameter must be
   * a supertype of A */
  def getOrElse[B >: A](default: => B): B = 
    this match {
      case None => default
      case Some(x) => x
    }

  def flatMap[B](f:A => Option[B]): Option[B] =
    map(f) getOrElse None

  /* Don’t
   * evaluate
   * ob unless
   * needed
   */
  def orElse[B >: A](ob: () => Option[B]): Option[B] = 
    this map (Some(_)) getOrElse ob()

  /*Convert Some to None if
   *the value doesn’t satisfy f .
   */
  def filter(f: A => Boolean): Option[A] = 
    flatMap(x => if (f(x)) Some(x) else None)
   
  // EXERCISE 4.2
  // Implement the variance function in terms of flatMap . If the mean of a sequence is m ,
  // the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
  // See the definition of variance on Wikipedia (http://mng.bz/0Qsr).
  def variance(xs: Seq[Double]): Option[Double] =  ???

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
