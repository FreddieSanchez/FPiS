package fpis.Chapter4v2

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(x) => Some(f(x))
    }

  /* The B >: A says
   * that the B type
   * parameter must be
   * a supertype of A .
   *
   * getOrElse returns the result inside the Some case of the Option , or if the Option
   * is None , returns the given default value
   */
  def getOrElse[B >: A](default: => B): B = 
    this match {
      case None => default
      case Some(x) => x
    }
   
  def flatMap[B](f: A => Option[B]): Option[B] = 
    this match {
      case None => None
      case Some(x) => f(x)
    }

  /* map(Option(Option(B))) get Option(B) orElse None */
  def flatMapMap[B](f: A => Option[B]): Option[B] = 
    map(f) getOrElse None

 
  /* Don’t evaluate ob unless needed.*/
  /* orElse returns the first Option if it’s defined; otherwise, it returns the second 
   * Option .
   */
  def orElse[B >: A](ob: => Option[B]): Option[B] = 
    this match {
      case None => ob
      case _ => this
    }

  
  def orElseMap[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =  
    this match {
      case None => None
      case Some(x)  => if(f(x)) this else None
    }

  def filterFlatMap(f: A => Boolean): Option[A] =  
    flatMap(x => if(f(x)) this else None)

}

object Option {
  // EXERCISE 4.2
  // Implement the variance function in terms of flatMap . If the mean of a sequence is m ,
  // the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
  // See the definition of variance on Wikipedia (http://mng.bz/0Qsr).

  def mean(xs: Seq[Double]): Option[Double] = 
    if (xs.isEmpty) None else Some(xs.sum/xs.length)

  def variance(xs: Seq[Double]): Option[Double] = 
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m,2))))

  // EXERCISE 4.3
  // Write a generic function map2 that combines two Option values using a binary func-
  // tion. If either Option value is None , then the return value is too. Here is its signature:
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap( x => b.map( y=> f(x,y)))

  // EXERCISE 4.4
  // Write a function sequence that combines a list of Options into one Option containing
  // a list of all the Some values in the original list. If the original list contains None even
  // once, the result of the function should be None ; otherwise the result should be Some
  // with a list of all the values. Here is its signature:
  def sequence[A](a: List[Option[A]]): Option[List[A]] = 
    a match {
      case Nil => Some(Nil)
      case h::t => h flatMap((hh:A) => sequence(t) map ((zz:List[A]) => hh::zz))
    }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = 
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_::_))

  // EXERCISE 4.5
  // Implement this function. It’s straightforward to do using map and sequence , but try
  // for a more efficient implementation that only looks at the list once. In fact, imple-
  // ment sequence in terms of traverse .
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
    sequence(a map(f))


  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
    a match { 
      case Nil => Some(Nil)
      case h::t => map2(f(h),traverse(t)(f))(_::_)
    }


}
