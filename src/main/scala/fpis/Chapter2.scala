package fpis

import scala.annotation.tailrec


object Chapter2 {

  // Exercise 2.1
  // Write a recursive function to get the nth Fibonacci nuber
  // The first two Fibonacci numbers are 0 and 1. The nth number is always the sum of the previous 2
  // the sequence begins 0, 1, 1, 2, 3, 5. Your definitoin should use a local tail-recursive function.
  // https://en.wikipedia.org/wiki/Fibonacci_number
  // fib(0) = 0
  // fib(1) = 1
  // fib(2) = 1
  // fib(3) = 2
  def fib(n: Int): BigInt =  {
    @tailrec
    def _fib(n: Int, prev: BigInt, current: BigInt):BigInt =  {
      if (n == 0) 0
      else if (n == 1) current
      else _fib(n - 1, current, prev + current)
    }

    _fib(n, 0, 1)
  }

  // Exercise 2.2
  // Implement isSorted, which checks whether an Array[A] is sorted according to the given comparison function
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    
    @tailrec
    def _isSorted[A](n:Int): Boolean = {
      if (n >= as.length - 1 ) true
      else if (ordered(as(n),as(n+1))) _isSorted(n+1)
      else false
    }

    _isSorted(0)
  }

  // Exercise 2.3
  // Takes a function f that takes two parameters A, B, and partially applies f, and 
  // returns a function that only takes in one parameter A.
  def curry[A,B,C](f:(A,B) => C): A => (B => C) =  
    (a: A) => (b:B) => f(a,b)

  // Exercise 4: Implement `uncurry`
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a:A, b:B) => f(a)(b)


  // Exercise 5: Implement `compose`
  def compose[A,B,C](f: B => C, g:A => B): A => C =
    (a:A) => f(g(a))


  def main(args: Array[String]) = {
    
    assert(fib(1) == 1)
    assert(fib(3) == 2)
    assert(fib(5) == 5)

    assert(isSorted(Array(1), (x:Int, y:Int) => x <= y))
    assert(isSorted(Array(1,2), (x:Int, y:Int) => x <= y))
    assert(!isSorted(Array(1,0), (x:Int, y:Int) => x <= y))
    assert(isSorted(Array(1,2,3), (x:Int, y:Int) => x <= y))
  }

}
