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


}
