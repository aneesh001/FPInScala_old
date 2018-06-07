package fpinscala.gettingstarted

/**
  * Aneesh Dandime
  * 6 - 6 - 2018
  * 11:30 PM IST
  */

object GettingStarted {
  def abs(n: Int): Int = {
    if(n < 0) -n
    else n
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(acc: Int, c: Int): Int = {
      if(c > n) acc
      else go(acc * c, c + 1)
    }

    go(1, 1)
  }

  // Exercise 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(a: Int, b: Int, n: Int): Int = {
      if(n == 1) a
      else go(b, a + b, n - 1)
    }

    go(0, 1, n)
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }
}

object Monomorphic {
  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def go(n: Int): Int = {
      if(n >= ss.length) -1
      else if(ss(n) == key) n
      else go(n + 1)
    }

    go(0)
  }
}

object Polymorphic {
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def go(n: Int): Int = {
      if(n >= as.length) -1
      else if(p(as(n))) n
      else go(n + 1)
    }

    go(0)
  }

  // Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if(n >= as.length - 1) true
      else if(!ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }

    loop(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    b => f(a, b)
  }

  // Exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a => b => f(a, b)
  }

  // Exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  // Exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}