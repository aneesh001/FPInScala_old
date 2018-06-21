package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(h, t) => h * product(t)
  }

  def apply[A](as: A*): List[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  // Exercise 3.1
  // The answer is 3. The pattern matches the third case.

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException("tail of empty list!")
    case Cons(_, t) => t
  }

  // Exercise 3.3
  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => throw new IllegalArgumentException("can't replace head of empty list!")
    case Cons(_, t) => Cons(a, t)
  }

  def setHead2[A](l: List[A], a: A): List[A] = Cons(a, tail(l))

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if(n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  // Exercise 3.5
  def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match {
    case Cons(h, t) if p(h) => dropWhile(t, p)
    case _ => l
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(h, t) => Cons(h, append(t, l2))
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException("init of empty list!")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // Tail recursive answer to Exercise 3.6
  def init2[A](l: List[A]): List[A] = {
    import scala.collection.mutable.ListBuffer
    val buf = new ListBuffer[A]

    @tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => throw new IllegalArgumentException("init of empty list!")
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
    }

    go(l)
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def sum2(l: List[Int]): Int = foldRight(l, 0)(_ + _)

  def product2(d: List[Double]): Double = foldRight(d, 1.0)(_ * _)

  // Exercise 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  // Exercise 3.10
  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // Exercise 3.11
  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def product3(d: List[Double]): Double = foldLeft(d, 1.0)(_ * _)
  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, x) => Cons(x, acc))

  // Exercise 3.13
  def foldRightUsingFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((acc, x) => f(x, acc))

  // Crazy implementation taken from book site. NOT STACK-SAFE.
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  // Exercise 3.14
  def appendUsingFoldRight[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)(Cons(_, _))

  // Exercise 3.15
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A]())(append)

  // Exercise 3.16
  def addOne(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, addOne(t))
  }

  def addOneViaFold(l: List[Int]): List[Int] =
    foldRight(l, List[Int]())((x, acc) => Cons(x + 1, acc))

  // Exercise 3.17
  def doubleToString(d: List[Double]): List[String] =
    foldRight(d, List[String]())((x, acc) => Cons(x.toString, acc))

  // Exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def mapViaFold[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, List[B]())((x, acc) => Cons(f(x), acc))

  def mapUsingBuffer[A, B](as: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]

    @tailrec
    def loop(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => buf += f(h); loop(t)
    }

    loop(as)
    List(buf.toList: _*)
  }

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((x, acc) => if(f(x)) Cons(x, acc) else acc)

  def filterUsingBuffer[A](as: List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]

    @tailrec
    def loop(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => if(f(h)) buf += h; loop(t)
    }

    loop(as)
    List(buf.toList: _*)
  }

  // Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def flatMapUsingFold[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, List[B]())((x, acc) => append(f(x), acc))

  // Exercise 3.21
  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if(f(x)) List(x) else Nil)

  // Exercise 3.22
  def addRespective(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addRespective(t1, t2))
  }

  // Exercise 3.23
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
  }

  // Exercise 3.24
  @tailrec
  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def beginsWith(s: List[A], t: List[A]): Boolean = (s, t) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) => if(h1 == h2) beginsWith(t1, t2) else false
    }

    sup match {
      case Nil => sub == Nil
      case l if beginsWith(l, sub) => true
      case Cons(_, t) => hasSubSequence(t, sub)
    }
  }

  def main(args: Array[String]): Unit = {
    val ll = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    println(concat(ll))
  }
}
