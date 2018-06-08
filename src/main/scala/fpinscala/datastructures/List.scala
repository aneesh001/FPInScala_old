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
    case Cons(h, Nil) => Nil
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
}
