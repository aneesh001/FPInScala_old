package fpinscala.laziness

import Stream._
import scala.annotation.tailrec

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  // Exercise 5.1
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toList_1: List[A] = {
    @tailrec
    def loop(s: Stream[A], l: List[A]): List[A] = s match {
      case Empty => l
      case Cons(h, t) => loop(t(), h() :: l)
    }

    loop(this, Nil).reverse
  }

  // Exercise 5.2

  // n == 1 case is handled separately because stream should not
  // look at the tail unnecessarily.
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  // final added to allow @tailrec annotation.
  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => empty
  }

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists_1(p: A => Boolean): Boolean =
    foldRight(false)((x, acc) => p(x) || acc)

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((x, acc) => p(x) && acc)

  // Exercise 5.5
  def takeWhile_1(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((x, acc) => if(p(x)) cons(x, acc) else empty)

  // Exercise 5.6
  def headOption_1: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // Exercise 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((x, acc) => cons(f(x), acc))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((x, acc) => if(f(x)) cons(x, acc) else acc)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((x, acc) => cons(x, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((x, acc) => f(x) append acc)
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
