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
    foldRight(empty: Stream[A])((x, acc) => if (p(x)) cons(x, acc) else empty)

  // Exercise 5.6
  def headOption_1: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // Exercise 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((x, acc) => cons(f(x), acc))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((x, acc) => if (f(x)) cons(x, acc) else acc)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((x, acc) => cons(x, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((x, acc) => f(x) append acc)

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  // Exercise 5.13
  def map_1[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }
  }

  def take_1(n: Int): Stream[A] = {
    unfold((n, this)) {
      case (1, Cons(h, t)) => Some((h(), (0, empty)))
      case (x, Cons(h, t)) => Some((h(), (x - 1, t())))
      case _ => None
    }
  }

  def takeWhile_2(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }
  }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  }

  // Useful to write a general function called zipWithAll.
  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s)) {
      case (Cons(h, t), Empty) => Some(((Some(h()), None), (t(), empty)))
      case (Empty, Cons(h, t)) => Some(((None, Some(h())), (Empty, t())))
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case _ => None
    }
  }

  // Exercise 5.14
  def startsWith[B >: A](s: Stream[B]): Boolean = {
    zipAll(s).takeWhile {
      case (_, Some(_)) => true
      case _ => false
    }.forAll { case (h1, h2) => h1 == h2 }
  }

  // Exercise 5.15
  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Empty => None
      case s => Some((s, s.drop(1)))
    } append Stream(empty)
  }

  def hasSubSequence[B >: A](s: Stream[B]): Boolean =
    tails exists(_ startsWith s)

  // Exercise 5.16

  // slightly inefficient because if evaluates by name parameter 'acc'
  // multiple times. Use lazy val to cache the result.
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    foldRight(empty[B])((x, acc) => acc match {
      case Empty => cons(f(x, z), Empty)
      case Cons(h, _) => cons(f(x, h()), acc)
    })
  }

  // Implementation taken from textbook site.
  def scanRight_1[B](z: B)(f: (A, => B) => B): Stream[B] = {
    foldRight((z, Stream(z)))((x, acc) => {
      lazy val lacc = acc
      val res = f(x, lacc._1)
      (res, cons(res, lacc._2))
    })._2
  }
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

  // Exercise 5.8
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  // Exercise 5.9
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  // Exercise 5.10
  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] =
      cons(a, go(b, a + b))

    go(0, 1)
  }

  // Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  def unfold_1[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map((v: (A, S)) => cons(v._1, unfold_1(v._2)(f))).getOrElse(empty)

  // Exercise 5.12
  def constant_1[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  def from_1(n: Int): Stream[Int] =
    unfold(n)(s => Some(s, s + 1))

  def fibs_1: Stream[Int] = unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))
}
