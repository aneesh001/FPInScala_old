package fpinscala.errorhandling

import scala.{Option => _, Either => _}

sealed trait Either[+E, +A] {
  // Exercise 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case _ => _
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case _ => _
  }

  def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case _ => _
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if(xs.isEmpty) Left("mean of empty sequence!")
    else Right(xs.sum / xs.length)
  }

  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch { case e: Exception => Left(e) }
  }

  // Exercise 4.7
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
  }

  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
    traverse(as)(x => x)
}
