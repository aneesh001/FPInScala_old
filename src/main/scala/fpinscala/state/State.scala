package fpinscala.state

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  // Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, next) = rng.nextInt

    if(n >= 0) (n, next)
    else if(n == Int.MinValue) (-1 * (n + 1), next)
    else (-1 * n, next)
  }

  // Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (n, next) = nonNegativeInt(rng)

    (n.toDouble / Int.MaxValue.toDouble, next)
  }

  //Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)

    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt

    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  // Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (1 to count).toList.
      foldLeft((List[Int](), rng))((acc, x) => {
        val (i, r) = acc._2.nextInt
        (i :: acc._1, r)
      })
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, r) = s(rng)
      (f(a), r)
    }
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  // Exercise 6.5
  val _double: Rand[Double] =
    map(nonNegativeInt)(_ / Int.MaxValue.toDouble + 1)

  // Exercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])
    (f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)

      (f(a, b), r2)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // Exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => fs.foldLeft((List[A](), rng))((acc, x) => {
      val (a, r) = x(acc._2)
      (a :: acc._1, r)
    })
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((x, acc) => map2(x, acc)(_ :: _))

  def ints2(n: Int): Rand[List[Int]] =
    sequence(List.fill(n)(int))

  // Exercise 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r) = f(rng)
      g(a)(r)
    }

  // Exercise 6.9
  def mapViaFlatMap[A, B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap(ra)(a => unit(f(a)))

  // This is map2 via flatmap.
  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])
    (f: (A, B) => C): Rand[C] = {

    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }
}

import State._

case class State[S, +A](run: S => (A, S)) {
  // Exercise 6.10
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, ns) = run(s)
      f(a).run(ns)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    this.flatMap(a => sb.map(b => f(a, b)))
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // Exercise 6.10
  // See other implementations on the book's github page.
  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] =
    l.foldRight(unit[S, List[A]](List())) {
      (x, acc) => x.map2(acc)(_ :: _)
    }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  // for better understanding
  def modify2[S](f: S => S): State[S, Unit] = State {
    s => ((), f(s))
  }

  // for better understanding
  def modigy3[S](f: S => S): State[S, Unit] =
    get.flatMap(s => set(f(s)))
}

// Exercise 6.11 - Taken from book's github page.
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update = (i: Input) => (s: Machine) => {
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    sequence(inputs.map(i => modify[Machine](update(i)))).
      flatMap(_ => get.map(s => (s.coins, s.candies)))
}

