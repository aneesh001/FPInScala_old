package fpinscala.parallelism

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  // Exercise 7.3 Taken from some github page
  def _map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)

      new Future[C] {
        def isDone = af.isDone && bf.isDone

        def isCancelled = af.isCancelled || bf.isCancelled

        def get = f(af.get, bf.get)

        def cancel(evenIfRunning: Boolean): Boolean = true

        def get(timeout: Long, units: TimeUnit) = {
          val start = System.currentTimeMillis

          val a = af.get(timeout, units)

          val used = System.currentTimeMillis - start
          val left = units.toMillis(timeout) - used

          val b = bf.get(left, units)

          f(a, b)
        }
      }
    }
  }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // Exercise 7.4
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))
}
