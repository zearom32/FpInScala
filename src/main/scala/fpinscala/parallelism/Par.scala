package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

/**
 * Created by zearo on 2015/11/26.
 */
object Par {
  type Par[A] = ExecutorService => Future[A]

  def map2[A,B,C](a:Par[A], b:Par[B])(f:(A,B) => C):Par[C] =
    (es:ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }


  def fork[A](a: => Par[A]):Par[A] =
  es => es.submit(new Callable[A] {
    override def call(): A = a(es).get
  }
  )

  def unit[A](a:A):Par[A] = (es: ExecutorService) => UnitFuture(a)
  def async[A](a: => A):Par[A] = fork(unit(a))
  def run[A](s: ExecutorService)(a:Par[A]): Future[A] = a(s)

  private case class UnitFuture[A](get:A) extends Future[A]{
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  //Ex4. This API already enables a rich set of operations. Here's a simple example:
  //     using async, write a function to convert any function A => B to one that evaluates its result asynchronously
  def asyncF[A,B](f: A => B): A => Par[B] = a => async(f(a))

  def sortPar(l: Par[List[Int]]): Par[List[Int]] = map2(l, unit(()))((a, _) => a.sorted)

  def map[A,B](fa: Par[A])(f: A => B): Par[B] =
  map2(fa, unit(()))((a,_) => f(a))


  //Ex5. (optional) Implement product and map as primitives, then define map2 in terms of them.
  def product[A,B](fa:Par[A],fb:Par[B]):Par[(A,B)] = (es:ExecutorService) =>{
    val a = fa(es)
    val b = fb(es)
    UnitFuture((a.get,b.get))
  }

  def _map[A,B](fa:Par[A])(f: A => B):Par[B] = (es:ExecutorService) =>UnitFuture(f(fa(es).get))

  def _map2[A,B,C](fa:Par[A],fb:Par[B])(f: (A,B) => C):Par[C] =  _map(product(fa,fb))(x => f(x._1,x._2))

  //Ex6. Note that we could always just write parMap as a new primitive. See if you can implement it this way.
  //     Remember that Par[A] is simply ans alias for ExecutorService => Future[A]. Here is the signature for parMap:
  def parMap[A,B](l:List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = l.map(asyncF(f))
    sequence(fbs)
  }

  //Ex7. (hard) Let's write this function, typically called sequence. No additional primitives are required.
  def sequence[A](l:List[Par[A]]):Par[List[A]] = l.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))

  //Ex8. Implement parFilter, which filters elements of a list in parallel.
  def parFilter[A](l: List[A])(f: A => Boolean):Par[List[A]] = {
    val pars = l.map(asyncF(a => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

}
