package fpinscala.monads

/**
 * Created by zearo on 2015/11/16.
 */

trait Functor[F[_]] {
  def map[A,B](fa:F[A])(f: A => B): F[B]
}

class listFunctor extends Functor[List] {
  def map[A,B](as:List[A])(f: A => B): List[B] = as map f
}

trait Monad[M[_]] extends Functor[M]{
  def unit[A](a: => A):M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]):M[B]

  def map[A,B](ma:M[A])(f: A => B):M[B] =
    flatMap(ma)(a => unit(f(a)))

}
