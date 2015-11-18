package fpinscala.laziness

import Stream._
/**
 * Created by zearo on 2015/11/16.
 */
trait Stream[+A] {

  //Ex1. write a function to convert a Stream to a List, which will force its evaluation and let us look at it in the REPL.
  // you can convert to the regular List type in the standard library. You can place this and other functions that accept a
  //Stream inside the Stream trait.
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  override def toString() = this.toList.toString()
  //Ex2. Write a function take for returning the first n elements of a Stream.
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), Empty)
    case _ => Empty
  }

  //Ex3. Write the function takeWhile for returning all starting elements of a Stream that match the given predicate.
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }


  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }

  //Ex4. Implement forAll, which checks that all elements in the Stream match a given predicate.
  //Your implementation snhould terminate the traversal as soon as it encounters a non-matching value.
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  //Ex5. Use foldRight to implement takeWhile. This whill construct a stream incrementally, and only if the values in the
  // result are demanded by some other expression.
  def takeWhileUsingfoldRight(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => {
      if (p(a))
        cons(a, b)
      else
        empty[A]
    })
  }

  //Ex6. implement map, filter, append, and flatMap using foldRight
  def map[B](f: A => B) = {
    foldRight(empty[B])((a, b) => cons(f(a), b))
  }

  def filter(p: A => Boolean) = {
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)
  }

  def append[B >: A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((h, t) => cons(h, t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h, t) => f(h).append(t))
  }

  //Ex12. Use unfold to implement map, take, takeWhile, zip(as in Chapter3), and zipAll. The zipAll function should continue
  //the traversal as long as either stream has more elements. it uses Option to indicate whether each stream has been exhausted.
  def map_2[B](f: A => B) = {
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  }

  def take_2(n: Int) = {
    unfold((this, n)) {
      case (Cons(h,t),1) => Some((h(),(t(),0)))
      case (Cons(h,t),n) if n > 1 => Some((h(),(t(),n-1)))
      case _ => None
    }
  }

  def takeWhile_2(f: A => Boolean) = {
    unfold(this){
      case Cons(h,t) if f(h()) => Some((h(),t()))
      case _ => None
    }
  }

  //TODO: implement zip and zip2

  //Ex14. implement tails using unfold. For a given Stream, tails returns the Stream of suffixes of the input sequence,
  //starting with the origin Stream. So, given Stream(1,2,3), it would return Stream(Stream(1,2,3),Stream(2,3),Stream(3),Stream.empty)

  def tails:Stream[Stream[A]] = {
    unfold(this){
      case Cons(h,t) => Some(Cons(h,t),t())
      case Empty => None
    }
  }

}




case object Empty extends Stream[Nothing]

// notice here.
// () => A. () is Function0
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def empty[A]:Stream[A] = Empty

  def cons[A](hd: => A, tl: => Stream[A]):Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A](as: A*):Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  val ones:Stream[Int] = cons(1,ones)


  //Ex7. Generalize ones slightly to the function constant which return an infinite Stream of a given value.
  def constant[A](a:A):Stream[A] = cons(a,constant(a))

  //Ex8. Write a function that generates an infinite stream of integers, starting from n, then n+1, n+2, etc.
  def from(n: Int):Stream[Int] = cons(n,from(n+1))

  //Ex9. Write a function fibs that generates the infinite stream of Fibonacci numbers: 0,1,1,2... and so on.
  private def fib_1(a:Int, b:Int):Stream[Int] = cons(a,fib_1(b,a+b))
  def fib:Stream[Int] = fib_1(0,1)

  //Ex10. We can write a more general stream building function. It takes an initial state, a function for producing both
  // the next state and the next value in the generated stream. It is usually called unfold:
  def unfold[A,S](z:S)(f: S => Option[(A,S)]):Stream[A] = f(z) match {
    case Some((a,s)) => cons(a,unfold(s)(f))
    case _ => empty[A]
  }

  //Ex11. Write fibs, from, constant, and ones in terms of unfold.
  def fibs_2:Stream[Int] = unfold((0,1))(x => Some((x._1,(x._2,x._1+x._2))))

  def from_2(a:Int):Stream[Int] = unfold(a)(x => Some((a,a+1)))

  def constant_2[A](a:A):Stream[A] = unfold(a)(x => Some((x,x)))

  def ones_2 = constant_2(1)


  //Ex13(hard): implement startsWith using functions you've written. It should check if one Stream is a prefix of another.
  //For instance, Stream(1,2,3) startsWith Stream(1,2) would be true.
  def startsWith[A](s:Stream[A],s2:Stream[A]):Boolean = (s,s2) match{
    case (_,Empty) => true
    case (Cons(h1,t1),Cons(h2,t2)) if h1() == h2() => startsWith(t1(),t2())
    case _ => false
  }

  def hasSubsequence[A](s1:Stream[A],s2:Stream[A]):Boolean = s1.tails exists (startsWith(_,s2))
}
