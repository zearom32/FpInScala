package fpinscala.datastructures

import scala.annotation.tailrec

/**
 * Created by zearo on 2015/11/30.
 */

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List{
  def sum(ints: List[Int]):Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product (ds:List[Double]):Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as :A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //Ex2. Implement the function tail.
  def tail[A](l: List[A]):List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  //Ex3. Generalize tail to the function drop, which removes the first n elements from a list
  def drop[A](l: List[A], n:Int):List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t,n-1)
    }
  }

  //Ex4. Implement dropWhile
  def dropWhile[A](l:List[A])(f: A => Boolean):List[A] = l match{
    case Nil => Nil
    case Cons(h,t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  //Ex5. implement the function setHead for replacing the first element of a List with a different value
  def setHead[A](l:List[A], a:A):List[A] = l match {
    case Nil => Nil
    case Cons(h,t) => Cons(a,t)
  }

  //Ex6. Implement a function init which returns a List consisting of all but last element of a List.
  def init[A](l:List[A]):List[A] = l match {
    case Nil => Nil
    case Cons(h,Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }

  def append[A](a1:List[A], a2:List[A]):List[A] = a1 match{
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t,a2))
  }

  def foldRight[A,B](l:List[A], z:B)(f: (A,B) => B):B = l match {
    case Nil => z
    case Cons(x,xs) => f(x,foldRight(xs,z)(f))
  }

  def sum2(l:List[Int]) = foldRight(l,0)(_ + _)
  def product2(l:List[Double]) = foldRight(l,1.0)(_ * _)

  def length[A](l:List[A]):Int = foldRight(l,0)((_,acc) => acc + 1)

  @tailrec
  def foldLeft[A,B](l:List[A], z:B)(f:(B,A) =>B) :B = l match {
    case Nil => z
    case Cons(x,xs) => foldLeft(xs,f(z,x))(f)
  }

  def sumViafoldLeft(l:List[Int]):Int = foldLeft(l,0)(_ + _)
  def productViafoldLeft(l:List[Double]):Double = foldLeft(l,1.0)(_ * _)
  def lengthViafoldLeft[A](l:List[A]):Int = foldLeft(l,0)((acc,_) => acc + 1)

  //Ex12.
  def reverse[A](l:List[A]):List[A] = foldLeft(l, List[A]())((a,b) => Cons(b,a))

  //Ex13. (hard)
  def foldLeft2[A,B](l:List[A],z:B)(f: (B,A) => B) = foldRight(l,(x:B) => x)((x,fx) => b => fx(f(b,x)))(z)

  //Ex14.
  def append2[A](a1:List[A], a2:List[A]):List[A] = foldRight(a1,a2)((x,l) => Cons(x,l))

  //Ex15.
  def concatenate[A](ls: List[List[A]]):List[A] = foldLeft(ls,List[A]())(append2(_,_))

  def map[A,B](l:List[A])(f: A => B) :List[B] = foldRight(l,Nil:List[B])((h,t) => Cons(f(h),t))

  def filter[A](l:List[A])(f: A => Boolean):List[A] = foldRight(l,Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)

  def flatMap[A,B](l:List[A])(f:A => List[B]):List[B] = concatenate(map(l)(f))

  def filterViaflatMap[A](l:List[A])(f: A => Boolean):List[A] = flatMap(l)(x => if (f(x)) List(x) else Nil)

  def zipWith[A,B,C](a:List[A], b:List[B])(f: (A,B) => C):List[C] = (a,b) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

}
