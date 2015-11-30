package fpinscala.datastructures

/**
 * Created by zearo on 2015/11/30.
 */

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]

object Tree{
  def size[A](t: Tree[A]):Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => size(l) + size(r) + 1
  }



  def maximum(t:Tree[Int]):Int = t match{
    case Leaf(l) => l
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def depth[A](t:Tree[A]):Int = t match {
    case Leaf(l) => 1
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t:Tree[A])(f: A => B):Tree[B] = t match {
    case Leaf(l) => Leaf(f(l))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t:Tree[A])(f: A => B)(g: (B,B) => B):B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g),fold(r)(f)(g))
  }

}
