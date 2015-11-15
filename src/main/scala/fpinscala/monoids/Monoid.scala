package fpinscala.monoids

/**
 * Created by zearom32 on 2015/11/13.
 */


trait Monoid[A] {
  def op(a1:A, a2: A): A
  def zero: A
}


object Monoid{
  val stringMonoid = new Monoid[String]{
    def op(a1:String, a2: String):String = a1 ++ a2
    def zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]]{
    def op(a1:List[A],a2:List[A]):List[A] = a1 ++ a2
    val zero = Nil
  }


  //Ex1. Given Monoid instances for intger addition and multiplication as well as the Boolean operators.
  val intAddition: Monoid[Int] = new Monoid[Int]{
    def op(a1:Int, a2:Int) = a1 + a2
    def zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int]{
    def op(a1:Int, a2:Int) = a1 * a2
    def zero = 1
  }

  val booleanOr:Monoid[Boolean] = new Monoid[Boolean]{
    def op(a1:Boolean, a2:Boolean) = a1 || a2
    def zero = false
  }

  val booleanAnd:Monoid[Boolean]  = new Monoid[Boolean]{
    def op(a1:Boolean, a2:Boolean) = a1 && a2
    def zero = true
  }

  //Ex2. Given a Monoid instance for combining Options
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]]{
    def op(a1:Option[A], a2:Option[A]):Option[A] = a1 orElse a2
    def zero = None
  }

  //Ex3. A function having the same argument and return type is called an endofunction. Write a monoid for endofunctions.
  def EndoMonoid[A]: Monoid[A => A] =  new Monoid[A => A]{
    def op(a1: A=>A, a2:A=>A) = a1 compose a2
    def zero = x => x
  }


  //Ex4. Use the property-based testing framework we developed in Part 2 t oimplement a property for the monoid laws. Use your property to test the monoids we have written.
  //TODO: implement monoidLaws[A]
  //val monoidLaws[A](m:Monoid[A]):Prop

  //Ex5. Write a monoid instance for String that inserts spaces between words unless there already is one, and trims spaces off the ends of the result.
  def wordsMonoid:Monoid[String] = new Monoid[String]{
    def op(a1:String, a2:String) = a1.trim() ++ " " ++ a2.trim()
    def zero = ""
  }

  //Ex6. Implement concatenate, a function that folds a list with a monoid
  def concatenate[A] (as:List[A], m: Monoid[A]) :A ={
    as.foldLeft(m.zero)(m.op)
  }

  //Ex7. if our list has an element type that doesn't have a Monoid instance. map over the list to turn it into a type that does.
  def foldMap[A,B](as:List[A], m: Monoid[B])(f: A => B): B= {
    // TODO: verify if it is good with a countless list
    //as.map(f).foldLeft(m.zero)(m.op)
    as.foldLeft(m.zero)((a,b) => m.op(a,f(b)))
  }

  //Ex8(hard). The foldMap function can be implemented using either foldLeft or foldRight. But you can also write foldLeft and foldRight using foldMap!
  def dual[A](m:Monoid[A]) : Monoid[A] = new Monoid[A]{
    def op(x:A, y:A) = m.op(y,x)
    def zero = m.zero
  }

  def foldLeftUsingfoldMap[A,B](as:List[A])(z:B)(f: (B,A) => B): B = {
    foldMap(as,dual(EndoMonoid[B]))(a => b => f(b,a))(z)
  }


  def foldRightUsingfoldMap[B,A](as:List[A])(z:B)(f:(A,B) => B):B = {
    foldMap(as,EndoMonoid[B])(f.curried)(z)
  }

//Ex11.
  def foldMapV[A,B](as:IndexedSeq[A],m:Monoid[B])(f: A=>B):B = {
    if (as.isEmpty) m.zero
    else
      if (as.length == 1)
      f(as.head)
    else{
      val (l,r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l,m)(f),foldMapV(r,m)(f))
    }
  }
  sealed trait WC
  case class Stub(chars:String) extends WC
  case class Part(lStub:String, words:Int, rStub:String) extends WC
  //Ex9. Write a monoid instance for WC and make sure that it meets the monoid laws
  val wcMonoid: Monoid[WC] = new Monoid[WC]{
    def op(x:WC, y:WC) = (x,y) match{
      case (Stub(c),Stub(d)) => Stub(c+d)
      case (Stub(c),Part(l,w,r)) => Part(c+1,w,r)
      case (Part(l,w,r), Stub(c)) => Part(l,w,r+c)
      case (Part(l1,w1,r1), Part(l2,w2,r2)) =>
        Part(l1,w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2,r2)
    }
    def zero = Part("",0,"")
  }

  // correct! interesting!
  def count(s:String):Int = {
    def wc(c:Char):WC = if (c.isWhitespace || c == ',') Part("",0,"") else Stub(c.toString)
    def unstub(s:String) = s.length min 1
    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match{
      case Stub(u) => unstub(u)
      case Part(l,w,r) => unstub(l) + w + unstub(r)
    }
  }

  //Ex12. Use foldMap to detect whether a given IndexedSeq[Int] is ordered.
  //You will need to come up with a creative monoid instance
  sealed trait IsOrd
  object NotOrd extends IsOrd
  case class Ord(m:Int) extends IsOrd

  def isOrdered(x:IndexedSeq[Int]):Boolean = {
    val ordMonoid = new Monoid[IsOrd] {
      def op(x:IsOrd, y:IsOrd) = (x,y) match {
        case (NotOrd,_) => NotOrd
        case (_,NotOrd) => NotOrd
        case (Ord(p),Ord(q)) => if (p > q) NotOrd else Ord(q)
      }
      def zero = Ord(Int.MinValue)
    }

    foldMapV(x,ordMonoid)(x => Ord(x)) match {
      case NotOrd => false
      case _ => true

    }
  }



  // Ex17. prove it.
  def productMonoid[A,B](a:Monoid[A],b:Monoid[B]): Monoid[(A,B)] = new Monoid[(A,B)]{
    def op(x:(A,B),y:(A,B)) = (a.op(x._1,y._1),b.op(x._2,y._2))
    def zero = (a.zero,b.zero)
  }

  //Ex18. Do the same with Either. This is called a monoid coproduct.

  def coproductMonoid[A,B](a:Monoid[A],b:Monoid[B]):Monoid[Either[A,B]] = new Monoid[Either[A,B]] {
    def op(x:Either[A,B],y:Either[A,B]) = (x,y) match {
      case (Left(a),Right(b)) => Left(a)
      case (Right(b),Left(a)) => Left(a)
      case (Left(p),Left(q)) => Left(a.op(p,q))
      case (Right(p),Right(q)) => Right(b.op(p,q))
    }
    def zero = Right(b.zero)
  }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K,V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }
    }

  //Ex19. Write a monoid instance for functions whose results are monoids.
  def functionMonoid[A,B](B:Monoid[B]):Monoid[A => B] = new Monoid[A=>B]{
    def zero:A => B = a=>B.zero
    def op(x:A=>B, y:A=>B):A=>B = a => B.op(x(a),y(a))
  }

  //Ex20. Use monoids to compute a frequency map of words in an IndexedSeq of Strings.
  def frequencyMap(strings: IndexedSeq[String]):Map[String,Int] = {
    foldMapV(strings, mapMergeMonoid[String, Int](intAddition))((s:String) => Map(s -> 1))
  }

}


//Ex13.
trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(EndoMonoid[B])(z)

  def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(EndoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldMap[A,B](as:List[A])(f:A => B)(mb:Monoid[B]):B = {
    as.foldRight(mb.zero)((a,b) => mb.op(f(a),b))
  }
}


//Ex14. Recall the Tree data type from Chap3. Implement a Foldable instance for it.

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A],right:Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A,B](as:Tree[A])(f:A=>B)(mb:Monoid[B]):B = as match{
    case Leaf(v) => mb.op(mb.zero,f(v))
    case Branch(l,r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }

  override def foldLeft[A,B](as:Tree[A])(z:B)(f:(B,A)=>B):B = as match{
    case Leaf(v) => f(z,v)
    case Branch(l,r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }

  override def foldRight[A,B](as:Tree[A])(z:B)(f:(A,B)=>B):B = as match{
    case Leaf(v) => f(v,z)
    case Branch(l,r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }

}
