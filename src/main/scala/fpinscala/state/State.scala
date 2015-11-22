package fpinscala.state

/**
 * Created by zearo on 2015/11/21.
 */

trait RNG {
  def nextInt:(Int,RNG)
}

object RNG{

  //TODO: What's the difference between 'def simple' and 'case class Simple'
  def simple(seed: Long): RNG = new RNG{
    def nextInt = {
      val seed2 = (seed*0x5deece66dL+0xbL) & ((1L << 48)-1)
      ((seed2 >>> 16).asInstanceOf[Int],simple(seed2))
    }
  }

  case class Simple(seed:Long) extends RNG {
    def nextInt = {
      val seed2 = (seed*0x5deece66dL+0xbL) & ((1L << 48)-1)
      ((seed2 >>> 16).asInstanceOf[Int],Simple(seed2))
    }
  }

  //Ex1. Write a function to generate a random positive integer. Make sure to handle the corner case Int.MinValue which doesn't have a positive counterpart.
  def positiveInt(rng:RNG):(Int,RNG) = {
    val (i,r) = rng.nextInt
    (if (i < 0) -(i+1) else i,r)
  }

  //Ex2. Write a function to generate a Double Between 0 and 1, not including 1. Note: you can use Int.MaxValue to obtain the maximum positive integer and you can use x.toDouble to convert an Int,x, to a Double.
  def double(rng:RNG):(Double,RNG) = {
    val (i,r) = positiveInt(rng)
    (if (i == Int.MaxValue) 0.0 else i.toDouble / Int.MaxValue.toDouble,r)
  }

  //Ex3. Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a (Double, Double, Double) 3 -tuple. You should be able to reuse the functions you've already written.
  def  intDouble(rng:RNG):((Int,Double),RNG) = {
    val (i,r) = rng.nextInt
    val (d,q) = double(r)
    ((i,d),q)
  }

  def doubleInt(rng:RNG):((Double,Int),RNG) = {
    val (d,r1) = double(rng)
    val (i,r2) = r1.nextInt
    ((d,i),r2)
  }

  def double2(rng:RNG):((Double,Double,Double),RNG) = {
    val (i1,r1) = double(rng)
    val (i2,r2) = double(r1)
    val (i3,r3) = double(r2)
    ((i1,i2,i3),r3)
  }


  //Ex4. Write a function a generate a list of random integers.
  def ints(count:Int)(rng:RNG):(List[Int],RNG) = count match{
    case 0 => (Nil,rng)
    case c =>
      lazy val (i,r) = rng.nextInt
      val u = ints(c-1)(r)
      (i::u._1,u._2)

  }

  type Rand[+A] = RNG => (A,RNG)
  val int: Rand[Int] = _.nextInt

  def unit[A](a:A):Rand[A] = rng => (a,rng)

  def map[A,B](s:Rand[A])(f: A => B):Rand[B] = rng =>
    {
      val (a,rng2) = s(rng)
      (f(a),rng2)
    }

  //Ex5. Use map to generate an Int between 0 and n, inclusive:
  def positiveMax(n:Int):Rand[Int] =  map(positiveInt)(_ % (n+1))

  //Ex6. Use map to reimplement RNG.double in a more elegant way.
  def doubleViaMap:Rand[Double] = map(positiveInt)(_ / (Int.MaxValue.toDouble + 1))

  //Ex7. Unfortunately, map is not powerful enough to implement intDouble and doubleInt from before. What we need is a new combinator map2,
  //that can combine two RNG actions into one using a binary rather than unary function.  Write its implementation and then use it to reimplement the intDouble and doubleInt functions.
  def map2[A,B,C](ra:Rand[A], rb:Rand[B])(f: (A,B) => C):Rand[C] = rng => {
    val (a,r1) = ra(rng)
    val (b,r2) = rb(r1)
    (f(a,b),r2)
  }
  def intDoubleViaMap2:Rand[(Int,Double)] = map2(positiveInt,double)((_,_))
  def doubleIntViaMap2:Rand[(Double,Int)] = map2(double,positiveInt)((_,_))

  //Ex8. (hard): If we can combine two RNG transitions, we should be able to combine a whole list of them. Implement sequence, for combining a List of transitions into a single transition.
  // Use it to reimplement the ints function you wrote before. For the latter, you can use the standard library function List.fill(n)(x) to make a list with x repeated n times.
  def sequence[A](fs:List[Rand[A]]):Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f,acc) => map2(f,acc)(_ :: _))

  def ints2(n:Int) = sequence(List.fill(n)(int))

  //Ex9. Implement flatMap, then use it to reimplement positiveInt.
  def flatMap[A,B](f:Rand[A])(g: A => Rand[B]) :Rand[B] = rng => {
    val (a,r1) = f(rng)
    g(a)(r1)
  }

  //Ex10. Reimplement map and map2 interms of flapMap
  def _map[A,B](s:Rand[A])(f: A => B):Rand[B] =
  {
    flatMap(s)(a => unit(f(a)))
  }

  def _map2[A,B,C](ra:Rand[A], rb:Rand[B])(f: (A,B) => C):Rand[C] =  {
    flatMap(ra)(a => map(rb)(b => f(a,b)))
  }


}

case class State[S,+A](run: S => (A,S)){
  //Ex11. Generalize the functions unit, map, map2, flatMap and sequence. Add them as methods on the State case class where possible. Otherwise you should put them in a State companion object.
  def map[B](f: A => B): State[S,B] = flatMap(a => State.unit(f(a)))

  //notice here.
  //nice implementation.
  def map2[B,C](sb:State[S,B])(f:(A,B) => C):State[S,C] = flatMap(a => sb.map(b => f(a,b)))

  def flatMap[B](f: A => State[S,B]):State[S,B] = State(s =>{
    val (i,r) = run(s)
    f(i).run(r)
  })

}

object State{
  type Rand[A] = State[RNG,A]

  def unit[S,A](a:A):State[S,A] = State(s => (a,s))

  def sequence[S,A](sas: List[State[S,A]]):State[S,List[A]] = {
    def go(s:S, actions:List[State[S,A]],acc:List[A]):(List[A],S) = actions match{
        case Nil => (acc.reverse, s)
        case h::t => h.run(s) match {case (a,s2) => go(s2,t,a::acc)}
      }
    State((s:S) => go(s,sas,List()))
  }


  def sequence2[S,A](sas: List[State[S,A]]):State[S,List[A]] = {
    sas.foldRight(unit[S,List[A]](List[A]()))((a,acc) => a.map2(acc)(_ :: _))
  }


  def modify[S](f: S => S):State[S,Unit] = for {
    s <- get
    _ <- set(f(s))
   } yield ()

  def get[S]:State[S,S] = State(s => (s,s))
  def set[S](s:S):State[S,Unit] = State(_ => ((),s))


}

