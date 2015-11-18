/**
 * Created by zearo on 2015/11/13.
 */
import fpinscala.monoids._
import fpinscala.laziness._
object test {
  def main(args:Array[String]): Unit = {
    import Monoid._
    val m = productMonoid(intAddition, intAddition)
    val p = ListFoldable.foldMap(List(1,2,3,4))(a => (a,1))(m)
    println(p)
    def f(i: => Int) = {
      val j = i
      println("j")
      (j,j)
    }
    f({println("222");4})
    val w = Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0)
    println(w.toList)
    import Stream._
    println(constant(1233).take(10).toList)
    println(from(1233).take(10).toList)
    println(fib.take(10).toList)
    println(ones_2.take(10).toList)
    println(Stream(2,3,4,5).map_2(x => x + 1).toList)
    println(Stream(1,2,3,4,5,6,7).takeWhile(_ < 4).toList)
    println(Stream(1,2,3,4,5,6,7).takeWhile_2(_ < 4).toList)
    println(startsWith(Stream(1,2,3,4,5),Stream(1,2,3)))
    println(startsWith(Stream(1,2,3),Stream(1,2,3)))
    println(startsWith(Stream(1,2,3),Stream(1,2,3,4)))
    println(startsWith(Empty,Empty))
    println(Stream(1,2,3,4).tails)
    println(hasSubsequence(Stream(1,2,3,4,5),Stream(2,3,5)))
  }
}
