/**
 * Created by zearo on 2015/11/13.
 */
import fpinscala.monoids._

object test {
  def main(args:Array[String]): Unit = {
    import Monoid._
    val m = productMonoid(intAddition, intAddition)
    val p = ListFoldable.foldMap(List(1,2,3,4))(a => (a,1))(m)
    println(p)
  }
}
