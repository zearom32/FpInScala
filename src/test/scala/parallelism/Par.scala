package parallelism
import org.scalatest._
import fpinscala.parallelism._
/**
 * Created by zearo on 2015/11/30.
 */
class Par extends FlatSpec{
  import Par._
  "map" should "be correct" in {
    assert(map(unit(1))(_ + 1) == unit(2))
  }
}
