
package scala.ihmm

import java.util.Random
import jsat.linear.Vec
import jsat.distributions.multivariate.Dirichlet

object DirRand {

  def random(size: Int, alpha: Double): List[Double] = {
    val rand = new Random(System.currentTimeMillis())
    val vec = Vec.zeros(size)
    for(i <- Range(0, size)) {
      vec.set(i, alpha)
    }
    new Dirichlet(vec).sample(1, rand).get(0).arrayCopy().toList
  }

}
