package scala.ihmm

import collection.mutable.{ListBuffer => ListBf}


object Utils {

  def logSumExp(logProbs: List[Double]): Double = {
    val maxLogProb = logProbs.max
    val expSum = logProbs.foldLeft(0.0) { (expSum, logProb) =>
      expSum + math.exp(logProb - maxLogProb)
    }
    maxLogProb + math.log(expSum)
  }

  def logSum(logProbs: ListBf[Double]): Double = {
    val maxLogProb = logProbs.max
    val expSum = logProbs.foldLeft(0.0) { (expSum, logProb) =>
      expSum + math.exp(logProb - maxLogProb)
    }
    maxLogProb + math.log(expSum)
  }
}
