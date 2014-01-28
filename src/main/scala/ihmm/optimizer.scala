package scala.ihmm

import collection.mutable.ListBuffer


object Optimizer {

  def run(sentences: ListBuffer[Array[String]], vocabulary: List[String],
    stateN: Int): Int = {
    343
  }

  def initInitProb(stateN: Int): Array[Double] = {
    Array(23.3)
  }

  def initTranseProb(stateN: Int): Array[Array[Double]] = {
    Array(Array(3.0))
  }

  def initEmitProb(stateN: Int, vocabulary: List[String]): Array[Map[String, Double]] = {
    Array(Map("hoge" -> 4.0))
  }
}

