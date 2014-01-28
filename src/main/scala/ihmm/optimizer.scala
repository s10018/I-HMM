package scala.ihmm

import collection.mutable.ListBuffer


object Optimizer {

  val Threshold = 0.001

  def run(sentences: ListBuffer[Array[String]], vocabulary: List[String],
    stateN: Int): HMMparameter = {
    val param = new HMMparameter(Array(34.3), Array(Array(34.1)), Array(Map("hoge" -> 34.2)))
    return param
    def BaumWelch(oldHMMparam: HMMparameter): HMMparameter = {
      def calcLogLike(hmmParam: HMMparameter): Double = {
        24324.4
      }
      def EStep(hmmParam: HMMparameter): FBParameter = {
        "hoge"
      }
      def MStep(param: FBparameter): HMMparameter = {
        "foo"
      }
      def oneStep = (MStep compose EStep)
      def _BaumWelch(oldHMMparam: HMMparameter, oldLoglike: Double): HMMparameter = {
        val newHMMparam = oneStep(oldHMMparam)
        val newLoglike  = calcLogLike(oldHMMparam)
        if ((newLogLike - oldLogLike).abs < Threshold)
          newHMMparam
        else
          _BaumWelch(newHMMparam, newLogLike)
      }
      val initialHMMparam = HMMparameter.randomInit(vocabulary, stateN)
      val initialLogLike  = calcLogLike(initialHMMparam)
      _BaumWelch(initialHMMparam, initialLogLike)
    }
  }
}
