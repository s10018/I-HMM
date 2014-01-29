package scala.ihmm


object HMMparamFactory {

  def randomInit(vocabulary: List[String], stateN: Int): HMMparameter = {
    val initProb:   Array[Double]
    val transeProb: Array[Array[Double]]
    val emitProb:   Array[Map[String, Double]]

    return new HMMparameter(initProb, transeProb, emitProb)
  }
}

class HMMparameter(_initProb: Array[Double], _transeProb: Array[Array[Double]], _emitProb: Array[Map[String, Double]]) {
  val initProb   = _initProb
  val transeProb = _transeProb
  val emitProb   = _emitProb
}

// alphas[seq][state], betas[seq][state]
class FBparameter(_alphas: Array[Array[Double]], _betas: Array[Array[Double]]) {
  val alphas = _alphas
  val betas  = _betas
}
