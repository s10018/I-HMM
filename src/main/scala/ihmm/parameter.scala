package scala.ihmm

class HMMparameter(_initProb: Array[Double],
  _transeProb: Array[Array[Double]],
  _emitProb: Array[Map[String, Double]]) {
  val initProb   = _initProb
  val transeProb = _transeProb
  val emitProb   = _emitProb
}

class FBparameter {
  val hoge = "Hoge"
}
