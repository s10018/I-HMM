package scala.ihmm

import collection.mutable.{ListBuffer => ListBf}


object HMMparamFactory {

  def randomInit(vocabulary: List[String], stateN: Int): HMMparameter = {
    val initProb:   ListBf[Double]
    val transeProb: ListBf[ListBf[Double]]
    val emitProb:   ListBf[Map[String, Double]]

    return new HMMparameter(initProb, transeProb, emitProb)
  }
}


class HMMparameter(_initProb: ListBf[Double], _transeProb: ListBf[ListBf[Double]], _emitProb: ListBf[Map[String, Double]]) {
  val initProb   = _initProb    
  val transeProb = _transeProb  // transeProb(preState)(nextState)
  val emitProb   = _emitProb
}


// alphas(seq)(state), betas(seq)(state)
class FBparameter(_alphas: ListBf[ListBf[Double]], _betas: ListBf[ListBf[Double]]) {
  require(_alphas.size == _betas.size)
  val alphas = _alphas
  val betas  = _betas
  val seqN   = _alphas.size
  val stateN = _alphas.head.size

  // gamma(seq)(state)
  def convert2gamma: ListBf[ListBf[Double]] = {
    val logLike = calcLogLike
    Range(0, seqN).toList.foldLeft(ListBuf.empty[ListBuf[Double]]) { (gammaN, seqK) =>
      Range(0, stateN).toList.foldLeft(ListBuf.empty[Double]) { (gammaNk, stateK) =>
        val gammaLogProb = alpahs(seqK)(stateK) + betas(seqK)(stateK) - logLike
      }
    }
  }
  // xi(seq)(preState)(nextState)
  def convert2xi(hmmParam: HMMparameter, sentence: ListBf[String]): ListBf[ListBf[ListBf[Double]]] = {
    val logLike = calcLogLike
    Range(1, seqN).toList.foldLeft(ListBf.empty[ListBf[ListBf[Double]]]) { (xiN, seqK) =>
      Range(0, stateN).toList.foldLeft(ListBf.empty[ListBf[Double]]) { (xiNk, preStateK) =>
        Range(0, stateN).toList.foldLeft(ListBf.empty[Double]) { (xiNkK, nextStateK) =>
          val xiLogProb = alphas(seqK - 1)(preStateK) + betas(seqK)(nextStateK)
          + hmmParam.transe(preStateK)(nextStateK) + hmmParam.emitProb(seqK)(sentence(seqK)) - logLike
          xiNkK += xiProb
        }
      }
    }
  }

  def calcLogLike: Double = {
    Utils.logSumExp(alphas.last.toList)
  }
}
