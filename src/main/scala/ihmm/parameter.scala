package scala.ihmm

import scala.util.Random
import collection.mutable.{ListBuffer => ListBf}

type Gamma = List[Array[Double]]
type Xi    = List[Array[Array[Double]]]


object HMMparamFactory {

  val r_gen = new Random

  def randomInit(vocabulary: List[String], stateN: Int): HMMparameter = {
    val initProb   = randomLogProb(stateN).toArray
    val transeProb = Range(0, stateN).toArray.map(stateK => randomLogProb(stateN).toArray)
    val emitProb   = Range(0, stateN).toArray.map { stateK =>
      vocabulary.zip(randomLogProb(vocabulary.size)).toMap
    }

    return new HMMparameter(initProb, transeProb, emitProb)
  }
  def randomLogProb(sizeN: Int): List[Double] = {
    val probMass = Range(0, sizeN).toList.map(i => r_gen.nextDouble)
    val logNmlz  = math.log(probMass.sum)
    probMass.map { pMass =>
      math.log(pMass) - logNmlz
    }
  }
}


class HMMparameter(_initProb: Array[Double], _transeProb: Array[Array[Double]], _emitProb: Array[Map[String, Double]]) {
  val initProb   = _initProb    
  val transeProb = _transeProb  // transeProb(preState)(nextState)
  val emitProb   = _emitProb    // emitProb(state)(word)
}


// alphas(seq)(state), betas(seq)(state)
class FBparameter(_alphas: List[Array[Double]], _betas: List[Array[Double]]) {
  require(_alphas.size == _betas.size)
  val alphas = _alphas
  val betas  = _betas
  val seqN   = _alphas.size
  val stateN = _alphas.head.size

  val logLike: Double = Utils.calcLogSumExp(alphas.last.toList)

  // gamma(seq)(state)
  def convert2gamma: Gamma = {
    alphas.zip(betas).map { (alpha, beta) =>
      Range(0, stateN).toArray.map { stateK =>
        gammaLogProb = alpha(stateK) + beta(stateK) - logLike
      }
    }
  }
  // xi(seq)(preState)(nextState)
  def convert2xi(hmmParam: HMMparameter, sentence: List[String]): Xi = {
    (betas.tail.zip(alpha.init)).zip(sentence.tail).map { ((betaN, alphaPreN), wordN) =>
      Range(0, stateN).toArray.map { preStateK =>
        Range(0, StateN).toArray.map { nextStateK =>
          alphaPreN(preStateK) + hmmParam.emitProb(nextStateK)(wordN)
          + hmmParam.traneProb(preStateK)(nextStateK) + betaN(nextStateK) - logLike
        }
      }
    }
  }
}
