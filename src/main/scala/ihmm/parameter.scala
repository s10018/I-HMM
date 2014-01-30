package scala.ihmm

import scala.util.Random
import collection.mutable.{ListBuffer => ListBf}

type Gamma = ListBf[ListBf[Double]]
type Xi    = ListBf[ListBf[ListBf[Double]]]


object HMMparamFactory {

  val r_gen = new Random

  def randomInit(vocabulary: List[String], stateN: Int): HMMparameter = {
    val initProb   = randomLogProb(stateN)
    val transeProb = Range(0, stateN).toList.map(stateK => randomLogProb(stateN))
    val emitProb   = Range(0, stateN).toList.map { stateK =>
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


class HMMparameter(_initProb: List[Double], _transeProb: List[ListBf[Double]], _emitProb: List[Map[String, Double]]) {
  val initProb   = _initProb    
  val transeProb = _transeProb  // transeProb(preState)(nextState)
  val emitProb   = _emitProb    // emitProb(state)(word)
}


// alphas(seq)(state), betas(seq)(state)
class FBparameter(_alphas: ListBf[ListBf[Double]], _betas: ListBf[ListBf[Double]]) {
  require(_alphas.size == _betas.size)
  val alphas = _alphas
  val betas  = _betas
  val seqN   = _alphas.size
  val stateN = _alphas.head.size

  val logLike: Double = Utils.calcLogSumExp(alphas.last.toList)

  // gamma(seq)(state)
  def convert2gamma: Gamma = {
    Range(0, seqN).toList.foldLeft(ListBuf.empty[ListBuf[Double]]) { (gammaN, seqK) =>
      Range(0, stateN).toList.foldLeft(ListBuf.empty[Double]) { (gammaNk, stateK) =>
        val gammaLogProb = alpahs(seqK)(stateK) + betas(seqK)(stateK) - logLike
        gammaNk += gammaLogProb
      }
    }
  }
  // xi(seq)(preState)(nextState)
  def convert2xi(hmmParam: HMMparameter, sentence: ListBf[String]): Xi = {
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
}

