package scala.ihmm

import scala.util.Random
import java.io.PrintWriter
import collection.mutable.{ListBuffer => ListBf}




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
    val probMass = Range(0, sizeN).toList.map(i => r_gen.nextDouble + 0.5)
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

  def printInitProb(layerK: Int, fileP: PrintWriter): Unit = {
    initProb.zipWithIndex.foreach { probState =>
      fileP.println("I" + " "  + layerK.toString + " " + probState._2.toString + " " + probState._1.toString)
    }
  }
  def printTranseProb(layerK: Int, fileP: PrintWriter): Unit = {
    transeProb.zipWithIndex.foreach { probsPreState =>
      val probs    = probsPreState._1
      val preState = probsPreState._2
      probs.zipWithIndex.foreach { probState =>
        fileP.println("T" + " " + layerK.toString + " " + preState.toString + " " + probState._2.toString + " " + probState._1.toString)
      }
    }
  }
  def printEmitProb(layerK: Int, fileP: PrintWriter): Unit = {
    emitProb.zipWithIndex.foreach { pMapState =>
      val probPairs = pMapState._1.toList
      val state    = pMapState._2
      probPairs.foreach { probPair =>
        fileP.println("E" + " " + layerK.toString + " " + state.toString + " " + probPair._1 + " " + probPair._2)
      }
    }
  }
}


// alphas(seq)(state), betas(seq)(state)
class FBparameter(_alphas: List[Array[Double]], _betas: List[Array[Double]]) {

  type Gamma = List[Array[Double]]
  type Xi    = List[Array[Array[Double]]]

  require(_alphas.size == _betas.size)
  val alphas = _alphas
  val betas  = _betas
  val seqN   = _alphas.size
  val stateN = _alphas.head.size

  val logLike: Double = Utils.logSumExp(alphas.last.toList)

  // gamma(seq)(state)
  def convert2gamma: Gamma = {
    alphas.zip(betas).map { alphaBeta =>
      Range(0, stateN).toArray.map { stateK =>
        alphaBeta._1(stateK) + alphaBeta._2(stateK) - logLike
      }
    }
  }
  // xi(seq)(preState)(nextState)
  def convert2xi(hmmParam: HMMparameter, sentence: List[String]): Xi = {
    (betas.tail.zip(alphas.init)).zip(sentence.tail).map { betaAlWd =>
      val betaN     = betaAlWd._1._1
      val alphaPreN = betaAlWd._1._2
      val wordN     = betaAlWd._2
      Range(0, stateN).toArray.map { preStateK =>
        Range(0, stateN).toArray.map { nextStateK =>
          alphaPreN(preStateK) + hmmParam.emitProb(nextStateK)(wordN)
          + hmmParam.transeProb(preStateK)(nextStateK) + betaN(nextStateK) - logLike
        }
      }
    }
  }
}
