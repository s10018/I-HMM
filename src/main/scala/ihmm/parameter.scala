package scala.ihmm

import scala.util.Random
import java.io.PrintWriter
import collection.mutable.{ListBuffer => ListBf}



object HMMparamFactory {

  val r_gen = new Random

  def randomInit2(vocabulary: Array[String], stateN: Int): HMMparameter = {
    val initProb   = randomLogProb(stateN)
    val transeProb = Range(0, stateN).toArray.map( stateK => randomLogProb(stateN) )
    val emitProb   = Range(0, stateN).toArray.map { stateK =>
      vocabulary.zip(randomLogProb(vocabulary.size)).toMap
    }
    new HMMparameter(initProb, transeProb, emitProb)
  }
  def randomLogProb(sizeN: Int): Array[Double] = {
    val probMass = Range(0, sizeN).iterator.map(i => r_gen.nextDouble + 0.5)
    val logNmlz  = math.log(probMass.sum)
    probMass.map { pMass =>
      math.log(pMass) - logNmlz
    }.toArray
  }
  def randomInit(vocabulary: Array[String], stateN: Int): HMMparameter = {
    val initProb   = DirRand.logRandom(stateN, 0.5).toArray
    val transeProb = Range(0, stateN).toArray.map( stateK => DirRand.logRandom(stateN, 0.5).toArray )
    val emitProb   = Range(0, stateN).toArray.map { stateK =>
      vocabulary.zip(DirRand.logRandom(vocabulary.size, 0.5)).toMap
    }
    new HMMparameter(initProb, transeProb, emitProb)
  }
  def meanDistInit(vocabulary: Array[String], stateN: Int): HMMparameter = {
    val mean1 = math.log(1.0 / stateN)
    val mean2 = math.log(1.0 / vocabulary.size)

    val initProb   = Range(0, stateN).toArray.map(_ => mean1)
    val transeProb = Range(0, stateN).toArray.map( stateK => Range(0, stateN).toArray.map(_ => mean1) )
    val emitProb   = Range(0, stateN).toArray.map { stateK =>
      vocabulary.zip(Array.fill(vocabulary.size)(mean2)).toMap
    }
    new HMMparameter(initProb, transeProb, emitProb)
  }
}


class HMMparameter(_initProb: Array[Double], _transeProb: Array[Array[Double]], _emitProb: Array[Map[String, Double]]) {
  val initProb   = _initProb    
  val transeProb = _transeProb  // transeProb(preState)(nextState)
  val emitProb   = _emitProb    // emitProb(state)(word)

  def printInitProb(layerK: Int, fileP: PrintWriter): Unit = {
    initProb.iterator.zipWithIndex.foreach { probState =>
      fileP.println("I" + " "  + layerK.toString + " " + probState._2.toString + " " + probState._1.toString)
    }
    //println("I: " + Utils.logSumExp(initProb).toString)
  }
  def printTranseProb(layerK: Int, fileP: PrintWriter): Unit = {
    transeProb.iterator.zipWithIndex.foreach { probsPreState =>
      val probs    = probsPreState._1
      val preState = probsPreState._2
      probs.iterator.zipWithIndex.foreach { probState =>
        fileP.println("T" + " " + layerK.toString + " " + preState.toString + " " + probState._2.toString + " " + probState._1.toString)
      }
      //println("E: " + Utils.logSumExp(probs).toString)
    }
  }
  def printEmitProb(layerK: Int, fileP: PrintWriter): Unit = {
    emitProb.iterator.zipWithIndex.foreach { pMapState =>
      val probPairs = pMapState._1.toList
      val state    = pMapState._2
      probPairs.iterator.foreach { probPair =>
        fileP.println("E" + " " + layerK.toString + " " + state.toString + " " + probPair._1 + " " + probPair._2)
      }
      //println("E: " + Utils.logSumExp(probPairs.map { pair => pair._2 }).toString)
    }
  }
}


// alphas(seq)(state), betas(seq)(state)
class FBparameter(_alphas: Array[Array[Double]], _betas: Array[Array[Double]]) {

  type Gamma = Array[Array[Double]]
  type Xi    = Array[Array[Array[Double]]]

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
  def convert2xi(hmmParam: HMMparameter, sentence: Array[String]): Xi = {
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
