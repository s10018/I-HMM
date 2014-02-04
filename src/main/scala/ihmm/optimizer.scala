package scala.ihmm

import collection.mutable.{ListBuffer => ListBf}
import collection.mutable.{Map => mMap}




object Optimizer {
  type Gamma = Array[Array[Double]]
  type Xi    = Array[Array[Array[Double]]]

  val Threshold = 0.001

  def run(sentences: Array[Array[String]], vocabulary: Array[String], stateN: Int): HMMparameter = {
    BaumWelch(sentences, vocabulary, stateN)
  }

  def BaumWelch(sentences: Array[Array[String]], vocabulary: Array[String], stateN: Int): HMMparameter = {
    val initialHMMparam = HMMparamFactory.randomInit(vocabulary, stateN)
    val initialLogLike  = sentences.iterator.foldLeft(0.0) { (_logLike, sentence) =>
      _logLike + Utils.logSumExp(calcAlpha(sentence, initialHMMparam, stateN).last)
    }
    _BaumWelch(initialHMMparam, initialLogLike, sentences, vocabulary, stateN)
  }

  def _BaumWelch(oldHMMparam: HMMparameter, oldLogLike: Double,
    sentences: Array[Array[String]], vocabulary: Array[String], stateN: Int): HMMparameter = {
    var newLogLike = 0.0

    val initProbMass:   Array[Double]               = Array.fill(stateN)(Double.NegativeInfinity)
    val transeProbMass: Array[Array[Double]]        = Array.tabulate(stateN) { _ => Array.fill(stateN)(Double.NegativeInfinity) }
    val emitProbMass:   Array[mMap[String, Double]] = Array.tabulate(stateN) { _ =>
      vocabulary.iterator.foldLeft(mMap.empty[String, Double]) { (_map, word) => _map + (word -> Double.NegativeInfinity) }
    }
    val sentN: Int = sentences.size
    var idx: Int   = 0
    while (idx < sentN) {
      val sentence = sentences(idx)
      // E Step
      val alpha = calcAlpha(sentence, oldHMMparam, stateN)
      val beta  = calcBeta( sentence, oldHMMparam, stateN)

      val fbParam = new FBparameter(alpha, beta)
      val gamma   = fbParam.convert2gamma
      val xi      = fbParam.convert2xi(oldHMMparam, sentence)
      newLogLike += fbParam.logLike

      // M Step
      Range(0, stateN).iterator.foreach { stateK =>
        initProbMass(stateK) = Utils.logSumExp2(initProbMass(stateK), gamma(0)(stateK))
      }
      if (sentence.size > 1) {
        Range(0, stateN).iterator.foreach { preStateK =>
          Range(0, stateN).iterator.foreach { nextStateK =>
            val seqProbSum = Utils.logSumExp( xi.map { xiN => xiN(preStateK)(nextStateK) } )
            transeProbMass(preStateK)(nextStateK) = Utils.logSumExp2(transeProbMass(preStateK)(nextStateK), seqProbSum)
          }
        }
      }
      Range(0, stateN).iterator.foreach { stateK =>
        vocabulary.iterator.foreach { word =>
          val emitedMass = gamma.zip(sentence).filter( gammaKword => gammaKword._2 == word ).map( gammaKword => gammaKword._1(stateK) )
          if (emitedMass.size != 0) {
            emitProbMass(stateK)(word) = Utils.logSumExp(emitProbMass(stateK)(word) +: emitedMass)
          }
        }
      }
      idx += 1
    }

    val initProbDenom = Utils.logSumExp(initProbMass)
    Range(0, stateN).iterator.foreach { stateK => initProbMass(stateK) -= initProbDenom }
    val transeProbDenoms = Range(0, stateN).iterator.map { stateK => Utils.logSumExp(transeProbMass(stateK)) }
    transeProbDenoms.zipWithIndex.foreach { denomPreStateK =>
      val denom     = denomPreStateK._1
      val preStateK = denomPreStateK._2
      Range(0, stateN).iterator.foreach { nextStateK => transeProbMass(preStateK)(nextStateK) -= denom }
    }
    val emitProbDenoms = Range(0, stateN).iterator.map { stateK => Utils.logSumExp(emitProbMass(stateK).values.toArray) }
    emitProbDenoms.zipWithIndex.foreach { denomStateK =>
      val denom  = denomStateK._1
      val stateK = denomStateK._2
      vocabulary.iterator.foreach { word => emitProbMass(stateK)(word) -= denom }
    }

    val newHMMparam = new HMMparameter(initProbMass, transeProbMass, emitProbMass.map( _mmap => _mmap.toMap ))

    if ((newLogLike - oldLogLike).abs < Threshold)
      newHMMparam
    else
      _BaumWelch(newHMMparam, newLogLike, sentences, vocabulary, stateN)
  }

  def calcAlpha(sentence: Array[String], hmmParam: HMMparameter, stateN: Int): Array[Array[Double]] = {
    val alphas = ListBf.empty[Array[Double]] += (Range(0, stateN).toArray.map { stateK =>
      hmmParam.initProb(stateK) + hmmParam.emitProb(stateK)(sentence.head)
    })
    sentence.tail.foldLeft(alphas) { (_alphas, word) =>
      _alphas += (Range(0, stateN).toArray.map { stateK =>
        val logProbs = _alphas.last.zipWithIndex.map { probState =>
          probState._1 + hmmParam.transeProb(probState._2)(stateK)
        }
        Utils.logSumExp(logProbs) + hmmParam.emitProb(stateK)(word)
      })
    }.toArray
  }
  def calcBeta(sentence: Array[String], hmmParam: HMMparameter, stateN: Int): Array[Array[Double]] = {
    val betas = ListBf.empty[Array[Double]] += ( Range(0, stateN).toArray.map(_ => 0.0) )
    sentence.tail.reverse.foldLeft(betas) { (_betas, word) =>
      val beta = Range(0, stateN).toArray.map { stateK =>
        val logProbs = _betas.head.zipWithIndex.map { probState =>
          probState._1 + hmmParam.emitProb(probState._2)(word) + hmmParam.transeProb(stateK)(probState._2)
        }
        Utils.logSumExp(logProbs)
      }
      beta +=: betas
    }.toArray
  }
}
