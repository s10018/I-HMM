package scala.ihmm

import collection.mutable.{ListBuffer => ListBf}


object Optimizer {

  val Threshold = 0.001

  def run(sentences: ListBf[ListBf[String]], vocabulary: List[String],
    stateN: Int): HMMparameter = {
    val param = new HMMparameter(ListBf(34.3), ListBf(ListBf(34.1)), ListBf(Map("hoge" -> 34.2)))
    return param
    def BaumWelch: HMMparameter = {
      def calcLogLike(fbParams: List[FBparameter]): Double = {
        fbParams.foldLeft(0.0) { (accumLogLike, fbParam) =>
          accumLogLike + fbParam.calcLogLike
        }
      }
      def EStep(hmmParam: HMMparameter): List[FBParameter] = {
        sentences.foldLeft(ListBf.empty[FBParameter]) { (fbParams, sentence) =>
          val alphas = ListBf.empty[List[Double]] += (Range(0, stateN).toList.map { stateK =>
            hmmParam.initProb(stateK) + hmmParam.emitProb(stateK)(sentence.head)
          })
          sentence.tail.foldLeft(alphas) { (_alphas, word) =>
            _alphas += (Range(0, stateN).toList.map { stateK =>
              val logProbs = _alphas.last.zipWithIndex.map { (_prob, _stateK) =>
                _prob + hmmPram.transeProb(_stateK)(stateK)
              }
              logSumExp(logProbs) + hmmPram.emitProb(stateK)(word)
            })
          }
          val betas = ListBf.empty[List[Double]] += (Range(0, stateN).toList.map(_ => 0.0))
          sentence.tail.reverse.foldLeft(betas) { (_betas, word) =>
            val beta = Range(0, stateN).toList.map { stateK =>
              val logProbs = _betas.head.zipWithIndex.map { (_prob, _stateK) =>
                _prob + hmmParam.emitProb(_stateK)(word) + hmmParam.transeProb(stateK)(_stateK)
              }
              logSumExp(logProbs)
            }
            beta +=: betas
          }
          new FBparameter(alphas, betas)
        }
      }
      def MStep(param: FBparameter): HMMparameter = {
        "foo"
      }
      def _BaumWelch(oldHMMparam: HMMparameter, oldLogLike: Double): HMMparameter = {
        val newFBprams  = EStep(oldHMMparam)
        val newHMMparam = MStep(newFBparams)
        val newLoglike  = calcLogLike(newFBparams)
        if ((newLogLike - oldLogLike).abs < Threshold)
          newHMMparam
        else
          _BaumWelch(newHMMparam, newLogLike)
      }
      val initialHMMparam = HMMparamFactory.randomInit(vocabulary, stateN)
      val initialFBparams = EStep(initialHMMparam)
      val initialLogLike  = calcLogLike(initialFBparams)
      _BaumWelch(initialHMMparam, initialLogLike)
    }
  }
}
