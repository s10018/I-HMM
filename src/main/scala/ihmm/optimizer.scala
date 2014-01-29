package scala.ihmm

import collection.mutable.{ListBuffer => ListBf}

type Gamma = ListBf[ListBf[Double]]
type Xi    = ListBf[ListBf[ListBf[Double]]]


object Optimizer {

  val Threshold = 0.001

  def run(sentences: ListBf[ListBf[String]], vocabulary: List[String],
    stateN: Int): HMMparameter = {
    val param = new HMMparameter(ListBf(34.3), ListBf(ListBf(34.1)), ListBf(Map("hoge" -> 34.2)))
    return param
    def BaumWelch: HMMparameter = {
      def calcLogLike(fbParams: ListBf[FBparameter]): Double = {
        fbParams.foldLeft(0.0) { (accumLogLike, fbParam) =>
          accumLogLike + fbParam.logLike
        }
      }
      def EStep(hmmParam: HMMparameter): ListBf[FBParameter] = {
        sentences.foldLeft(ListBf.empty[FBParameter]) { (fbParams, sentence) =>
          val alphas = ListBf.empty[List[Double]] += (Range(0, stateN).toList.map { stateK =>
            hmmParam.initProb(stateK) + hmmParam.emitProb(stateK)(sentence.head)
          })
          sentence.tail.foldLeft(alphas) { (_alphas, word) =>
            _alphas += (Range(0, stateN).toList.map { stateK =>
              val logProbs = _alphas.last.zipWithIndex.map { (_prob, _stateK) =>
                _prob + hmmPram.transeProb(_stateK)(stateK)
              }
              Utils.logSumExp(logProbs) + hmmPram.emitProb(stateK)(word)
            })
          }
          val betas = ListBf.empty[ListBf[Double]] += (Range(0, stateN).toList.map(_ => 0.0))
          sentence.tail.reverse.foldLeft(betas) { (_betas, word) =>
            val beta = Range(0, stateN).toList.map { stateK =>
              val logProbs = _betas.head.zipWithIndex.map { (_prob, _stateK) =>
                _prob + hmmParam.emitProb(_stateK)(word) + hmmParam.transeProb(stateK)(_stateK)
              }
              Utils.logSumExp(logProbs)
            }
            beta +=: betas
          }
          new FBparameter(alphas, betas)
        }
      }
      def MStep(fbParams: ListBf[FBparameter], hmmParam: HMMparameter): HMMparameter = {
        def updateInitProb(gammas: ListBf[Gamma]): ListBf[Double] = {
          val denom = Utils.logSumExp(gammas.map(gamma => Utils.logSumExp(gamma.head)))
          Range(0, stateN).toList.map { stateK =>
            Utils.logSumExp(gammas.map(gamma => gamma.head(stateK))) - denom
          }
        }
        def updateTranseProb(xis: ListBf[Xi]): ListBf[ListBf[Double]] = {
          Range(0, stateN).toList.map { preStateK =>
            val denom = Utils.logSumExp(xis.map { xi =>
              Utils.logSumExp(xi.map { xiN =>
                Util.logSumExp(xiN(preStateK))
              })
            })
            Range(0, stateN).toList.map { nextStateK =>
              Utils.logSumExp(xi.map { xiN =>
                Utils.logSumExp(xiN.map(xiNk => xiNk(preStateK)(nextStateK)))
              }) - denom
            }
          }
        }
        def updateEmitProb(gammas: ListBf[Gamma]): ListBf[Map[String, Double]] = {

        }
        val gammas = fbParams.map { fbParam => fbParam.convert2gamma }
        val xis    = fbParams.zip(sentences).map { (fbParam, sentence) => fbParam.convert2xi(hmmParam, sentence) }

        new HMMparameter(updateInitProb(gammas), updateTranseProb(xis, gammas), updateEmitProb(gammas))
      }
      def _BaumWelch(oldHMMparam: HMMparameter, oldLogLike: Double): HMMparameter = {
        val newFBprams  = EStep(oldHMMparam)
        val newHMMparam = MStep(newFBparams, olfHMMparam)
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
