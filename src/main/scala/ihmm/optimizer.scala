package scala.ihmm

import collection.mutable.{ListBuffer => ListBf}
import collection.mutalbe.{Map => mMap}

type Gamma = List[Array[Double]]
type Xi    = List[Array[Array[Double]]]


object Optimizer {

  val Threshold = 0.001

  def run(sentences: List[List[String]], vocabulary: List[String], stateN: Int): HMMparameter = {
    val param = new HMMparameter(Array(34.3), Array(Array(34.1)), Array(Map("hoge" -> 34.2)))
    return param
    def BaumWelch: HMMparameter = {
      def calcLogLike(fbParams: List[FBparameter]): Double = {
        fbParams.foldLeft(0.0) { (accumLogLike, fbParam) =>
          accumLogLike + fbParam.logLike
        }
      }
      def EStep(hmmParam: HMMparameter): List[FBParameter] = {
        sentences.map { sentence =>
          val alphas = ListBf.empty[Array[Double]] += (Range(0, stateN).toArray.map { stateK =>
            hmmParam.initProb(stateK) + hmmParam.emitProb(stateK)(sentence.head)
          })
          sentence.tail.foldLeft(alphas) { (_alphas, word) =>
            _alphas += (Range(0, stateN).toArray.map { stateK =>
              val logProbs = _alphas.last.zipWithIndex.map { (_prob, preStateK) =>
                _prob + hmmPram.transeProb(preStateK)(stateK)
              }
              Utils.logSumExp(logProbs) + hmmPram.emitProb(stateK)(word)
            })
          }
          val betas = ListBf.empty[Array[Double]] += (Range(0, stateN).toArray.map(_ => 0.0))
          sentence.tail.reverse.foldLeft(betas) { (_betas, word) =>
            val beta = Range(0, stateN).toArray.map { stateK =>
              val logProbs = _betas.head.zipWithIndex.map { (_prob, nextStateK) =>
                _prob + hmmParam.emitProb(nextStateK)(word) + hmmParam.transeProb(stateK)(nextStateK)
              }
              Utils.logSumExp(logProbs)
            }
            beta +=: betas
          }
          new FBparameter(alphas.toList, betas.toList)
        }
      }
      def MStep(fbParams: List[FBparameter], hmmParam: HMMparameter): HMMparameter = {
        def updateInitProb(gammas: List[Gamma]): Array[Double] = {
          val denom = Utils.logSumExp(gammas.map(gamma => Utils.logSumExp(gamma.head.toList)))
          Range(0, stateN).toArray.map { stateK =>
            Utils.logSumExp(gammas.map(gamma => gamma.head(stateK))) - denom
          }
        }
        def updateTranseProb(xis: List[Xi]): Array[Array[Double]] = {
          Range(0, stateN).toArray.map { preStateK =>
            val denom = Utils.logSumExp(xis.map { xi =>
              Utils.logSumExp(xi.map { xiN =>
                Utils.logSumExp(xiN(preStateK))
              })
            })
            Range(0, stateN).toArray.map { nextStateK =>
              Utils.logSumExp(xi.map { xiN =>
                Utils.logSumExp(xiN.map(xiNk => xiNk(preStateK)(nextStateK)))
              }) - denom
            }
          }
        }
        def updateEmitProb(gammas: List[Gamma]): Array[Map[String, Double]] = {
          Range(0, stateN).toArray.map { stateK =>
            val denom = Utils.logSumExp(gammas.map { gamma =>
              Utils.logSumExp(gamma.map(gammaN => gammaN(stateK)))
            })
            vocabulary.foldLeft(mMap.empty[String, Double]) { (emitMap, word) =>
              val emitLogProb = Utils.logSumExp(gammas.zip(sentences).foldLeft(Listbf.empty[Double]) { (logProbs, (gamma, sentence)) =>
                logProbs ++= gamma.zip(sentence).filter((gammaN, _word) => _word == word).map { (gammaN, _word) => gammaN(stateK) }
              }) - denom
              emitMap + (word -> emitLogProb)
            }.toMap
          }
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
