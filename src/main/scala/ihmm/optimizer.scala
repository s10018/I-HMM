package scala.ihmm

import collection.mutable.{ListBuffer => ListBf}
import collection.mutable.{Map => mMap}




object Optimizer {
  type Gamma = List[Array[Double]]
  type Xi    = List[Array[Array[Double]]]

  val Threshold = 0.0001

  def run(sentences: List[List[String]], vocabulary: List[String], stateN: Int): HMMparameter = {
    def BaumWelch: HMMparameter = {
      def calcLogLike(fbParams: List[FBparameter]): Double = {
        fbParams.foldLeft(0.0) { (accumLogLike, fbParam) =>
          accumLogLike + fbParam.logLike
        }
      }
      def EStep(hmmParam: HMMparameter): List[FBparameter] = {
        sentences.map { sentence =>
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
          }
          val betas = ListBf.empty[Array[Double]] += (Range(0, stateN).toArray.map(_ => 0.0))
          sentence.tail.reverse.foldLeft(betas) { (_betas, word) =>
            val beta = Range(0, stateN).toArray.map { stateK =>
              val logProbs = _betas.head.zipWithIndex.map { probState =>
                probState._1 + hmmParam.emitProb(probState._2)(word) + hmmParam.transeProb(stateK)(probState._2)
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
          val xis2 = xis.filter(xi => xi.size != 0)
          Range(0, stateN).toArray.map { preStateK =>
            val denom = Utils.logSumExp(xis2.map { xi =>
              Utils.logSumExp(xi.map { xiN =>
                Utils.logSumExp(xiN(preStateK))
              })
            })
            Range(0, stateN).toArray.map { nextStateK =>
              Utils.logSumExp(xis2.map { xi =>
                Utils.logSumExp(xi.map( xiN => xiN(preStateK)(nextStateK) ))
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
              val emitLogProb = Utils.logSumExp(gammas.zip(sentences).foldLeft(ListBf.empty[Double]) { (logProbs, gammaSent) =>
                val gamma    = gammaSent._1
                val sentence = gammaSent._2
                logProbs ++= gamma.zip(sentence).filter( gammaWd => gammaWd._2 == word ).map( gammaWd => gammaWd._1(stateK) )
              }) - denom
              emitMap + (word -> emitLogProb)
            }.toMap
          }
        }
        val gammas = fbParams.map { fbParam => fbParam.convert2gamma }
        val xis    = fbParams.zip(sentences).map { fbParamSent => fbParamSent._1.convert2xi(hmmParam, fbParamSent._2) }

        new HMMparameter(updateInitProb(gammas), updateTranseProb(xis), updateEmitProb(gammas))
      }
      def _BaumWelch(oldHMMparam: HMMparameter, oldLogLike: Double): HMMparameter = {
        val newFBparams  = EStep(oldHMMparam)
        val newHMMparam  = MStep(newFBparams, oldHMMparam)
        val newLogLike   = calcLogLike(newFBparams)

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
    BaumWelch
  }
}
