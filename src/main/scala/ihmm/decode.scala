package scala.ihmm

import scala.io.Source
import breeze.stats.distributions

// 特徴
case class Features(word:String, latent_status:IndexedSeq[Int]) {
  override def toString(): String = word + "\t" + latent_status.mkString(" ")
}

// パラメータ
case class Param(param: Map[String, Int]) {
  def get(key: String): Int = param(key)
  def getRange(key: String): Range = Range(0, param(key))
}

// M 個分の確率
case class InitProb(prob: Map[Int, Double]) {
  def get(state: Int): Double = prob(state)
}
case class EmitProb(prob: Map[Int, Map[String, Double]]) {
  def get(state: Int, word: String): Double = prob(state).apply(word)
}
case class TransProb(prob: Map[Int, Map[Int, Double]]) {
  def get(state1: Int, state2: Int): Double = prob(state1).apply(state2)
}
case class State(pos:Int, state:Int)
case class Model(layer: Int, init: InitProb, emit: EmitProb, trans: TransProb)


object Decode {

  val UNK = "##UNKNOWN##"

  def output(result : List[IndexedSeq[Features]]) {
    result foreach { features => println(features.mkString("\n") + "\n") }
  }

  def parseDecode(map: Map[String, String], rest :List[String]): Map[String, String] = {
    val opt = rest match {
      case Nil => map
      case inputfile :: file :: tail
          => Map("inputfile" -> inputfile, "probfile" -> file, "mode" -> "decode")
      case _ => map
    }
    opt.get("inputfile") match {
      case Some( n ) => opt
      case None => Map("mode" -> "")
    }
  }

  def decoding(tokens: Array[String], param: Param, vocab: Set[String], models: Map[Int, Model]): 
      IndexedSeq[Features] = {

    val seqfs = for (m <- param.getRange("M")) yield {
      val model = models(m)

      // best_score expresses position i and state k's best score
      var best_score = collection.mutable.Map[Int, Map[Int, Double]]()
      var best_path = collection.mutable.Map[State, State]()

      best_score(0) = Map[Int, Double]()

      // init
      for (k <- Range(0, param.get("K"))) {
        if (vocab.contains(tokens(0)))
          best_score(0) += k -> (- model.init.get(k) - model.emit.get(k, tokens(0)))
        else
          best_score(0) += k -> (- model.init.get(k) - model.emit.get(k, UNK))
        best_path += State(0, k) -> State(-1, k)
      }
      // calculating best_score
      for (i <- Range(1, tokens.size)) { // i is position
        best_score(i) = Map[Int, Double]()
        for (k <- param.getRange("K")) {
          var tmp_result = Map[Int, Double]()
          for (l <- param.getRange("K")) {
            tmp_result += l -> (best_score(i-1)(k) - model.trans.get(l, k))
          }
          val best = tmp_result.minBy(_._2)
          best_path += State(i, k) -> State(i-1, best._1) // now -> prev path
          if (vocab.contains(tokens(i)))
            best_score(i) += k -> (- model.emit.get(k, tokens(i)) + best._2) // i th token's best score
          else
            best_score(i) += k -> (- model.emit.get(k, UNK) + best._2) // i th token's best score
        }
      }

      val pair = best_path filter {case (n, m) =>
        (n.pos == tokens.size - 1)
      } minBy {
        case (n, m) => best_score(n.pos)(n.state) 
      }

      def detectPath(pair: State, path: List[Int]): List[Int] = {
        pair match {
          case State(-1, m) => m :: path
          case State(n, m) => detectPath(best_path(State(n, m)), m :: path)
        }
      }
      detectPath(pair._2, List[Int]())
    }

    (0 until tokens.size) map {
      i => Features(tokens(i), (0 until seqfs.size) map {j => seqfs(j)(i) })
    }
  }

  def LoadModel(param: Param, probs: List[Array[String]]): Map[Int, Model] = {
    param.getRange("M") map {m =>
      def parse(init: Map[Int, Double], emit:Map[Int, Map[String, Double]],
        trans: Map[Int, Map[Int, Double]], rest: List[Array[String]]): Model = {
        rest match {
          case Nil => Model(m, InitProb(init), EmitProb(emit), TransProb(trans))
          case _ => {
            val head = rest.head
            head(0) match {
              case "I"
                  => parse(init ++ Map(head(2).toInt -> head(3).toDouble), emit, trans, rest.tail)
              case "E" => {
                val map = emit.get(head(2).toInt) match {
                  case Some(n) => Map(head(2).toInt -> (n ++ Map(head(3) -> head(4).toDouble)))
                  case None => Map(head(2).toInt -> Map(head(3) -> head(4).toDouble))
                }
                parse(init, emit ++ map, trans, rest.tail)
              }
              case "T" => {
                val map = trans.get(head(2).toInt) match {
                  case Some(n) => Map(head(2).toInt -> (n ++ Map(head(3).toInt -> head(4).toDouble)))
                  case None => Map(head(2).toInt -> Map(head(3).toInt -> head(4).toDouble))
                }
                parse(init, emit, trans ++ map, rest.tail)
              }
            }
          }
        }
      }
      val i = Map[Int, Double]()
      val t = Map[Int, Map[Int, Double]]()
      val e = Map[Int, Map[String, Double]]()
      m -> parse(i, e, t, probs filter {line => line(1) == m.toString} )
    } toMap
  }

  def makeParam(head: Array[String]): Param
    = Param(Map("M" -> head(0).toInt, "K" -> head(1).toInt))

  def LoadFile(filename: String): List[Array[String]] 
    = Source.fromFile(filename).getLines().map(_.split(' ')).toList

  def decode(option: Map[String, String]): Unit = {

    val filename = option.get("inputfile") match {
      case Some(filename) => filename
      case None => ""
    }
    val probfilename = option.get("probfile") match {
      case Some(filename) => filename
      case None => ""
    }

    val document = LoadFile(filename)
    val probs = LoadFile(probfilename)
    val param = makeParam(probs.head)
    val vocab = probs.tail.head.toSet
    val models = LoadModel(param, probs.tail.tail)
    output(document.map {
      sentence => decoding(sentence, param, vocab, models)
    })

  }

}
