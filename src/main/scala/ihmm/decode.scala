package scala.ihmm

import scala.io.Source
import breeze.stats.distributions

// 特徴
class Features(w:String, latent_size:Int) {
  val latent_status = Array.fill(latent_size)(0);
  val word = w
  override def toString(): String = word + "\t" + latent_status.mkString(" ")
}

// パラメータ
case class Param(param: Map[String, Int]) {
  def get(key: String): Int = param(key)
  def getRange(key: String): Range = {
    key match {
      case n:String => Range(0, param(n))
      case _ => Range(0, 10) 
    }
  }
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
case class Model(layer: Int, init: InitProb, emit: EmitProb, trans: TransProb)

object Decode {

  implicit def strEmit(s: Array[String]) =
    new { def toEmit() = Array(s(0), s(1).toInt, s(2).toInt, s(3), s(4).toDouble) }

  implicit def strTrans(s: Array[String]) =
    new { def toTrans() = Array(s(0), s(1).toInt, s(2).toInt, s(3).toInt, s(4).toDouble) }

  val UNK = "##UNKNOWN##"

  def output(result : Iterator[Array[Features]]) {
    result foreach { features => println(features.mkString("\n") + "\n") }
  }

  def parseDecode(map: Map[String, String], rest :List[String]): Map[String, String] = {
    def parse(map: Map[String, String], rest :List[String]): Map[String, String] = {
      rest match {
        case Nil => map
        case "-p" :: file :: tail => parse(map ++ Map("probfile" -> file), tail)
        case inputfile :: tail => parse(map ++ Map("inputfile" -> inputfile), tail)
      }
    }
    val opt = parse(map, rest)
    opt.get("inputfile") match {
      case Some( n ) => opt
      case None => Map("mode" -> "")
    }
  }

  def decoding(tokens: Array[String], param: Param, model: Model): Array[Features] = {

    // Viterbi algorithm
    var features = tokens.map{ token => new Features(token, 4) }

    for (m <- param.getRange("M")) {

      // best_score expresses position i and state k's best score
      var best_score = collection.mutable.Map[Int, Map[Int, Double]]()
      var best_path = collection.mutable.Map[Int, Int]()

      best_score(-1) = Map[Int, Double]()
      // init
      for (k <- param.getRange("K")) 
        best_score(-1) += k -> model.init.get(k)

      // calculating best_score
      for ((token, i) <- tokens.zipWithIndex) { // i is position
        for (k <- param.getRange("K")) { 
          var tmp_result = Map[Int, Double]()
          for (l <- param.getRange("K")) {
            tmp_result += l -> (best_score(i-1)(k) + model.trans.get(l, k))
          }
          val best = tmp_result.maxBy(_._2)
          best_path += best._1 -> k
          best_score ++ Map(i -> Map(k -> (model.emit.get(k, token) + best._2))) // i th token's best score
        }
      }
      // val path = best_path(param.get("K") - 1).minBy()
      // // detect best_path
      // def detect(rest: Map[Int, Int], features: List[Int]) {
      //   rest match {
      //     case Nil => features
      //     case head :: tail => detect(tail, features)
      //   }
      // }
      // detect()
      // update Feature
    }

    tokens.map{ token => new Features(token, 4) }
  }

  def LoadModel(param: Param, probs: List[Array[String]]): Map[Int, Model] = {
    // ここ書く
    param.getRange("M") map {m =>

      val init = InitProb(probs filter {
        p => p.head == "I" && p(1) == m.toString
      } map {p => p(2).toInt -> p(3).toDouble } toMap)

      // val emit = probs filter {
      //   p => p.head == "E" && p(1) == m.toString
      // } map {p => p(2) -> Map(p(3) -> p(4))}

      val trans = TransProb(Map[Int, Map[Int, Double]]())

      val emit = EmitProb(Map[Int, Map[String, Double]]())

      m -> Model(m, init, emit, trans)
    } toMap
  }

  def makeParam(head: Array[String]): Param = Param(Map("M" -> head(0).toInt, "K" -> head(1).toInt))

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

    val document = Source.fromFile(filename).getLines()
    val probs = LoadFile(probfilename)
    val param = makeParam(probs.head)
    val model = LoadModel(param, probs.tail)

    output(document.map {
      sentence => decoding(sentence.split(' '), param, model(0))
    })

  }

}
