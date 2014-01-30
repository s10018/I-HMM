package scala.ihmm

import scala.io.Source
import scala.annotation.tailrec
import collection.mutable.{ListBuffer => ListBf}
import collection.mutable.{Map => mMap}

object Train {
  val usage = "usage: jave -jar PL-MRF.jar train -test testfile -layer layer_n -state state_n -c cut-off -dump dumpfile"
  val unknow = "##UNKOWN##"

  def parseTrain(opt: Map[String, String], rest :List[String]): Map[String, String] = {
    try {
      rest match {
        case Nil => opt
        case "-test" :: testPath :: rest
            => parseTrain(opt ++ Map("testPath" -> testPath), rest)
        case "-layer" :: layerN :: rest
            => parseTrain(opt ++ Map("layerN"   -> layerN  ), rest)
        case "-state" :: stateN :: rest
            => parseTrain(opt ++ Map("stateN"   -> stateN  ), rest)
        case "-c" :: cutOff :: rest
            => parseTrain(opt ++ Map("cutOff"   -> cutOff  ), rest)
        case "-dump" :: dumpPath :: rest
            => parseTrain(opt ++ Map("dumpPath" -> dumpPath), rest)
        case _
            => Map("hoge" -> "")
      }
    } catch {
      case e: RuntimeException => Map("hoge" -> "")
    }
  }

  def train(opt: Map[String, String]): Unit = {
    val testPath: String = opt("testPath")
    val dumpPath: String = opt("dumpPath")

    val layerN: Int = opt("layerN").toInt
    val stateN: Int = opt("stateN").toInt
    val cutOff: Int = opt("cutOff").toInt

    println(testPath);println(dumpPath);println(layerN);println(stateN);println(cutOff)

    val sentences = readAndSetData(testPath, cutOff)
    val vocabulary = examinVocabulary(sentences)
    sentences.foreach { sent =>
      sent.foreach { word =>
        print(word + " ")
      }
      print("\n")
    }
    vocabulary.foreach(println)
    println(Optimizer.run(sentences, vocabulary, stateN))
  }

  def readAndSetData(testPath: String, cutOff: Int): List[List[String]] = {
    def convert2sentences(testPath: String): List[List[String]] = {
      def split2words(sentence: String): List[String] = {
        sentence.split(" ").toList
      }
      val sentences = ListBf.empty[List[String]]
      for(line <- Source.fromFile(testPath).getLines()) {
        sentences += split2words(line)
      }
      return sentences.toList
    }
    def extractLowFreqWord(sentences: List[List[String]], cutOff: Int): List[String] = {
      def countFreq(sentences: List[List[String]]): Map[String, Int] = {
        sentences.foldLeft(mMap.empty[String, Int]) { (mapC, sentence) =>
          sentence.foldLeft(mapC) { (mapC_, word) =>
            mapC_ + (word -> (mapC_.getOrElse(word, 0) + 1))
          }
        }.toMap
      }
      countFreq(sentences).filter(wdCount => wdCount._2 < cutOff).keys.toList
    }
    val sentences   = convert2sentences(testPath)
    val lowFreqWord = extractLowFreqWord(sentences, cutOff)

    sentences.map { sentence =>
      sentence.map( word => if (lowFreqWord.contains(word)) unknow else word )
    }
  }

  def examinVocabulary(sentences: List[List[String]]): List[String] = {
    sentences.foldLeft(ListBf.empty[String]) { (vocab, sentence) =>
      vocab ++= sentence.distinct
    }
    .distinct.toList
  }
}
