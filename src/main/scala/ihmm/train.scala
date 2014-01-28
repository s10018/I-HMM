package scala.ihmm

import scala.io.Source
import scala.annotation.tailrec
import collection.mutable.ListBuffer

object Train {
  val usage = "usage: jave -jar PL-MRF.jar train -test testfile -layer layer_n -state state_n -c cut-off -dump dumpfile"

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

    println(testPath)
    println(dumpPath)
    println(layerN)
    println(stateN)
    println(cutOff)

    val sentences = readAndSetData(testPath, cutOff)
    //lowFreqWord.foreach(token => println(token))
  }

  def readAndSetData(testPath: String, cutOff: Int): ListBuffer[Array[String]] = {
    def convert2words(testPath: String): ListBuffer[Array[String]] = {
      def split2words(sentence: String): Array[String] = {
        sentence.split(" ")
      }
      val sentences = new ListBuffer[Array[String]]
      for(line <- Source.fromFile(testPath).getLines()) {
        sentences += split2words(line)
      }
      return sentences
    }
    def extractLowFreqWord(sentences: ListBuffer[Array[String]], cutOff: Int): List[String] = {
      def countFreq(sentences: ListBuffer[Array[String]]): collection.mutable.Map[String, Int] = {
        sentences.foldLeft(collection.mutable.Map.empty[String, Int]) { (mapC, sentArr) =>
          sentArr.toList.foldLeft(mapC) { (mapC_, token) =>
            mapC_ + (token -> (mapC_.getOrElse(token, 0) + 1))
          }
        }
      }
      val lowFreqWord = countFreq(sentences)
        .filter { case (token, count) => count < cutOff }
        .keys
        .toList
      return lowFreqWord
    }
    val sentences = convert2words(testPath)
    val lowFreqWord = extractLowFreqWord(sentences, cutOff)
    lowFreqWord.foreach(token => println(token))
    return sentences
  }
}
