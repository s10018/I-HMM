package scala.ihmm

import scala.io.Source
import java.io.PrintWriter
import scala.annotation.tailrec
import collection.mutable.{ListBuffer => ListBf}
import collection.mutable.{Map => mMap}

object Train {
  val usage = "usage: jave -jar PL-MRF.jar train -test testfile -layer layer_n -state state_n -c cut-off -dump dumpfile"
  val UNKNOWN = "##UNKNOWN##"

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

    val (sentences, vocabulary) = readAndSetData(testPath, cutOff)

    println("Number of Sentence: " + sentences.size.toString)
    println("Number of Token: " + sentences.foldLeft(0)( (total, sent) => total + sent.size ).toString)
    println("Number of Vocabulary: " + vocabulary.size.toString)


    val hmmParams = Range(0, layerN).par.map { _ =>
      Optimizer.run(sentences, vocabulary, stateN)
    }.seq
    val fileP = new PrintWriter(dumpPath)

    fileP.println(layerN.toString + " " + stateN.toString)
    fileP.println(vocabulary.mkString(" "))

    hmmParams.zipWithIndex.foreach { case (hmmParam, layerK) =>
      hmmParam.printTranseProb(layerK, fileP)
    }
    hmmParams.zipWithIndex.foreach { case (hmmParam, layerK) =>
      hmmParam.printEmitProb(layerK, fileP)
    }
    hmmParams.zipWithIndex.foreach { case (hmmParam, layerK) =>
      hmmParam.printInitProb(layerK, fileP)
    }
    fileP.flush
    fileP.close
  }

  def readAndSetData(testPath: String, cutOff: Int): (List[List[String]], List[String]) = {
    def convert2sentences(testPath: String): ListBf[List[String]] = {
      def split2words(sentence: String): List[String] = {
        sentence.split(" ").toList
      }
      val s = Source.fromFile(testPath)
      val sentences = ListBf.empty[List[String]]

      for (line <- s.getLines) {
        sentences += split2words(line)
      }
      s.close
      sentences
    }
    def countFreq(sentences: ListBf[List[String]]): mMap[String, Int] = {
      sentences.foldLeft(mMap.empty[String, Int]) { (mapC, sentence) =>
        sentence.foldLeft(mapC) { (_mapC, word) =>
          _mapC + (word -> (_mapC.getOrElse(word, 0) + 1))
        }
      }
    }
    def extractLowFreqWord(wordFreq: mMap[String, Int]): mMap[String, Int] = {
      wordFreq.filter(wdCount => wdCount._2 <= cutOff)
    }
    def excludeLowFreqWord(sentences: ListBf[List[String]], lowFreqWord: mMap[String, Int]): List[List[String]] = {
      sentences.map { sentence =>
        sentence.map { word => if (lowFreqWord.contains(word)) UNKNOWN else word }
      }.toList
    }
    def examinVocabulary(wordFreq: mMap[String, Int], lowFreqWord: mMap[String, Int]): List[String] = {
      val preVocabulary = wordFreq.filter { wdCount => wdCount._2 > cutOff }
      if (lowFreqWord.size == 0) {
        preVocabulary.keys.toList
      } else {
        UNKNOWN :: (preVocabulary.keys.toList)
      }
    }
    val sentences   = convert2sentences(testPath)
    val wordFreq    = countFreq(sentences)
    val lowFreqWord = extractLowFreqWord(wordFreq)

    val sentences2  = excludeLowFreqWord(sentences, lowFreqWord)
    val vocabulary  = examinVocabulary(wordFreq, lowFreqWord)

    (sentences2, vocabulary)
  }
}
