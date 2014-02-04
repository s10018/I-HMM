package scala.ihmm

import scala.io.Source
import java.io.PrintWriter
import scala.annotation.tailrec
import collection.mutable.{ListBuffer => ListBf}
import collection.mutable.{Map => mMap}
import collection.mutable.{ArrayBuffer => ArrayBf}



object Train {
  val usage = "usage: jave -jar PL-MRF.jar train -train trainfile -layer layer_n -state state_n -c cut-off -dump dumpfile"
  val UNKNOWN = "##UNKNOWN##"

  def parseTrain(opt: Map[String, String], rest :List[String]): Map[String, String] = {
    try {
      rest match {
        case Nil => opt
        case "-train" :: testPath :: rest
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

  def readAndSetData(testPath: String, cutOff: Int): (Array[Array[String]], Array[String]) = {
    val sentences   = convert2sentences(testPath)
    val wordFreq    = countFreq(sentences)
    val lowFreqWord = extractLowFreqWord(wordFreq, cutOff)
    val vocabulary  = examinVocabulary(wordFreq, lowFreqWord, cutOff)
    excludeLowFreqWord(sentences, lowFreqWord)

    (sentences, vocabulary)
  }
  def convert2sentences(testPath: String): Array[Array[String]] = {
    val s         = Source.fromFile(testPath)
    val sentences = ListBf.empty[Array[String]]

    s.getLines.foreach { line => sentences += split2words(line) }
    s.close
    sentences.toArray
  }
  def split2words(sentence: String): Array[String] = {
    sentence.split(" ")
  }
  def countFreq(sentences: Array[Array[String]]): mMap[String, Int] = {
    val wordFreq = mMap.empty[String, Int]
    sentences.iterator.foreach { sentence =>
      sentence.iterator.foreach { word =>
        wordFreq(word) = wordFreq.getOrElse(word, 0) + 1
      }
    }
    wordFreq
  }
  def extractLowFreqWord(wordFreq: mMap[String, Int], cutOff: Int): mMap[String, Int] = {
    wordFreq.filter(wdCount => wdCount._2 <= cutOff)
  }
  def excludeLowFreqWord(sentences: Array[Array[String]], lowFreqWord: mMap[String, Int]): Unit = {
    sentences.iterator.foreach { sentence =>
      Range(0, sentence.size).iterator.foreach { k =>
        sentence(k) = if (lowFreqWord.contains(sentence(k))) UNKNOWN else sentence(k)
      }
    }
  }
  def examinVocabulary(wordFreq: mMap[String, Int], lowFreqWord: mMap[String, Int], cutOff: Int): Array[String] = {
    val preVocabulary = wordFreq.filter { wdCount => wdCount._2 > cutOff }
    if (lowFreqWord.size == 0) {
      preVocabulary.keys.toArray
    } else {
      UNKNOWN +: preVocabulary.keys.toArray
    }
  }
}
