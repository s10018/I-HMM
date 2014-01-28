package scala.ihmm

import scala.io.Source
import scala.annotation.tailrec

object Train {

  def parseTrain(map: Map[String, String], rest :List[String]): Map[String, String] = {
    Map("a"->"")
  }

  def train(option: Map[String, String]): Unit = {
    option.get("inputfile") match {
      case Some(filename) => {
        val document = Source.fromFile(filename)
        println("training Start!!")
      }
      case _ => { error("Illegal Argument!!") }
    }
  }
  def _convert2words(file_path : String): collection.mutable.ListBuffer[Array[String]] = {
    def split2words(sentence: String): Array[String] = {
      sentence.split(" ")
    }
    val sentences = new collection.mutable.ListBuffer[Array[String]]
    for(line <- Source.fromFile(file_path).getLines()) {
      sentences += split2words(line)
    }

    return sentences
  }
}
