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

}
