package scala.plmrf

import scala.io.Source
import scala.annotation.tailrec

object Train {

  def train(inputfile: Option[String]): Unit = {
    inputfile match {
      case Some(filename) => {
        val document = Source.fromFile(filename)
        println("training Start!!")
      }
      case _ => { error("Illegal Argument!!") }
    }
  }

}
