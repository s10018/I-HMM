package scala.plmrf

import scala.io.Source

object Decode {

  def decode(inputfile: Option[String]): Unit = {
    inputfile match {
      case Some(filename) => {
        val document = Source.fromFile(filename)
        println("decoding Start!!")
      }
      case _ => { error("Illegal Argument!!") }
    }
  }

}
