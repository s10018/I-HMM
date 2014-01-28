package scala.plmrf

import scala.io.Source

object Decode {

  def outputFile() {
  }

  def decode(inputfile: Option[String]): Unit = {
    inputfile match {
      case Some(filename) => {
        val document = Source.fromFile(filename)

        println("Loading....")

        // input the code for loading

        println("decoding Start!!")

        outputFile()

      }
      case _ => { error("Illegal Argument!!") }
    }
  }

}
