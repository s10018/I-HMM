package scala.ihmm

import scala.io.Source

object Decode {

  def outputFile() {
  }

  def parseDecode(map: Map[String, String], rest :List[String]): Map[String, String] = {
    Map("a"->"")
  }

  def decode(option: Map[String, String]): Unit = {
    option.get("inputfile") match {
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
