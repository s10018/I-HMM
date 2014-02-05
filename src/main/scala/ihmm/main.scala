package scala.ihmm

import scala.tools._

object Main {

  val usage = "usage: java -jar PL-MRF.jar [train|decode|conll] input_file"

  def ArgumentParse(map: Map[String, String], args: List[String]): Map[String, String] = {
    def parse (map : Map[String, String], args: List[String]): Map[String, String] = {
      args match {
        case Nil => map
        case "train" :: rest
            => Train.parseTrain(map ++ Map("mode" -> "train"), rest)
        case "decode" :: rest
            => Decode.parseDecode(map, rest)
        case "conll" :: rest
            => Map("mode" -> "conll", "file" -> rest.mkString(" "))
        case _ 
            => Map("mode" -> "")
      }
    }
    parse(map, args)
  }

  def main(args: Array[String]): Unit = {
   //try {
    val opt = ArgumentParse(Map(), args.toList)

    opt.get("mode") match {
      case Some("train") => Train.train(opt)
      case Some("decode") => Decode.decode(opt)
      case Some("conll") => ConllParse.parse(opt)
      case _ => sys.error("Illegal Argument!!")
    }

    // } catch {
    //   case e: RuntimeException => println(usage)
    // }
  }

}
