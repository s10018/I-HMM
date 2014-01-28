package scala.plmrf

import scala.annotation.tailrec

object Main {

  val usage = "usage: java -jar PL-MRF.jar [train|decode] input_file"

  def ArgumentParse(map: Map[String, String], args: List[String]): Map[String, String] = {
    @tailrec
    def parse (map : Map[String, String], args: List[String]): Map[String, String] = {
      args match {
        case Nil => map
        case "train" :: rest
            => Train.parseTrain(map ++ "mode" -> "train", rest)
        case "decode" :: rest
            => Decode.parseDecode(map ++ "mode" -> "decode", rest)
        case _ 
            => Map("mode" -> "")
      }
    }
    parse(map, args)
  }

  def main(args: Array[String]): Unit = {
    try {
      val opt = ArgumentParse(Map(), args.toList)
      opt.get("mode") match {
        case Some("train") => Train.train(opt)
        case Some("decode") => Decode.decode(opt)
        case _ => error("Illegal Argument!!")
      }
    } catch {
      case e: RuntimeException => println(usage)
    }
  }

}
