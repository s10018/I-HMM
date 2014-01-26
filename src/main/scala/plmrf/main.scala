package scala.plmrf

import scala.annotation.tailrec

object Main {

  val usage = "usage: java -jar PL-MRF.jar [train|decode] input_file"

  def ArgumentParse(map: Map[String, String], args: List[String]): Map[String, String] = {
    @tailrec
    def parse (map : Map[String, String], args: List[String]): Map[String, String] = {
      args match {
        case Nil => map
        case "train" :: file :: rest 
            => parse(map ++ Map("mode" -> "train") ++ Map("inputfile" -> file), rest)
        case "decode" :: file :: rest 
            => parse(map ++ Map("mode" -> "decode") ++ Map("inputfile" -> file), rest)
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
        case Some("train") => Train.train(opt.get("inputfile"))
        case Some("decode") => Decode.decode(opt.get("inputfile"))
        case _ => error("Illegal Argument!!")
      }
    } catch {
      case e: RuntimeException => println(usage)
    }
  }

}
