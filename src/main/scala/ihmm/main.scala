package scala.ihmm

object Main {

  val usage = "usage: java -jar PL-MRF.jar [train|decode] input_file"

  def ArgumentParse(map: Map[String, String], args: List[String]): Map[String, String] = {
    def parse (map : Map[String, String], args: List[String]): Map[String, String] = {
      args match {
        case Nil => map
        case "train" :: rest
            => Train.parseTrain(map ++ Map("mode" -> "train"), rest)
        case "decode" :: rest
            => Decode.parseDecode(map ++ Map("mode" -> "decode"), rest)
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
