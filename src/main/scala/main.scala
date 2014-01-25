
import scala.io.Source

object Main {

  val usage = "usage: java -jar PL-MRF.jar [train|decode] input_file"

  def ArgumentParse(map : Map[String, Any], args: List[String]): Map[String, Any] = {
    def parse (map : Map[String, Any], args: List[String]): Map[String, Any] = {
      args match {
        case Nil => map
        case "train" :: file :: rest => {
          map ++ "mode" -> "train"
          map ++ "inputfile" -> file
          parse(map, rest)
        }
        case "decode" :: file :: rest => {
          map ++ "mode" -> "decode"
          map ++ "inputfile" -> file
          parse(map, rest)
        }
        case _ => {
          println(usage)
          sys.exit(0)
        }
      }
    }
    parse(map, args)
  }

  def main(args: Array[String]): Unit = {
    val opt = ArgumentParse(Map(), args.toList)
    opt foreach(println)
  }
}
