package scala.tools

import io.Source
import scopt.OptionParser
import scala.collection.parallel.immutable

case class Option(file:String = "",
  decode: Boolean = false, files: List[String] = List(), decfile: String = ""
)

class asInt(b: Boolean) {
  def toInt = if(b) "1" else "0"
}

object makeFeatures {

  implicit def convertBooleanToInt(b: Boolean) = new asInt(b)

  def makeSuffixesVect(surface: String): List[String] = {
    List("ing", "ogy", "ed", "s", "ly", "ion", "tion", "ity") map {
      s => (surface endsWith s) toInt
    }
  }
  def containsNumber(surface: String): String = {
    val numbers = Range(0, 10).map(_.toString)
    surface exists {
      w => numbers.contains(w.toString)
    } toInt
  }

  def extractFeature(line :String): List[String] = {
    val items = line.split('\t')
    val surface = items(1)
    val pos = items(4)
    List(surface) ::: makeSuffixesVect(surface) ::: List(containsNumber(surface), pos)
  }

  def surfacePOS(lines :String): String = lines.split('\t')(1) + "/" + lines.split('\t')(4)

  def splitBySeparator[T]( l: List[T], sep: T ): List[List[T]] = {
    l.span( _ != sep ) match {
      case (hd, _ :: tl) => hd :: splitBySeparator(tl, sep)
      case (hd, _) => List(hd)
    }
  }

  def parse_conll_file(filename: String, decode: Boolean): List[String] = {
    splitBySeparator(Source.fromFile(filename).getLines.toList, "") map {lines:List[String] =>
      val words = lines map {line:String =>
        extractFeature(line).mkString("\t")
      } 
      (words).mkString("\n") + "\n \n"
    }
  }

  def main(args: Array[String]): Unit = {

    val parser = new OptionParser[Option]("makeFeatures") {
      head("makeFeatures", "0.x")
      opt[Unit]("decode") action { (x, c) =>
        c.copy(decode = true)
      } text("decode for sentence")
      arg[String]("<file>...") unbounded() required() action { (x, c) =>
        c.copy(files = c.files :+ x) 
      } text("files extracted")
      help("help") text("prints this usage text")
    }

    parser.parse(args, Option()) map { opt =>
      opt.files foreach {file =>
        parse_conll_file(file, opt.decode) foreach (print(_))
      }
    } getOrElse {
      sys.exit(1)
    }

  }
}
