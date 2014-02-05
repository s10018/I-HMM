package scala.tools

import io.Source

object Count {

  def counter(filename: String): Unit = {
    val txt = Source.fromFile(filename).getLines.toList
    println(txt.par.flatMap(
      s => s.split(" ")
    ).groupBy(s => s).size)
  }

  def main(args: Array[String]): Unit = {
    if (args.size == 0) {
      println("usage: scala count.scala file1 file2 ...")
      sys.exit(1)
    }
    args foreach { counter(_) }
  }

}
