
package scala.ihmm

import scala.collection.mutable.Stack
import org.scalatest.FunSuite

class PLMRF_Decode_Test extends FunSuite {

  test("parseDecode should return Map('inputfile', 'mode')") {
    val opt = Decode.parseDecode(Map("mode" -> "decode"), List("hoge.txt"))
    assert(opt.contains("mode"))
    assert(opt.get("mode") == Some("decode"))
    assert(opt.contains("inputfile"))
    assert(opt.get("inputfile") == Some("hoge.txt"))
  }

  test("parseDecode should return Map('inputfile', 'mode') with weightfile") {
    val opt = Decode.parseDecode(Map("mode" -> "decode"), List("hoge.txt", "-p", "weightfile"))
    assert(opt.contains("mode"))
    assert(opt.get("mode") == Some("decode"))
    assert(opt.contains("inputfile"))
    assert(opt.get("inputfile") == Some("hoge.txt"))
    assert(opt.contains("probfile"))
    assert(opt.get("probfile") == Some("weightfile"))
  }

  test("ArgumentParse should get Map like below ver decode") {
    val opt = Main.ArgumentParse(Map(), List("decode", "file"))
    assert(opt.contains("mode"))
    assert(opt.get("mode") == Some("decode"))
    assert(opt.contains("inputfile"))
    assert(opt.get("inputfile") == Some("file"))
  }

}
