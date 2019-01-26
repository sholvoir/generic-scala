import java.io.File

import org.scalatest._
import cn.micit.generic._

/** class TableFileSpec extends FlatSpec with Matchers {
  "TableFile" should "have tests" in {
    var h: String = null
    val rows = TableFile.read(new File("testable0.txt")){ header =>
      h = header
      header.split("\\s") should be(Array("col1", "col2"))
      line => line.split("\\s")
    }
    TableFile.write(new File("testable1.txt"))(h)(rows)(_.mkString("\t"))
    TableFile.conditionFilter(new File("testable1.txt"))(new File("testable2.txt")) { line => _ =>
      line.split("\\s")(0) == "c"
    }
    TableFile.changeFilter(new File("testable0.txt"))(new File("testable3.txt")) { line => _ =>
      line.split("\\s")(0)
    }
  }
}*/

class OptionParserSpec extends FlatSpec with Matchers {
  "OptionParser" should "parse options and arguments" in {
    val args = Array("ddd", "-g", "aaa", "ccc", "-p", "-t", "bbb", "--xxx")
    val options = Options.parse(args)
    options should be(Map("" -> "ddd\tccc", "-g" -> "aaa", "-p" -> "", "-t" -> "bbb", "--xxx" -> ""))
  }
  it should "parse opptions from a String" in {
    Options.parse("name,id=id1,sex=male,nobar".split(",").map(x => x.split("="))) should be(Map(""->"name","id"->"id1","sex"->"male","nobar"->""))
  }
}

class IteratorsSpec extends FlatSpec with Matchers {
  "Iterators" should "combine iterators" in {
    val a1 = Seq(101, 202, 303, 404).iterator
    val a2 = Seq(110, 220, 330, 440).iterator
    val a3 = Iterators.combine(Seq(a1, a2)).toSeq
    a3 should be(Seq(101, 110, 202, 220, 303, 330, 404, 440))
  }
  it should "intersectZip two iterators" in {
    val a1 = Seq(101, 202, 303, 404).iterator
    val a2 = Seq("202", "404").iterator
    val a3 = Iterators.intersectZip(a1)(a2)(_ - _.toInt).toSeq
    a3 should be(Seq((202, "202"), (404, "404")))
  }
  it should "unionZip two iterators" in {
    val a1 = Seq(101, 202, 404).iterator
    val a2 = Seq("202", "303", "404").iterator
    val a3 = Iterators.unionZip(a1)(a2)(0)("")(_ - _.toInt).toSeq
    a3 should be(Seq((101, ""), (202, "202"), (0, "303"), (404, "404")))
  }
}