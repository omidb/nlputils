package dgraph

import com.github.omidb.nlp.formats.SExpression
import utest._

import scala.collection.immutable.TreeMap

object DGraphCoreTest extends TestSuite {


  override def tests = TestSuite {
    'SimpleSExpr {
      val res0 = SExpression.parse2("(S (NP (NPR I))(VP man ))").get
      val sid = res0.filterNodes(_.head == "S").head.id
      val npid = res0.filterNodes(_.head == "NP").head.id
      val vpid = res0.filterNodes(_.head == "VP").head.id
      val sEdges = res0.edges.filter(_._1._1== sid)
      println(s"$sid,$npid, $sEdges")
      assert(sEdges.map(e => e._1._2).toList.contains(npid))
      assert(res0.filterNodes(_.head == "VP").head.value(0) == "VP")
      assert(res0.filterNodes(_.head == "VP").head.value(1) == "man")
      assert(res0.nodes.size == 4)
    }

    'SimpleSExpr2 {
      val res0 = SExpression.parse2("(A (B B1) C1 (D D1) D)").get
      res0.nodes.foreach(println)
      assert(res0.filterNodes(_.head == "A").head.value(0) == "A")
      assert(res0.filterNodes(_.head == "A").head.value(1) == "C1")
      assert(res0.filterNodes(_.head == "A").head.value(2) == "D")

    }
  }


}