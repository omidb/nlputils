import com.github.omidb.nlp.formats.Pentree.PLabel$
import com.github.omidb.nlp.formats.SExpression
import dgraph.DGraph

object PlayGround extends App{

  val x:DGraph[IndexedSeq[String], String] = SExpression.parse("(ab1 (cd qj n) (ef))").get //"(+ 5 (+ 3 5))"

//  import com.github.omidb.nlp.formats.Pentree.PPOS._
  import dgraph.DGraphDSL._
  val q1 = query[IndexedSeq[String], String](
    <&(x => x.filter(_.startsWith("ab")).size > 0,
      -?>(x => true, <&(x => x.filter(_.startsWith("ef")).size > 0))
    )
  )

  println(PLabel.values)


}
