package com.github.omidb.nlp.formats

import dgraph._


object SExpression {

  def parse(s: String, checkQuote: Boolean = false) = {
    import fastparse.all._
    val whiteSpace = P((StringIn(" ", "\t") | StringIn("\r\n", "\n")).rep)
    val openBracket = P("(" ~ whiteSpace)
    val closeBracket = P(")" ~ whiteSpace)
    val nonQuoteTerminal =
      if (checkQuote)
        P(CharsWhile(c => !c.isWhitespace && c != '(' && c != ')' && c != '"').! ~ whiteSpace)
      else
        P(CharsWhile(c => !c.isWhitespace && c != '(' && c != ')').! ~ whiteSpace)

    val quote = P("\"" ~ CharsWhile(c => c != '"').! ~ "\"" ~ whiteSpace).map(x => "\"" + x + "\"")

    val terminal = if (checkQuote) nonQuoteTerminal | quote else nonQuoteTerminal

    def topNode: P[QNode[IndexedSeq[String], String]] = P(whiteSpace ~ node)
    def node: P[QNode[IndexedSeq[String], String]] = P(openBracket ~ (branch | leaf | emptyNodes) ~ closeBracket)

    def emptyNodes: P[QNode[IndexedSeq[String], String]] = P(node.rep(1))
      .map(y => QNode(IndexedSeq.empty, y.map(z => HalfEdge("", z)): _*))

    def branch: P[QNode[IndexedSeq[String], String]] = P(terminal.rep ~ node.rep(1))
      .map { case (x, y) => QNode(x.toIndexedSeq, y.map(z => HalfEdge("", z)): _*) }


    def leaf: P[QNode[IndexedSeq[String], String]] = P(terminal.rep).map(x => QNode(x.toIndexedSeq, EmptyHalfEdge))



    val res = topNode.parse(s)

    res match {
      case rs: Result.Success[QNode[IndexedSeq[String], String]] => {
        Some(DGraph.from(rs.get.value))
      }
      case _ => {
        None
      }
    }
  }

  def pprint(expr: DGraph[IndexedSeq[String], String]): String = {
    var pp = ""
    var tabIndx = -1
    def loop(nd: Int): Unit = {
      tabIndx += 1
      val pref1 = "  " * tabIndx + "(" + expr.nodes(nd).value.mkString(" ")
      pp += pref1

      expr.childs(nd).foreach(n => {
        pp += "\n"
        loop(n.id)
      })

      tabIndx -= 1
      pp += ")"
    }
    loop(expr.roots.head.id)
    pp
  }


}
