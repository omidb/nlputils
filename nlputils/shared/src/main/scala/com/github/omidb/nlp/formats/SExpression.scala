package com.github.omidb.nlp.formats

import dgraph._


object SExpression {

  def parse(s:String) = {
    import fastparse.all._
    val whiteSpace = P((StringIn(" ", "\t") | StringIn("\r\n", "\n")).rep)
    val openBracket = P("(" ~ whiteSpace)
    val closeBracket = P(")" ~ whiteSpace)
    val nonQuoteTerminal = P (CharsWhile(c => !c.isWhitespace && c != '(' && c != ')' && c != '"').! ~ whiteSpace)
    val quote = P( "\"" ~ CharsWhile(c => c != '"').! ~ "\"" ~ whiteSpace).map(x => "\""+ x +"\"")

    val terminal = nonQuoteTerminal | quote



    def node:P[QNode[IndexedSeq[String],String]] = P(openBracket ~ (branch | leaf | emptyNodes) ~ closeBracket)

    def emptyNodes:P[QNode[IndexedSeq[String],String]] = P(node.rep(1))
      .map(y => QNode(IndexedSeq.empty, y.map(z => HalfEdge("", z)):_*))

    def branch:P[QNode[IndexedSeq[String],String]] = P(terminal.rep ~ node.rep(1))
      .map{case(x,y) => QNode(x.toIndexedSeq, y.map(z => HalfEdge("", z)):_*)}


    def leaf:P[QNode[IndexedSeq[String],String]] = P(terminal.rep).map(x =>  QNode(x.toIndexedSeq, EmptyHalfEdge))


    println(quote.parse(""""sup" """))
    val res = node.parse(s)



    res match {
      case rs:Result.Success[QNode[IndexedSeq[String],String]] => {
        println(rs.get.value)
        Some(DGraph.from(rs.get.value))
      }
      case _ => {
        println(res)
        None
      }
    }
  }


}
