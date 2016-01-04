package com.github.omidb.nlp.formats

import dgraph._

import scala.collection.immutable.TreeMap


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


  def parse2(s: String, checkQuote: Boolean = false) = {
    var qstack = List.empty[Node[IndexedSeq[String]]]
    def isWhiteSpace(c:Char) = c.isWhitespace || c == '\n'
    var index = 0
    var token = ""
    var justCheckedWhiteSpaces = false
    var nIndex = 0
    val nodes = collection.mutable.Map.empty[Int, Node[IndexedSeq[String]]]
    var edges = TreeMap.empty[(Int,Int), DEdge[String]]
    var graph = DGraph.empty[IndexedSeq[String],String]()
    var oneNode = false

    def addNewAtr(str:String) = {
      qstack = qstack.updated(0, qstack.head.copy(value = qstack.head.value :+ str))
      token = ""
    }

    def addNewChild() = {
      val newNode = Node[IndexedSeq[String]](IndexedSeq.empty[String],nIndex)
      if(qstack.nonEmpty) edges = edges.updated((qstack.head.id, newNode.id), DEdge("",qstack.head.id, newNode.id))
      qstack =  newNode :: qstack
      nIndex += 1
    }

    while(index < s.length){
      val c = s.charAt(index)
      if(isWhiteSpace(c)){
        if(!justCheckedWhiteSpaces && token != "" && qstack.nonEmpty){
          addNewAtr(s"$token")
          justCheckedWhiteSpaces = true
        }
      }
      else if(c == '('){
        oneNode = true
        justCheckedWhiteSpaces = false
        if(token != "" && qstack.nonEmpty) addNewAtr(token)
        addNewChild()
      }
      else if(c == ')'){
        if(token != "" && qstack.nonEmpty) addNewAtr(token)
        //
        nodes.update(qstack.head.id,qstack.head)
        qstack = qstack.tail
      }
      else {
        justCheckedWhiteSpaces = false
        token += c
      }
      index +=1
    }

    nodes.foreach(n => println(n._2))
    edges.foreach(e => println(e._2))
    if(qstack.isEmpty && oneNode)
      Some(DGraph.from(nodes.toMap,edges))
    else None
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
