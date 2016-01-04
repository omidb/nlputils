package com.github.omidb.nlp.toolsInterface

import com.github.omidb.nlp.formats.SExpression
import dgraph.{DEdge, DGraph, Node}
import fastparse.all.parserApi


class TripsOnline extends TripsOnlineParser {
  import scalaj.http.{Http, HttpResponse}
  override def parse(server:String, text:String) = {
    sendRequest(server,text).map(body => processXML(body).toList)
  }

  def fromStrig(str:String) = processXML(str)

  def fromFile(path:String) = {
    import better.files._
    processXML(File(path).lines.mkString("\n"))
  }

  private def sendRequest(server:String, text:String) = {
    val response: Option[HttpResponse[String]] =
      try {
        Some(
          Http(server)
            .timeout(connTimeoutMs = 10000, readTimeoutMs = 10000)
            .param("genre", "text")
            .param("input", text)
            .params(Seq(
              ("tagsformat", "hidden"), ("treecontents", "phrase"), ("treeformat", "LinGO"),
              ("lfformat", "svg"), ("no-sense-words", ""), ("extsformat", "table"),
              ("tag-type", "%28or+terms+words+punctuation+%28and+stanford_core_nlp+%28or+named-entity+pos%29%29+street_addresses+capitalized_names+alphanumerics+quotations+alternate_spellings%29"),
              ("senses-only-for-penn-poss", ""))).asString
        )
      } catch {case _ => None}

    if(response.isDefined && response.get.isSuccess) Some(response.get.body) else None
  }

  private def processXML(body:String) = {
    val f = javax.xml.parsers.SAXParserFactory.newInstance()
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
    val sxp = f.newSAXParser()
    val xml = scala.xml.XML.withSAXParser(sxp).loadString(body)

    val containsMultipleSentence = xml \\ "trips-parser-output" contains "compound-communication-act"
    val mainNodes =
      if(containsMultipleSentence) xml \\ "trips-parser-output" \\ "compound-communication-act"
      else xml \\ "trips-parser-output"

//    val words = mainNodes \\ "utt" \\ "words" \\ "lisp" map(_.text) map(s => SExpression.parse2(s))
//    val tags = mainNodes \\ "utt" \\ "tags" \\ "lisp" map(_.text) map(s => SExpression.parse2(s))
//    val trees = mainNodes \\ "utt" \\ "tree" \\ "lisp" map(_.text) map(s => SExpression.parse2(s))
    val terms = mainNodes \\ "utt" \\ "terms" \\ "RDF" map processRDF
    val termsRoot = mainNodes \\ "utt" \\ "terms" map(_.attribute("root").get.toString.substring(1))

    val results = terms.zip(termsRoot).map{case(gr,root) => TripsLF(gr,gr.filterNodes(n=> n("ID") == root).headOption.map(_.id))}
    results
  }

  private def processRDF(node:scala.xml.Node) = {
    val rdfNS = node.scope.getURI("rdf")
    val LFNS = node.scope.getURI("LF")
    val roleNS = node.scope.getURI("role")
    val nodes = collection.mutable.Map.empty[Int,Node[Map[String,String]]]
    val nodeKeys = collection.mutable.Map.empty[String,Int]
    val edges = collection.mutable.MutableList.empty[(String,String,String)]
    //val edges =
    var graph = DGraph.empty[Map[String,String], String]
    var nodeID = 0
    val allNodes = node \\ "Description"
    allNodes foreach(nd => {
      val id = nd.attribute(rdfNS,"ID").map(_.toString()).mkString
      nodeKeys.update(id, nodeID)
      val allChilds = nd \ "_"
      val textChilds = allChilds map(c => c.label -> c.text) filterNot(_._2 == "")
      val atrChilds = allChilds map(c => c.label -> c.attributes.map(a => a.key -> a.value.mkString)) filterNot(_._2.toList.isEmpty)
      graph = graph.addNode(Node(("ID" -> id :: textChilds.toList).toMap,nodeID))
      edges ++= atrChilds.map(x => (id ,x._1, x._2.head._2.substring(1))).toList
      nodeID += 1
    })
    for (elem <- edges) {
      graph = graph.addEdge(DEdge(elem._2, nodeKeys(elem._1), nodeKeys(elem._3))).get
    }
    graph
  }

}




