package com.github.omidb.nlp.toolsInterface

import dgraph.DGraph

trait TripsOnlineParser {
  def parse(serverAddress:String, text:String):Option[List[TripsLF]]
}

object TripsServers {
  val ihmc = "http://trips.ihmc.us/parser/cgi/parse"
  val step = "http://trips.ihmc.us/parser/cgi/step"
  val ihmcLocal = "http://trips.ihmc.us/parser/cgi/parse"
  val rochester = "http://www.cs.rochester.edu/research/cisd/projects/trips/parser/cgi/web-parser-xml.cgi"
}

case class TripsLF(graph:DGraph[Map[String,String],String], rootNode:Option[Int])

class TripsInterface {

}
