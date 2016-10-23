package com.github.omidb.nlp.toolsInterface

import dgraph.DGraph

trait TripsOnlineParser {
  def onlineParse(serverAddress:String, text:String):TripsDoc
}

object TripsServers {
  val ihmc = "http://trips.ihmc.us/parser/cgi/parse"
  val step = "http://trips.ihmc.us/parser/cgi/step"
  val stepDev = "http://trips.ihmc.us/parser/cgi/step-dev"

  val drum = "http://trips.ihmc.us/parser/cgi/drum"
  val drumDev = "http://trips.ihmc.us/parser/cgi/drum-dev"


  val ihmcLocal = "http://trips.ihmc.us/parser/cgi/parse"
  val rochester = "http://www.cs.rochester.edu/research/cisd/projects/trips/parser/cgi/web-parser-xml.cgi"
}

case class TripsLF(graph:DGraph[Map[String,String],String], rootNode:Option[Int])

case class TripsDoc(lfs:Seq[TripsLF])

class TripsInterface {

}
