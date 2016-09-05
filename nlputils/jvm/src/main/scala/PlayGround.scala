import better.files.Cmds
import com.github.omidb.nlp.corpora.OntoNotesReader
import com.github.omidb.nlp.formats.SExpression
import com.github.omidb.nlp.toolsInterface.{TripsServers, TripsOnline}
import dgraph.DGraph

object PlayGround extends App{
  //import better.files._

//  val ontonotePath = File("E:/data/ontonotes-release-5.0/data/files/data/english/annotations/bc/")
//  val files = ontonotePath.listRecursively.filter(_.extension.getOrElse("") == ".onf").toList
//
////  val file = scala.io.Source.fromFile("E:/data/ontonotes-release-5.0/data/files/data/english/annotations/mz/sinorama/10/ectb_1001.onf").getLines().toIndexedSeq
////  val ontdoc = OntoNotesReader.read(file)
////  println(ontdoc)
//
//  val mores = files.flatMap(f => OntoNotesReader.read(f.lines.toIndexedSeq).sections)
//    .flatten.filter(_.treebankSentence.contains("more"))
//
//  mores.foreach(s => println(s.tree))

  val text = "I have an apple.It was good. it can be better and not not very delicious and funny what. "
  val onlineParser = new TripsOnline()
  //onlineParser.onlineParse(TripsServers.step, text).foreach(l => l.foreach(println(_)))
  onlineParser.fromFile("F:/OB_files_output/batch00000/coco_test-dev2015_question.txt-000000.xml").foreach(println)

  //val res0 = SExpression.parse2("(A (B B1) C1 (D D1) D").get
}
