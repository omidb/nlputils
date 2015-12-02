import better.files.Cmds
import com.github.omidb.nlp.corpora.OntoNotesReader
import com.github.omidb.nlp.formats.SExpression
import dgraph.DGraph

object PlayGround extends App{
  import better.files._

  val ontonotePath = File("E:/data/ontonotes-release-5.0/data/files/data/english/annotations/bc/")
  val files = ontonotePath.listRecursively.filter(_.extension.getOrElse("") == ".onf").toList

//  val file = scala.io.Source.fromFile("E:/data/ontonotes-release-5.0/data/files/data/english/annotations/mz/sinorama/10/ectb_1001.onf").getLines().toIndexedSeq
//  val ontdoc = OntoNotesReader.read(file)
//  println(ontdoc)

  val mores = files.flatMap(f => OntoNotesReader.read(f.lines.toIndexedSeq).sections)
    .flatten.filter(_.treebankSentence.contains("more"))

  mores.foreach(s => println(s.tree))


}
