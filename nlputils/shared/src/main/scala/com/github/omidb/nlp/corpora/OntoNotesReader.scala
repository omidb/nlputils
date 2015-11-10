package com.github.omidb.nlp.corpora

import com.github.omidb.nlp.formats._
import dgraph.DGraph


case class OntoNotesSentence(plainSentences:String,
                             treebankSentence:IndexedSeq[String],
                             tree:DGraph[IndexedSeq[String],String],
                             senses: Map[Int,String],
                             names: Map[Int, NameEntity],
                             preds: Map[Int,PredArg]
                            )
case class OntoNotesDocument(sections: List[List[OntoNotesSentence]], chains: List[List[CorefChain]])

object OntoNotesReader {

  def read(lines:IndexedSeq[String]) = {

    var index = 0
    val size = lines.size
    var plainSent = ""
    var tree = ""
    var leaves = IndexedSeq.empty[String]
    val sentences = scala.collection.mutable.MutableList.empty[OntoNotesSentence]
    val sections = scala.collection.mutable.MutableList.empty[List[OntoNotesSentence]]
    val chains = scala.collection.mutable.MutableList.empty[List[CorefChain]]
    while(index < size){
      val l = lines(index)
      if(l.startsWith("==========================================================================")){
        index += 3
        chains += processChain()
        sections += sentences.toList
        sentences.clear()
      }
      else if(l.startsWith("--------------------------------------------------------------------------")){
        index += 4
        plainSent = consume()
        plainSent = plainSent.split("\n").map(_.substring(4)).mkString("\n")
      }
      else if(l.startsWith("Treebanked sentence")){
        index += 2
        consume()
      }
      else if(l.startsWith("Tree")){
        index += 2
        tree = consume()
      }
      else if(l.startsWith("Leaves")){
        index += 2
        leaves = consume().split("\n")
        val leavs = processLeaves(leaves)
        sentences +=
          OntoNotesSentence(plainSent,leavs._5.toIndexedSeq,SExpression.parse(tree).get, leavs._1,leavs._2,leavs._4)
        index += 1
      }
      else if(l.startsWith("Speaker information:")){
        index += 6
      }

    }

    def consume() = {
      var str = ""
      while(lines(index) != ""){
        str += (lines(index) + "\n")
        index += 1
      }
      index += 1
      str
    }

    def processChain() = {

      var kind = ""
      var ref = ""
      val addressLists = scala.collection.mutable.MutableList.empty[(Int,TreeSpan)]
      val chains = scala.collection.mutable.MutableList.empty[CorefChain]
      while(index < lines.size && !lines(index).startsWith("---")){
        var ln = lines(index)
        while (index + 1 < lines.size && lines(index+1).startsWith("                          ")){
          ln += lines(index+1)
          index += 1
        }

        if(ln.startsWith("    Chain")){
          val sp = ln.substring(10).split(Array(' ', '('))
          ref = sp(0)
          kind = sp(2).substring(0, sp(2).size - 1)
        }
        else if(ln.startsWith("               ")){
         /// println(ln)
          val res = ln.substring(15).split(" ").filter(_.size > 0)
          val last = res.slice(1,res.size).mkString(" ")
          val spl = res(0).split(Array('.'))
          val spl2 = spl(1).split("-").map(_.toInt)
          addressLists += ((spl(0).toInt, TreeSpan(spl2(0), spl2(1), last)))
        } else if(lines(index) == ""){
          chains += CorefChain(kind, ref, addressLists.toList)
          addressLists.clear()
        }
        index += 1
      }
      chains.toList
  //    index += 2

    }

    def processLeaves(lves:IndexedSeq[String]) = {
      var i = 0
      var number = -1
      var value = ""
      val lss = lves.zipWithIndex.filter(_._1.startsWith("                               ")).map(_._2)
      val leavess = scala.collection.mutable.MutableList.empty[String]
      if(lss.size > 0) {
        normalizeCrazy(lss,lves)
      } else leavess ++= lves
      //
      val senses = scala.collection.mutable.Map.empty[Int,String]
      val names =  scala.collection.mutable.Map.empty[Int, NameEntity]
      val corefs =  scala.collection.mutable.Map.empty[Int, Coref]
      val preds = scala.collection.mutable.Map.empty[Int,PredArg]
      val words = scala.collection.mutable.MutableList.empty[String]

      //
      while(i < leavess.size){
        val l = leavess(i).substring(4)
        if(l(0).isDigit){
          val res = l.split(" ").filter(st => st != "" && st != " ")
          number = res(0).toInt
          value = res(1)
          words += value
          i += 1
        }
        else if(l.startsWith("       name:") || l.startsWith("    !  name:")){
          val isTreeAligned = l.startsWith("    !  name:")
          val res = l.substring(13).split(" ").filter(_.size > 0)
          val charPart = res(1).filter(x => x.isDigit || x == '-')
          val letterPart = res(1).filter(x => !x.isDigit &&  x != '-')

          val span = charPart.split("-").map(_.toInt)
          val last = letterPart + res.slice(2,res.size).mkString(" ")
          names.update(number, NameEntity(TreeSpan(span(0), span(1),last),res(0), isTreeAligned))
          i += 1
        }
        else if(l.startsWith("       coref:") || l.startsWith("    !  coref:")) {
          val isTreeAligned = l.startsWith("    !  coref:")
          val res = l.substring(14).split(" ").filter(_.size > 0)
          val spanPosTemp = res.indexWhere(e => e.contains("-") && e.split("-").filter(_.size > 0).forall(_.forall(_.isDigit)))
          val spanPos = if(spanPosTemp == -1) res.indexWhere(e => e.contains("-")) else spanPosTemp
          val knd = res.slice(0,spanPos - 1).mkString(" ")

          val charPart = res(spanPos).filter(x => x.isDigit || x == '-')
          val letterPart = res(spanPos).filter(x => !x.isDigit &&  x != '-')

          val span = charPart.split("-").map(_.toInt)
          val last = letterPart + res.slice(spanPos+1,res.size).mkString(" ")
          corefs.update(number, Coref(TreeSpan(span(0), span(1), res(spanPos - 1)),knd,last, isTreeAligned))
          i += 1
        }
        else if(l.startsWith("       prop:")){
          val res = l.substring(13).split("  ").filter(_.size > 0)
          i += 1
          val args = scala.collection.mutable.MutableList.empty[Argument]
          while(i < leavess.size && leavess(i).startsWith("            ")){
            val ll = leavess(i).substring(12)
            val ress = ll.split(" -> ")
            val arg = ress(0).split(" ").filter(c => c != "" && c != "*")
            val rest = ress.slice(1, ress.size)
            val chain = for(r <- rest) yield r.split(", ")
            val argLabel = if(arg.size > 0) arg(0) else ""
            args += Argument(argLabel, chain.map(ch => {
              val span = ch(0).split(":").map(_.toInt)
              TreePointer(span(0), span(1),ch(1))
            }).toList)
            i += 1
          }
          preds.update(number, PredArg(res(0), args.toList))

        }
        else if(l.startsWith("       sense:")){
          senses.update(number, l.substring(14))
          i += 1
        }
      }

      (senses.toMap, names.toMap, corefs.toMap, preds.toMap, words.toList)
    }

    def normalizeCrazy(lss:IndexedSeq[Int], lves:IndexedSeq[String]) = {
      val leavess = scala.collection.mutable.MutableList.empty[String]
      var lsi = 0
      while (lsi < lss.size) {
        if(lss.contains(lsi + 1) && lss.contains(lsi + 2) && lss.contains(lsi + 3) && lss.contains(lsi + 4)
          && lss.contains(lsi + 5)&& lss.contains(lsi + 6)&& lss.contains(lsi + 7)){
          leavess += (lves(lsi) + lves(lsi + 1) + lves(lsi + 2) +  lves(lsi + 3)  + lves(lsi + 4) +
            lves(lsi + 5) +  lves(lsi + 6)  + lves(lsi + 7))
          lsi += 8
        }
        else if(lss.contains(lsi + 1) && lss.contains(lsi + 2) && lss.contains(lsi + 3) && lss.contains(lsi + 4)
          && lss.contains(lsi + 5)&& lss.contains(lsi + 6)) {
          leavess += (lves(lsi) + lves(lsi + 1) + lves(lsi + 2) +  lves(lsi + 3)  + lves(lsi + 4) +
            lves(lsi + 5) +  lves(lsi + 6) )
          lsi += 7
        }
        else if(lss.contains(lsi + 1) && lss.contains(lsi + 2) && lss.contains(lsi + 3) && lss.contains(lsi + 4)
          && lss.contains(lsi + 5)) {
          leavess += (lves(lsi) + lves(lsi + 1) + lves(lsi + 2) +  lves(lsi + 3)  + lves(lsi + 4) +
            lves(lsi + 5)  )
          lsi += 6
        }
        else if(lss.contains(lsi + 1) && lss.contains(lsi + 2) && lss.contains(lsi + 3) && lss.contains(lsi + 4)){
          leavess += (lves(lsi) + lves(lsi + 1) + lves(lsi + 2) +  lves(lsi + 3)  + lves(lsi + 4))
          lsi += 5
        }
        else if(lss.contains(lsi + 1) && lss.contains(lsi + 2) && lss.contains(lsi + 3)){
          leavess += (lves(lsi) + lves(lsi + 1) + lves(lsi + 2) +  lves(lsi + 3) )
          lsi += 4
        }
        else if(lss.contains(lsi + 1) && lss.contains(lsi + 2)){
          leavess += (lves(lsi) + lves(lsi + 1) + lves(lsi + 2))
          lsi += 3
        }
        else if (lss.contains(lsi + 1)) {
          leavess += (lves(lsi) + lves(lsi + 1))
          lsi += 2
        }
        else {
          leavess += lves(lsi)
          lsi += 1
        }
      }
      leavess
    }

    OntoNotesDocument(sections.toList,chains.toList)
  }
}
