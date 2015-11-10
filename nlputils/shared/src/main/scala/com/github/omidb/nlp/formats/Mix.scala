package com.github.omidb.nlp.formats


case class NameEntity(pointer:TreeSpan, kind:String, isAlignedByTree:Boolean = true)
case class Coref(pointer:TreeSpan, kind:String, ref:String, isAlignedByTree:Boolean = true)
case class CorefChain(kind:String, value:String, chain:List[(Int, TreeSpan)])

case class PredArg(value:String, args:List[Argument])
case class Argument(arg:String, pointers: List[TreePointer])
case class TreePointer(word:Int, height:Int, value:String)
case class TreeSpan(from:Int, to:Int, value:String)

