# nlputils
simple tools for NLP

In this project there will be simple tools for everyday NLP tasks

Anything could change in next release 

Using:
```scala
 resolvers += Resolver.sonatypeRepo("snapshots")

 libraryDependencies += "com.github.omidb" %%% "nlputils" % "0.1.0-SNAPSHOT"
```

It contains a very messy OntoNotes reader.
 
```scala
 val file = scala.io.Source.fromFile("E:/data/ontonotes-release-5.0/data/files/data/english/annotations/mz/sinorama/10/ectb_1001.onf").getLines().toIndexedSeq
 
 val ontdoc = OntoNotesReader.read(file)
```

You can also do any S-Expr parse:

```scala
val tree = "(TOP (S (VP (V kill)) (NP him)))"

val dg:DGraph[IndexedSeq[String],String] = SExpression.parse(tree)
```