package com.github.omidb.nlp.formats

import enumeratum._

sealed trait Penntree extends Product with Serializable
case class Constituent(value:String, tags:Seq[String]) extends Penntree
case class Leaf(pos:String, word:String, tags:Seq[String]) extends Penntree

object Penntree {
  val tgs = PTag.values.map(_.value)
  def parse(str:String) = {
    def convertor(nds:IndexedSeq[String]) = {
      if(nds.size == 0)
        Constituent("", IndexedSeq.empty)
      else if(nds.size == 1)
        Constituent(nds(0), IndexedSeq.empty)
      else
        Leaf(nds(0), nds(1), IndexedSeq.empty)
    }

    val p = SExpression.parse(str)
    p match {
      case Some(pp) => Some(pp.mapByNodes(convertor))
      case None => None
    }
  }


  sealed abstract class PLabel(
                              val value: String,
                              val typ: String,
                              val level: String,
                              val comment:String) extends EnumEntry{
   // override def entryName: String = value
    override def toString = value
  }


  object PLabel extends Enum[PLabel] {

    val values = findValues

    case object CC extends PLabel("CC", "article", "word", "Coordinating conjunction")

    case object CD extends PLabel("CD", "article", "word", "Cardinal number")

    case object DT extends PLabel("DT", "determiner", "word", "Determiner")

    case object EX extends PLabel("EX", "determiner", "word", "Existential there")

    case object FW extends PLabel("FW", "determiner", "word", "Foreign word")

    case object IN extends PLabel("IN", "determiner", "word", "Preposition or subordinating conjunction")

    case object JJ extends PLabel("JJ", "adjective", "word", "Adjective")

    case object JJR extends PLabel("JJR", "adjective", "word", "Adjective, comparative")

    case object JJS extends PLabel("JJS", "adjective", "word", "Adjective, superlative")

    case object LS extends PLabel("LS", "marker", "word", "List item marker")

    case object MD extends PLabel("MD", "modal", "word", "Modal")

    case object NN extends PLabel("NN", "noun", "word", "Noun, singular or mass")

    case object NNS extends PLabel("NNS", "noun", "word", "Noun, plural")

    case object NNP extends PLabel("NNP", "noun", "word", "Proper noun, singular")

    case object NNPS extends PLabel("NNPS", "noun", "word", "Proper noun, plural")

    case object PDT extends PLabel("PDT", "noun", "word", "Predeterminer")

    case object POS extends PLabel("POS", "noun", "word", "Possessive ending")

    case object PRP extends PLabel("PRP", "noun", "word", "Personal pronoun")

    case object PRP$ extends PLabel("PRP&", "noun", "word", "Possessive pronoun (prolog version PRP-S)")

    case object RB extends PLabel("RB", "adverb", "word", "Adverb")

    case object RBR extends PLabel("RBR", "adverb", "word", "Adverb, comparative")

    case object RBS extends PLabel("RBS", "adverb", "word", "Adverb, superlative")

    case object RP extends PLabel("RP", "particle", "word", "Particle")

    case object SYM extends PLabel("SYM", "symbol", "word", "Symbol")

    case object TO extends PLabel("TO", "particle", "word", "to")

    case object UH extends PLabel("UH", "particle", "word", "Interjection")

    case object VB extends PLabel("VB", "verb", "word", "Verb, base form")

    case object VBD extends PLabel("VBD", "verb", "word", "Verb, past tense")

    case object VBG extends PLabel("VBG", "verb", "word", "Verb, gerund or present participle")

    case object VBN extends PLabel("VBN", "verb", "word", "Verb, past participle")

    case object VBP extends PLabel("VBP", "verb", "word", "Verb, non-3rd person singular present")

    case object VBZ extends PLabel("VBZ", "verb", "word", "Verb, 3rd person singular present")

    case object WDT extends PLabel("WDT", "particle", "word", "Wh-determiner")

    case object WP extends PLabel("WP", "particle", "word", "Wh-pronoun")

    case object WP$ extends PLabel("WP$", "particle", "word", "Possessive wh-pronoun (prolog version WP-S)")

    case object WRB extends PLabel("WRB", "particle", "word", "Wh-adverb")

    ///

    case object POUND extends PLabel("#", "punctuation", "word", "Pound sign")

    case object DOLLAR extends PLabel("$", "punctuation", "word", "Dollar sign")

    case object DOT extends PLabel(".", "punctuation", "word", "Sentence-final punctuation")

    case object COMMA extends PLabel(",", "punctuation", "word", "Comma")

    case object COLON extends PLabel(":", "punctuation", "word", "Colon, semi-colon")

    case object LPARAN extends PLabel("(", "punctuation", "word", "Left bracket")

    case object RPARAN extends PLabel(")", "punctuation", "word", "right bracket")

    case object QUOTE extends PLabel("\"", "punctuation", "word", "straight double quote")

    case object LSQUOTE extends PLabel("`", "punctuation", "word", "left single quote")

    case object RSQUOTE extends PLabel("'", "punctuation", "word", "right single quote")

    ///
    case object S extends PLabel("S", "clause", "clause", "simple declarative clause, i.e. one that is not introduced by a (possible empty) subordinating conjunction or a wh-word and that does not exhibit subject-verb inversion.")

    case object SBAR extends PLabel("SBAR", "clause", "clause", "Clause introduced by a (possibly empty) subordinating conjunction.")

    case object SBARQ extends PLabel("SBARQ", "clause", "clause", "Direct question introduced by a wh-word or a wh-phrase. Indirect questions and relative clauses should be bracketed as SBAR, not SBARQ.")

    case object SINV extends PLabel("SBARQ", "clause", "clause", "Inverted declarative sentence, i.e. one in which the subject follows the tensed verb or modal.")

    case object SQ extends PLabel("SQ", "clause", "clause", "Inverted yes/no question, or main clause of a wh-question, following the wh-phrase in SBARQ.")

    ///
    case object ADJP extends PLabel("ADJP", "phrase", "phrase", "Adjective Phrase")

    case object CONJP extends PLabel("CONJP", "phrase", "phrase", "Conjunction Phrase")

    case object FRAG extends PLabel("FRAG", "phrase", "phrase", "Fragment")

    case object INTJ extends PLabel("INTJ", "phrase", "phrase", "Interjection. Corresponds approximately to the part-of-speech tag UH")

    case object LST extends PLabel("LST", "phrase", "phrase", "List marker. Includes surrounding punctuation")

    case object NAC extends PLabel("NAC", "phrase", "phrase", "Not a Constituent; used to show the scope of certain prenominal modifiers within an NP.")

    case object NP extends PLabel("NP", "phrase", "phrase", "Noun Phrase")

    case object NX extends PLabel("NX", "phrase", "phrase", "Used within certain complex NPs to mark the head of the NP. Corresponds very roughly to N-bar level but used quite differently.")

    case object PP extends PLabel("PP", "phrase", "phrase", "Prepositional Phrase")

    case object PRN extends PLabel("PRN", "phrase", "phrase", "Parenthetical")

    case object PRT extends PLabel("PRT", "phrase", "phrase", "Particle. Category for words that should be tagged RP")

    case object QP extends PLabel("QP", "phrase", "phrase", "Quantifier Phrase (i.e. complex measure/amount phrase); used within NP")

    case object RRC extends PLabel("PRN", "phrase", "phrase", "Reduced Relative Clause")

    case object UCP extends PLabel("UCP", "phrase", "phrase", "Unlike Coordinated Phrase")

    case object VP extends PLabel("VP", "phrase", "phrase", "Verb Phrase")

    case object WHADJP extends PLabel("WHADJP", "phrase", "phrase", "Wh-adjective Phrase. Adjectival phrase containing a wh-adverb, as in how hot")

    case object WHAVP extends PLabel("WHAVP", "phrase", "phrase", "Wh-adverb Phrase. Introduces a clause with an NP gap. May be null (containing the 0 complementizer) or lexical, containing a wh-adverb such as how or why.")

    case object WHNP extends PLabel("WHNP", "phrase", "phrase", "Wh-noun Phrase. Introduces a clause with an NP gap. May be null (containing the 0 complementizer) or lexical, containing some wh-word, e.g. who, which book, whose daughter, none of which, or how many leopards.")

    case object WHPP extends PLabel("WHPP", "phrase", "phrase", "Wh-prepositional Phrase. Prepositional phrase containing a wh-noun phrase (such as of which or by whose authority) that either introduces a PP gap or is contained by a WHNP.")

    case object X extends PLabel("X", "phrase", "phrase", "Unknown, uncertain, or unbracketable. X is often used for bracketing typos and in bracketing the...the-constructions.")

    ///
    case object STAR extends PLabel("*", "null", "null", "``Understood'' subject of infinitive or imperative")

    case object ZERO extends PLabel("0", "null", "null", "Zero variant of that in subordinate clauses")

    case object T extends PLabel("T", "null", "null", "Trace---marks position where moved wh-constituent is interpreted")

    case object NIL extends PLabel("NIL", "null", "null", "Marks position where preposition is interpreted in\n    pied-piping context")

  }

  sealed abstract class PTag(
                                val value: String,
                                val typ: String,
                                val comment:String) extends EnumEntry{
    // override def entryName: String = value
    override def toString = value
  }

  object PTag extends Enum[PLabel] {
    val values = findValues

    case object _ADV extends PTag("-ADV", "function discrepancies", "(adverbial) - marks a constituent other than ADVP or PP when it is used adverbially (e.g. NPs or free (\"headless\" relatives). However, constituents that themselves are modifying an ADVP generally do not get -ADV. If a more specific tag is available (for example, -TMP) then it is used alone and -ADV is implied. See the Adverbials section.")

    case object _NOM extends PTag("-NOM", "function discrepancies", "(nominal) - marks free (\"headless\") relatives and gerunds when they act nominally")

    ///
    case object _DTV extends PTag("-DTV", "grammatical role", "(dative) - marks the dative object in the unshifted form of the double object construction. If the preposition introducing the \"dative\" object is for, it is considered benefactive (-BNF). -DTV (and -BNF) is only used after verbs that can undergo dative shift.")

    case object _LGS extends PTag("-LGS", "grammatical role", "(logical subject) - is used to mark the logical subject in passives. It attaches to the NP object of by and not to the PP node itself.")

    case object _PRD extends PTag("-PRD", "grammatical role", "(predicate) - marks any predicate that is not VP. In the do so construction, the so is annotated as a predicate.")

    case object _PUT extends PTag("-PUT", "grammatical role", "marks the locative complement of put.")

    case object _SBJ extends PTag("-SBJ", "grammatical role", "(surface subject) - marks the structural surface subject of both matrix and embedded clauses, including those with null subjects.")

    case object _TCP extends PTag("-TCP", "grammatical role", "(\"topicalized\") - marks elements that appear before the subject in a declarative sentence, but in two cases only:\n    if the front element is associated with a *T* in the position of the gap.\n    if the fronted element is left-dislocated (i.e. it is associated with a resumptive pronoun in the position of the gap).\n ")

    case object _VOC extends PTag("-VOC", "grammatical role", "(vocative) - marks nouns of address, regardless of their position in the sentence. It is not coindexed to the subject and not get -TPC when it is sentence-initial.")

    ///
    case object _BNF extends PTag("-BNF", "adverbial", " (benefactive) - marks the beneficiary of an action (attaches to NP or PP). \n This tag is used only when (1) the verb can undergo dative shift and (2) the prepositional variant (with the same meaning) uses for. The prepositional objects of dative-shifting verbs with other prepositions than for (such as to or of) are annotated -DTV.")

    case object _DIR extends PTag("-DIR", "adverbial", " (direction) - marks adverbials that answer the questions \"from where?\" and \"to where?\" It implies motion, which can be metaphorical as in \"...rose 5 pts. to 57-1/2\" or \"increased 70% to 5.8 billion yen\" -DIR is most often used with verbs of motion/transit and financial verbs.")

    case object _EXT extends PTag("-EXT", "adverbial", " (extent) - marks adverbial phrases that describe the spatial extent of an activity. -EXT was incorporated primarily for cases of movement in financial space, but is also used in analogous situations elsewhere. Obligatory complements do not receive -EXT. Words such as fully and completely are absolutes and do not receive -EXT.")

    case object _LOC extends PTag("-LOC", "adverbial", " (locative) - marks adverbials that indicate place/setting of the event. -LOC may also indicate metaphorical location. There is likely to be some varation in the use of -LOC due to differing annotator interpretations. In cases where the annotator is faced with a choice between -LOC or -TMP, the default is -LOC. In cases involving SBAR, SBAR should not receive -LOC. -LOC has some uses that are not adverbial, such as with place names that are adjoined to other NPs and NAC-LOC premodifiers of NPs. The special tag -PUT is used for the locative argument of put.")

    case object _MNR extends PTag("-MNR", "adverbial", " (manner) - marks adverbials that indicate manner, including instrument phrases.")

    case object _PRP extends PTag("-PRP", "adverbial", " (purpose or reason) - marks purpose or reason clauses and PPs.")

    case object _TMP extends PTag("-TMP", "adverbial", " (temporal) - marks temporal or aspectual adverbials that answer the questions when, how often, or how long. It has some uses that are not strictly adverbial, auch as with dates that modify other NPs at S- or VP-level. In cases of apposition involving SBAR, the SBAR should not be labeled -TMP. Only in \"financialspeak,\" and only when the dominating PP is a PP-DIR, may temporal modifiers be put at PP object level. Note that -TMP is not used in possessive phrases.")

    ///
    case object _CLR extends PTag("-CLR", "Miscellaneous", "(closely related) - marks constituents that occupy some middle ground between arguments and adjunct of the verb phrase. These roughly correspond to \"predication adjuncts\", prepositional ditransitives, and some \"phrasel verbs\". Although constituents marked with -CLR are not strictly speaking complements, they are treated as complements whenever it makes a bracketing difference. The precise meaning of -CLR depends somewhat on the category of the phrase.on S or SBAR - These categories are usually arguments, so the -CLR tag indicates that the clause is more adverbial than normal clausal arguments. The most common case is the infinitival semi-complement of use, but there are a variety of other cases.\n    on PP, ADVP, SBAR-PRP, etc - On categories that are ordinarily interpreted as (adjunct) adverbials, -CLR indicates a somewhat closer relationship to the verb. For example:\n      Prepositional Ditransitives\n        In order to ensure consistency, the Treebank recognizes only a limited class of verbs that take more than one complement (-DTV and -PUT and Small Clauses) Verbs that fall outside these classes (including most of the prepositional ditransitive verbs in class [D2]) are often associated with -CLR.\n      Phrasal verbs\n      Phrasal verbs are also annotated with -CLR or a combination of -PRT and PP-CLR. Words that are considered borderline between particle and adverb are often bracketed with ADVP-CLR.\n      Predication Adjuncts\n      Many of Quirk's predication adjuncts are annotated with -CLR.\n      on NP - To the extent that -CLR is used on NPs, it indicates that the NP is part of some kind of \"fixed phrase\" or expression, such as take care of. Variation is more likely for NPs than for other uses of -CLR.")

    case object _CLF extends PTag("-CLF", "Miscellaneous", "(cleft) - marks it-clefts (\"true clefts\") and may be added to the labels S, SINV, or SQ.")

    case object _HLN extends PTag("-HLN", "Miscellaneous", "(headline) - marks headlines and datelines. Note that headlines and datelines always constitute a unit of text that is structurally independent from the following sentence.")

    case object _TTL extends PTag("-TTL", "Miscellaneous", "(title) - is attached to the top node of a title when this title appears inside running text. -TTL implies -NOM. The internal structure of the title is bracketed as usual.")

  }



}
