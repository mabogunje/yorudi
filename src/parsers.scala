import scala.util.parsing.combinator._
import Bias._


class GrammarParser extends RegexParsers {
  /** The Grammar Parser uses a series of combinatorial parsers
   *  to build a single parser that can be used to parse certain
   *  formatted strings into Dictionary Word Definitions according
   *  to the classes defined in grammar.scala   
   */
  
  // Base token parsers - indicating word properties
  def root:Parser[WordProperty] = """\*""".r ^^^ {Root}
  
  def asmL:Parser[Assimilation] = """<\+""".r ^^^ {Assimilation(Left)}
  def asmR:Parser[Assimilation] = """\+>""".r ^^^ {Assimilation(Right)}
  def assimilation:Parser[Assimilation] = asmL | asmR
  
  def elsL:Parser[Elision] = """<\-""".r ^^^ {Elision(Left)}
  def elsR:Parser[Elision] = """\->""".r ^^^ {Elision(Right)}
  def elision:Parser[Elision] = elsL | elsR

  def property:Parser[WordProperty] = root|assimilation|elision 

  // Base parsers for all values - applies restrictions on acceptable strings
  def term:Parser[String] = """\p{L}+""".r ^^ {_.toLowerCase()}
  def value:Parser[String] = """[\w|\(\)]+""".r ^^ {_.toString()}

  // Base parser for word senses: Strings delimited by '/'. May be whole sentences 
  def sense:Parser[String] = "/" ~ rep(value) ^^ { case "/" ~ list => list mkString " " }

  // Base parser for attributes: User-defined key-value pairings separated by ':'
  def attribute:Parser[Tuple2[String,String]] = value ~ ":" ~ value ^^ { 
    case k~":"~v => (k -> v)
  }
  
  /**
   * Now we build the compound parsers which will produce our grammar objects
   */
  
  def word:Parser[Expression] = property.* ~ term ~ property.* ^^ {
    case plist1~term~plist2 => Term(term, (plist1 union plist2).distinct:_*)
  }
  
  def decomposition:Parser[List[Expression]] = "[" ~repsep(word, ".")~ "]" ^^ {
    case "[" ~ list ~ "]" => {list}
  }
  
  def glossary:Parser[List[Meaning]] = rep(sense) ^^ { _ map (Translation(_)) }
  
  def attribs:Parser[List[Tuple2[String,String]]] = "<"~ repsep(attribute, "|") ~">" ^^ {
    case "<" ~ list ~ ">" => { list }    
  }

  def wordEntry:Parser[(WordEntry, List[Meaning])] = term ~ decomposition ~ glossary ~ attribs.? ^^ {
    case term ~ dcomp ~ gloss ~ attrs => {
      var entry = WordEntry(Word(term, dcomp))
      for (a <- attrs) { a.map(entry.addAttributes(_)) }
      (entry -> gloss)
    }
  }
}

object ParserTest extends GrammarParser {
  def main(args:Array[String]) {
    val testEntry1 = "nigbati [ní . <-ìgbà* . tí]  /when (adv) /at the time <fr:256 | qr:90>"
    val testEntry2 = "kuule [kú+>* . <-ilé]  /Good evening"
    val testEntry3 = "ade [à . dé]  /crown"
    val objs = parseAll(wordEntry, testEntry3)
      println(objs)
  }
}
