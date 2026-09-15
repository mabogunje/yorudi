/**
 * @author damola
 *
 */
package net.mabogunje.yorudi

import scala.util.parsing.combinator._
import scala.util.{Either, Left => EitherLeft, Right => EitherRight}
import Bias._
import YorubaImplicits._
import scala.io.Codec

case class DictionaryParseError(source:String, lineNumber:Int, line:String, message:String) {
  override def toString:String = {
    s"$source:$lineNumber: $message\n$line"
  }
}

class GrammarParser extends RegexParsers {
  /** The Grammar Parser uses a series of combinatorial parsers
   *  to build a single parser that can be used to parse certain
   *  formatted strings into Dictionary Word Definitions according
   *  to the classes defined in grammar.scala   
   */
  
  // Base token parsers - indicating word properties
  def root:Parser[SpeechProperty] = """\*""".r ^^^ {Root}
  def connector:Parser[SpeechProperty] = """~""".r ^^^ {Connector}
  
  def asmL:Parser[Assimilated] = """<(\+)+""".r ^^ { str => Assimilated(Left, str.count(_ == '+')) }
  def asmR:Parser[Assimilated] = """(\+)+>""".r ^^ { str => Assimilated(Right, str.count(_ == '+')) }
  def assimilation:Parser[Assimilated] = asmL | asmR
  
  def elsL:Parser[Elided] = """<(\-)+""".r ^^ { str => Elided(Left, str.count(_ == '-')) }
  def elsR:Parser[Elided] = """(\-)+>""".r ^^ { str => Elided(Right, str.count(_ == '-')) }
  def elision:Parser[Elided] = elsL | elsR

  def prefixes:Parser[SpeechProperty] = asmL|elsL
  def postfixes:Parser[SpeechProperty] = asmR|elsR|root
  def property:Parser[SpeechProperty] = root|assimilation|elision

  // Base parsers for all values - applies restrictions on acceptable strings
  def headword:Parser[String] = """[\p{L}(\p{Mn})?\\~?\-?]+[\p{L}(\p{Mn})?\-]*""".r ^^ {_.toLowerCase()}
  def term:Parser[String] = """[\p{L}(\p{Mn})?\\~?]+[\p{L}(\p{Mn})?]*""".r ^^ {_.toLowerCase()}
  def value:Parser[String] = """[\p{L}\p{Mn}\d_\(\)\-,\.:'’!]+""".r ^^ {_.toString()}

  // Base parser for word senses: Strings delimited by '/'. May be whole sentences 
  def sense:Parser[String] = "/" ~> rep(value) ^^ { _ mkString " " }

  // Base parser for attributes: User-defined key-value pairings separated by ':'
  def attribute:Parser[(String,String)] = value ~ (":" ~> value) ^^ { case k ~ v => (k -> v) }
  
  /**
   * Now we build the compound parsers which will produce our grammar objects
   */
  
  def word:Parser[Yoruba] = prefixes.* ~ term ~ postfixes.* ^^ {
    case plist1~term~plist2 => Term(term, (plist1 union plist2))
  }
  
  def decomposition:Parser[List[Yoruba]] = "[" ~> repsep(word, ".") <~ "]"
  
  def glossary:Parser[List[Meaning]] = rep(sense) ^^ { _ map (Translation(_)) }
  
  def attribs:Parser[List[(String,String)]] = "<" ~> repsep(attribute, "|") <~ ">"

  def wordEntry:Parser[(WordEntry, List[Meaning])] = headword ~ decomposition ~ glossary ~ attribs.? ^^ {
    case term ~ dcomp ~ gloss ~ attrs => {
      val entry = WordEntry(Word(term, dcomp), attrs.getOrElse(List()).toMap)
      ((entry -> gloss))
    }
  }
}

class FileParser extends GrammarParser {
  val CODEC = Codec.UTF8
  val COMMENT = "#";
  val DIRECTIVE = "!";
  var LANGUAGE = "";

  def dictionaryContent(line:String):String = {
    line.takeWhile(_ != COMMENT.head).trim
  }

  def isDictionaryEntry(line:String):Boolean = {
    val trimmed = dictionaryContent(line)
    trimmed.nonEmpty && !trimmed.startsWith(COMMENT) && !trimmed.startsWith(DIRECTIVE)
  }

  def parseDictionaryLine(source:String, lineNumber:Int, line:String):Either[DictionaryParseError, (WordEntry, List[Meaning])] = {
    parseAll(wordEntry, dictionaryContent(line)) match {
      case Success(result, _) => EitherRight(result)
      case NoSuccess(message, _) => EitherLeft(DictionaryParseError(source, lineNumber, line, message))
    }
  }

  def parseDictionaryLines(source:String, lines:IndexedSeq[String]):Either[List[DictionaryParseError], List[(Int, (WordEntry, List[Meaning]))]] = {
    val parsed = lines.zipWithIndex.collect {
      case (line, idx) if isDictionaryEntry(line) =>
        val lineNumber = idx + 1
        parseDictionaryLine(source, lineNumber, line).right.map(lineNumber -> _)
    }.toList

    val errors = parsed.collect { case EitherLeft(error) => error }
    if(errors.nonEmpty) EitherLeft(errors) else EitherRight(parsed.collect { case EitherRight(entry) => entry })
  }

  @deprecated("This method is not safe for files in JARs. Use indexFile instead.", "0.1")
  def index(filename: String): Map[String, Long] = {
    val file = getClass.getClassLoader.getResourceAsStream(filename)
    val lines = scala.io.Source.fromInputStream(file)(CODEC).getLines()
    var result = Map[String, Long]()
    var offset = 0L

    for (line <- lines) {
      val parsed = parse(wordEntry, line)

      if (parsed.successful) {
        val (entry, _) = parsed.get
        result += (entry.word.toYoruba -> offset)
      }
      offset += line.getBytes(CODEC.charSet).length + 1
    }
    file.close()
    result
  }

  def indexFile(filename: String): (Map[String, Int], IndexedSeq[String]) = {
    val fileStream = getClass.getClassLoader.getResourceAsStream(filename)
    try {
      val lines = scala.io.Source.fromInputStream(fileStream)(CODEC).getLines().toIndexedSeq
      parseDictionaryLines(filename, lines) match {
        case EitherRight(entries) => {
          val indexMap = entries.foldLeft(Map[String, Int]()) {
            case (index, (lineNumber, (entry, _))) => index + (entry.word.toYoruba -> (lineNumber - 1))
          }
          (indexMap, lines)
        }
        case EitherLeft(errors) => {
          val details = errors.map(_.toString).mkString("\n")
          throw new IllegalArgumentException("Dictionary contains invalid entries:\n" + details)
        }
      }
    } finally {
      if (fileStream != null) fileStream.close()
    }
  }

  def loadDictionary(filename:String):IndexedDictionary = {
    val fileStream = getClass.getClassLoader.getResourceAsStream(filename)
    try {
      val lines = scala.io.Source.fromInputStream(fileStream)(CODEC).getLines().toIndexedSeq
      parseDictionaryLines(filename, lines) match {
        case EitherRight(entries) => IndexedDictionary(entries.map(_._2).toIndexedSeq)
        case EitherLeft(errors) => {
          val details = errors.map(_.toString).mkString("\n")
          throw new IllegalArgumentException("Dictionary contains invalid entries:\n" + details)
        }
      }
    } finally {
      if (fileStream != null) fileStream.close()
    }
  }
}


object Test extends FileParser {
  def main(args:Array[String]) = {
    val (idx, lines) = indexFile("dicts/gpt.en.yor")
    val dict = IndexedDictionary(idx, lines)
    println(dict.lookup("ìwé"))
  }
}
