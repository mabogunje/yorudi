import java.util.Locale

/**
 * Bias tells us which part of a word a WordProperty affects
 */

object Bias extends Enumeration {
  type Bias = Value
  val Left, Right, None = Value
}
import Bias._


/**
 * Word properties serve as markers for essential word features (helps parsing)
 */
abstract class WordProperty { 
  def bias = Bias.None
  override def toString = bias.toString()
}
case object Root extends WordProperty
case class Elision/*[T <: Bias]*/(where:Bias) extends WordProperty { override def bias = where}
case class Assimilation/*[T <: Bias]*/(where:Bias) extends WordProperty { override def bias = where}


/**
 * The prerequisites of a word are to have a root, a spelling, and checks for WordProperties
 */
sealed trait Vocable {
  def spelling:String
  def traits:Seq[WordProperty]
  def root: Vocable
  def isRoot: Boolean
  def isElided:Boolean
  def isAssimilated:Boolean
  
  //override def toString() = spelling
}

/**
 * Base class for words
 */
abstract class Expression extends Vocable {
  var spelling:String = ""
  var traits:Seq[WordProperty] = List()
  
  def isRoot = (traits contains Root)
  def isElided = (traits contains Elision)
  def isAssimilated = (traits contains Assimilation)
}

/**
 * Represents the smallest meaningful unit of language 
 * e.g prefixes such as the 'ˆ' in 'ˆdŽ'
 */
case class Term(word:String, properties:WordProperty*) extends Expression {
  spelling = word
  traits = properties
  
  def root = Term.this
}

/**
 * Can be used for any type of word, but especially compound words 
 * e.g word:n’gbat’, morphology:[n’, “gb‡, t’], properties:Elision(Left) 
 */
case class Word(word:String, morphology:List[Vocable], properties:WordProperty*) extends Expression {  
  spelling = word
  traits = properties
  
  def root = morphology.find(_.isRoot == true).getOrElse(this)
  override def isElided =  morphology.exists(_.isElided)  || super.isElided
  override def isAssimilated = morphology.exists(p => p.isAssimilated) || super.isAssimilated
}

/**
 * A unit of meaning carried by a Vocable
 */
sealed trait Meaning {
  def description:String
  def language:Locale
  
  override def toString() = description
}

/**
 * Represents a word meaning/definition in a specified language
 */
case class Translation(sense:String, lang:String="en") extends Meaning {
  var description = sense
  var language = new Locale(lang)  
}


sealed trait Entry {
  //def ref:String
  def spelling:String
  def inYoruba(composition:List[Vocable]):String
  //def senses(lang:String):List[Sememe]
  def attributes:Map[String, String]
  
  def addAttributes(attrs:Tuple2[String,String]*)
}

/**
 * Convenience type for pairing a word with its attributes
 * Also provides the method inYoruba for composing a Yoruba 
 * spelling from word morphology
 */
case class WordEntry(word:Word, attr:Tuple2[String,String]*) extends Entry with Vocable {
  var spelling = word.spelling
  var root = word.root
  var traits = word.traits
  var attributes = Map[String, String]() ++ attr
  
  // Implementing Pheme
  def isElided = word.isElided
  def isRoot = word.isRoot
  def isAssimilated = word.isAssimilated

  // Implementing Entry
  def inYoruba(composition:List[Vocable] = word.morphology):String = composition map { 
    case w => { w.traits match {
      case (p:Elision) => { 
        if (p.bias == Bias.Left) { w.spelling.drop(1) }
        else if (p.bias == Bias.Right) { w.spelling.dropRight(1) }
        else { w.spelling }
      } 
      case (p:Assimilation) => { 
        if (p.bias == Bias.Left) { w.spelling.head + w.spelling }
        else if (p.bias == Bias.Right) { w.spelling + w.spelling.last }
        else { w.spelling }
      }
      case _ => w.spelling
    }}
  } mkString " " // BROKEN

//def senses(lang:String="en") = senses.takeWhile(_.language.getLanguage == lang)
  def addAttributes(attrs:Tuple2[String,String]*) = for (a @ (k,v) <- attrs) {attributes += a} 
  
  override def toString() = this.inYoruba()
}

object GrammarTest {
  def main(args:Array[String]) {
    val entry = WordEntry(Word("nigbati", List(Term("n’"), Term("“gbˆ", Elision(Left), Root), Term("t’"))))
    println(entry)
  }
}
