import java.util.Locale
import YorubaImplicits._

/**
 * Bias tells us which part of a word a WordProperty affects
 */
object Bias extends Enumeration {
  type Bias = Value
  val Left, Right, None = Value
}
import Bias._


/**
 * Word properties are tokens for word features (helps parsing)
 */
abstract class WordProperty { 
  def bias = Bias.None
}
case object Root extends WordProperty
case class Elision(where:Bias) extends WordProperty { override def bias = where}
case class Assimilation(where:Bias) extends WordProperty { override def bias = where}

/**
 * Yoruba contract. All Yoruba word objects must implement these features
 */
sealed trait Yoruba {
  def spelling:String
  def properties:Seq[WordProperty]
  def root: Yoruba
  def isRoot: Boolean
  def isElided:Boolean
  def isAssimilated:Boolean
  def toYoruba:String
  
  override def equals(o:Any) = o match {
    case that:Yoruba => that.toYoruba.equalsIgnoreCase(this.toYoruba)
    case that:String => that.equalsIgnoreCase(this.toYoruba) ||
    					that.equalsIgnoreCase(this.spelling)
    case _ => super.equals(o)
  }
  
  def hashcode = this.toYoruba.hashCode
  
  override def toString() = toYoruba
}

abstract class Expression extends Yoruba {
  var spelling:String = ""
  var properties:Seq[WordProperty] = List()
  
  def isRoot = (properties contains Root)
  def isElided = (properties contains Elision)
  def isAssimilated = (properties contains Assimilation)
  
  def processProperties(word:String=spelling, props:List[WordProperty]=Nil):String = {
    props match {
      case (p:Elision) :: tail => { 
        if (p.bias == Bias.Left) { processProperties(word.drop(1), tail) }
        else if (p.bias == Bias.Right) { processProperties(word.dropRight(1), tail) }
        else { processProperties(word, tail) }
      }
      case (p:Assimilation) :: tail => {
        if (p.bias == Bias.Left) { processProperties(word.take(1) + word, tail) }
        else if (p.bias == Bias.Right) { processProperties(word + word.takeRight(1), tail) }
        else { processProperties(word, tail) }
      }
      case _ => word
    }
  }
}

/**
 * Term is used for simple yoruba words which cannot be decomposed  
 *
 */
case class Term(word:String, props:WordProperty*) extends Expression {
  spelling = word
  properties = props
  
  def root = this
  def toYoruba = spelling
}

/**
 * Word is used for compound yoruba words made up of Terms or other compound Words
 */
case class Word(word:String, decomposition:List[Yoruba], props:WordProperty*) extends Expression {  
  spelling = word
  properties = props
  
  def root = decomposition.find(_.isRoot == true).getOrElse(this)
  override def isElided =  decomposition.exists(_.isElided)  || super.isElided
  override def isAssimilated = decomposition.exists(p => p.isAssimilated) || super.isAssimilated
  
  def toYoruba:String = decomposition map { 
    term => { processProperties(term.toYoruba, term.properties.toList) }
  } mkString ""
}

/**
 * Contract for meanings of Yoruba Expressions
 */
sealed trait Meaning {
  def description:String
  def language:Locale
  
  override def toString() = description
}

/**
 * A Yoruba translation i.e meaning in another language
 */
case class Translation(sense:String, lang:String="en") extends Meaning {
  var description = sense
  var language = new Locale(lang)  
}

/**
 * Convenience type for pairing a word with its attributes (used in dictionary)
 */
case class WordEntry(word:Word, attr:Tuple2[String,String]*) {
  var attributes = Map[String, String]() ++ attr
  
  def addAttributes(attrs:Tuple2[String,String]*) = for (a @ (k,v) <- attrs) {attributes += a}   
}

object YorubaImplicits {
  implicit def string2yoruba(str:String):Yoruba = Term(str)
}

object GrammarTest {
  def main(args:Array[String]) {    
    val word1 = Word("nigba", List("ní", Term("ìgbà", Elision(Left))))
    val word2 = Word("nigbati", List(word1, "tí"))
    val word3 = Word("kuule", List(Term("kú", Assimilation(Right)), Term("ilé", Elision(Left))))
        
    println(word1)
    println(word2)
    println(word3)
  }
}
