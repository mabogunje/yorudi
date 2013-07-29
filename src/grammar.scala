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
trait WordProperty { 
  def bias = Bias.None
}
case object Root extends WordProperty
case class Elided(override val bias:Bias) extends WordProperty
case class Assimilated(override val bias:Bias) extends WordProperty

/**
 * Yoruba contract. All Yoruba word objects must implement these features
 */
sealed trait Yoruba {
  def spelling:String
  def properties:Seq[WordProperty]
  def root: Yoruba

  def isRoot: Boolean = (properties contains Root)
  def isElided:Boolean = (properties contains Elided)
  def isAssimilated:Boolean = (properties contains Assimilated)
  
  def abbreviation(word:String=spelling, props:List[WordProperty]=properties.toList):String = {
    props match {
      case (p:Elided) :: tail => { 
        if (p.bias == Bias.Left) { abbreviation(word.drop(1), tail) }
        else if (p.bias == Bias.Right) { abbreviation(word.dropRight(1), tail) }
        else { abbreviation(word, tail) }
      }
      case (p:Assimilated) :: tail => {
        if (p.bias == Bias.Left) { abbreviation(word.take(1) + word, tail) }
        else if (p.bias == Bias.Right) { abbreviation(word + word.takeRight(1), tail) }
        else { abbreviation(word, tail) }
      }
      case _ => word
    }
  }
  
  def toYoruba:String
  def as(that:WordProperty):Yoruba 
  override def toString = toYoruba
}

/**
 * Term is used for simple yoruba words which cannot be decomposed  
 *
 */
case class Term(override val spelling:String, override val properties:Seq[WordProperty]=List()) extends Yoruba {  
  def root = this
  def toYoruba = spelling
  
  def as(that:WordProperty):Yoruba = this.copy(properties = this.properties :+ that)
}

/**
 * Word is used for compound yoruba words made up of Terms or other compound Words
 */
case class Word(override val spelling:String, decomposition:Seq[Yoruba], override val properties:Seq[WordProperty]=List()) extends Yoruba {    
  def root = decomposition.find(_.isRoot).getOrElse(this)
  override def isElided =  decomposition.exists(_.isElided)  || super.isElided
  override def isAssimilated = decomposition.exists(p => p.isAssimilated) || super.isAssimilated
  
  def toYoruba:String = decomposition map { w => { w.abbreviation() }} mkString ""
  
  def as(that:WordProperty):Yoruba = this.copy(properties = this.properties :+ that)
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
case class Translation(override val description:String, lang:String="en") extends Meaning {
  val language = new Locale(lang)  
}

/**
 * Convenience type for pairing a word with its attributes (used in dictionary)
 */
case class WordEntry(word:Word, attributes:Map[String,String])

object YorubaImplicits {
  implicit def string2yoruba(str:String):Yoruba = Term(str)
}

object GrammarTest {
  def main(args:Array[String]) {    
    val word1 = "dé"
    val word2 = Word("adé", List("a", word1 as Root))
    val word3 = Word("sade", List("ṣé" as Elided(Right), word2 as Root))
        
    println(word1)
    println(word2)
    println(word3)
  }
}