import java.util.Locale
import Bias._
import Tone._
import YorubaImplicits._

/**
 * Yoruba interface. All Yoruba word objects must implement these features
 */
sealed trait Yoruba {
  def spelling:String
  def abbreviation:String
  def contraction:String
  def properties:Seq[SpeechProperty]
  def root: Yoruba
  

  def isRoot: Boolean = (properties contains Root)
  
  def isElidedLeft = properties contains Elided(Left)
  def isElidedRight = properties contains Elided(Right)
  def isElided:Boolean = (isElidedLeft || isElidedRight)
  
  def isAssimilatedLeft = properties contains Assimilated(Left)
  def isAssimilatedRight = properties contains Assimilated(Right)
  def isAssimilated:Boolean = (isAssimilatedLeft || isAssimilatedRight)
  
  def getAssimilation = properties.find(p => ((p == Assimilated(Left)) || (p == Assimilated(Right)))).get
    
  def assimilate(that:Yoruba, p:SpeechProperty=Assimilated(Right)):String = {
    var modChar = ' '
      
    p.bias match {
      case Left => { modChar = Tone.as(Tone.get(this.abbreviation.last), that.abbreviation.head)
        if (p.count > 1) {
          this.abbreviation.dropRight(1) + modChar + that.abbreviation.head + that.abbreviation.drop(1)
        }
        else this.abbreviation.dropRight(1) + modChar + that.abbreviation.drop(1)
      }
      case Right => { modChar = Tone.as(Tone.get(that.abbreviation.head), this.abbreviation.last) 
        if (p.count > 1) {
          this.abbreviation + modChar + that.abbreviation.drop(1)
        }
        else this.abbreviation.dropRight(1) + modChar + that.abbreviation.drop(1)
      }
    }    
  }
  
  def toYoruba:String = { if (isAssimilated) contraction else abbreviation } 
  def as(that:SpeechProperty):Yoruba
  override def toString = toYoruba
}

/**
 * Term is used for simple yoruba words which cannot be decomposed  
 *
 */
case class Term(override val spelling:String, override val properties:Seq[SpeechProperty]=List()) extends Yoruba {  
  def root = this
  
  def abbreviation:String = {
    properties match {
      case (p:Elided) :: tail => { 
        if (p.bias == Bias.Left) 
          spelling.drop(p.count)
        else if (p.bias == Bias.Right) 
          spelling.dropRight(p.count)
        else 
          spelling
      }
      case _ => spelling
    }
  }

  override def contraction:String = spelling 
  
  def as(that:SpeechProperty):Yoruba = this.copy(properties = this.properties :+ that)
}

/**
 * Word is used for compound yoruba words made up of Terms or other compound Words
 */
case class Word(override val spelling:String, decomposition:Seq[Yoruba], override val properties:Seq[SpeechProperty]=List()) extends Yoruba {    
  def root = decomposition.find(_.isRoot).getOrElse(this)
  override def isElided =  decomposition.exists(_.isElided)  || super.isElided
  override def isAssimilated = decomposition.exists(p => p.isAssimilated) || super.isAssimilated
  
  def abbreviation:String = decomposition map { w => w.abbreviation } mkString "" 
  def contraction:String = {
    decomposition.iterator.sliding(2).toList map { case left :: right => {
      if (left.isAssimilatedLeft && right.head.isAssimilatedRight)
        ""
      else if (left.isAssimilatedLeft && !right.head.isAssimilated)
        right.head.abbreviation
      else if (left.isAssimilatedRight) {
        if (right.head.isAssimilated) {
          if (right.head.getAssimilation.count > left.getAssimilation.count)
            left.assimilate(right.head, right.head.getAssimilation)
          else
            left.assimilate(right.head, left.getAssimilation)
        }
      }
      else if (right.head.isAssimilatedLeft) 
        left.assimilate(right.head, right.head.getAssimilation)
      else
        left.abbreviation
    }} mkString ""
  }
  
  def as(that:SpeechProperty):Yoruba = this.copy(properties = this.properties :+ that)
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
    val word1 = Word("de", List("dé" as Root))
    val word2 = Word("ade", List("à", word1 as Root))
    val word3 = Word("sade", List("ṣé" as Elided(Right), word2 as Root))
    val word4 = Word("kaabo", List("kú" as Assimilated(Right), "àbò" as Assimilated(Left, 2)))
    val word5 = Word("kuule", List("kú" as Assimilated(Right, 2), "ilé" as Assimilated(Left)))    
    val word6 = Word("abanisise", List("a", "bá" as Assimilated(Right), "eni" as Assimilated(Left) as Root, "ṣiṣẹ"))
    val word7 = Word("abanigbele", List("a", "bá", "ni" as Root, "gbé" as Assimilated(Right), "íle" as Assimilated(Left)))
    
    val test = List(word1, word2, word3, word4, word5, word6, word7)
    
    for (word <- test) println(word.toYoruba)
  }
}