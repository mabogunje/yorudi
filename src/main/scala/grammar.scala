/**
 * @author damola
 *
 */
package net.mabogunje.yorudi

import java.util.Locale
import Bias._
import Tone._
import YorubaImplicits._

/**
 * Yoruba interface. All Yoruba word objects must implement these features
 */
sealed trait Yoruba {
  val complexLetters = List("ẹ́", "ẹ̀", "ọ́", "ọ̀")
  
  def spelling:String
  def properties:Seq[SpeechProperty]
  def root: Yoruba
  
  def isRoot: Boolean = properties contains Root  
  def isElided:Boolean
  def isAssimilated:Boolean
  def isComplexLetter(letter:Char) = complexLetters.contains(letter)


  def elisions(bias:Bias) = properties.find(_==Elided(bias))
  def assimilations(bias:Bias) = properties.find(_==Assimilated(bias))  
  
  def assimilate(that:Yoruba, p:SpeechProperty=Assimilated(Right, 2)):Yoruba = {
    var left = this.abbreviated.spelling.dropRight(1) 
    var mid = ""; 
    var right = that.abbreviated.spelling.drop(1)
    
    p.bias match {
      case Left => {
        mid += Tone.as(Tone.get(this.abbreviated.spelling.last), that.abbreviated.spelling.head).toString
        
        if (p.count > 2) 
          mid += that.abbreviated.spelling.head.toString               
      }
      case Right => {
        mid = Tone.as(Tone.get(that.abbreviated.spelling.head), this.abbreviated.spelling.last).toString
        
        if (p.count > 2)
          left = this.abbreviated.spelling
       }
      }
    
    val newSpelling = left + mid + right
    val newProperties = this.properties.filterNot(_ == p) ++ that.properties.filterNot(_ == p.opposite)
    Term(newSpelling, newProperties)
  }

  def +(that:Yoruba):Yoruba = {
    Term(this.toYoruba + that.toYoruba)
  }
  def as(that:SpeechProperty):Yoruba
  
  def abbreviated:Yoruba
  def toYoruba:String
  override def toString = toYoruba
}

/**
 * Term is used for simple yoruba words which cannot be decomposed  
 *
 */
case class Term(override val spelling:String, override val properties:Seq[SpeechProperty]=List()) extends Yoruba {  
  def root = this

  def isElided = (properties contains Elided(Left)) || (properties contains Elided(Right))
  def isAssimilated = (properties contains Assimilated(Left)) || (properties contains Assimilated(Right))
  
  def abbreviated:Yoruba = {
    val newSpelling = properties.foldLeft(spelling)((str, property) =>
      property match {
        case (p:Elided) => p.bias match {
          case Left => if (isComplexLetter(str.head)) str.drop(p.count + 1) else str.drop(p.count)
          case Right => if(isComplexLetter(str.last)) str.dropRight(p.count + 1) else str.dropRight(p.count)
          case _ => str
        }
        case _ => str
      })
      
      Term(newSpelling, properties.filterNot(_.isInstanceOf[Elided]))
  }
  
  def as(that:SpeechProperty):Yoruba = this.copy(properties = this.properties :+ that)
  
  def toYoruba:String = spelling
}

/**
 * Word is used for compound yoruba words made up of Terms or other compound Words
 */
case class Word(override val spelling:String, decomposition:Seq[Yoruba], override val properties:Seq[SpeechProperty]=List()) extends Yoruba {    
  def root = decomposition.find(_.isRoot).getOrElse(spelling)
  
  def isElided = decomposition forall { word => (word.elisions(Left).isDefined && 
    								   			word.elisions(Right).isDefined)
    						 }
  
  def isAssimilated = decomposition exists {word => (word.assimilations(Left).isDefined || 
    								   				 word.assimilations(Right).isDefined)
    						 }
  def equals(that:Word) = this.decomposition.equals(that.decomposition)
  def contraction = {
    if (!isAssimilated)
      decomposition map { word => word.abbreviated }
    else
    {
      val wordPairs = decomposition.iterator.sliding(2).toList 
      wordPairs map { pair =>
        pair match {
          case left :: right => { 
            if (right.headOption.isEmpty)
              left 
            else 
            {
              val removeLeft = left.assimilations(Left).isDefined
              val removeRight = right.head.assimilations(Right).isDefined
              val assimilateToLeft = left.assimilations(Right).isDefined && right.head.assimilations(Left).isDefined
              
              if (removeLeft && removeRight) 
                ""
              else if(removeLeft) 
                right.head.abbreviated
              else if(assimilateToLeft)
              {
                val fromRight = (right.head.assimilations(Left).get.count > left.assimilations(Right).get.count)
                
                fromRight match {
                  case true => left.assimilate(right.head, right.head.assimilations(Left).get)
                  case false => left.assimilate(right.head, left.assimilations(Right).get)
                  }
              }
              else
                left.abbreviated
            }
          }
          case Nil => List(this)
        }}}
  }

  def abbreviated:Yoruba = {    
    val newSpelling = contraction mkString ""       
    Word(newSpelling, decomposition)
  }
      
  def as(that:SpeechProperty):Yoruba = this.copy(properties = this.properties :+ that)
  def toYoruba = this.abbreviated.spelling
}

/**
 * Contract for meanings of Yoruba Expressions
 */
sealed trait Meaning {
  def description:String
  def language:String
  
  override def toString() = description
}

/**
 * A Yoruba translation i.e meaning in another language
 */
case class Translation(override val description:String, override val language:String = "en-NG") extends Meaning
{
  def locale = Locale.forLanguageTag(language);
}

/**
 * Convenience type for pairing a word with its attributes (used in dictionary)
 */
case class WordEntry(word:Word, attributes:Map[String,String])
{
  override def toString = word.toString + " (" + (word.decomposition mkString " + ") + ")"
}

object YorubaImplicits {
  implicit def string2yoruba(str:String):Yoruba = Term(str)
}
