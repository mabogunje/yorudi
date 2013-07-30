import java.util.Locale
import YorubaImplicits._

/**
 * Bias tells us which part of a word a WordProperty affects
 */
object Bias extends Enumeration {
  type Bias = Value
  val Left, Right, Neutral = Value
}
import Bias._

/**
 * Tone tells us which accent a character possesses
 */
object Tone extends Enumeration {
  type Tone = Value
  val Mid, Low, High = Value //ordered by precedence
  
  val characterMap = Map( 'a' -> Map(Tone.Mid -> 'a', Tone.Low -> 'à', Tone.High -> 'á'),
		  			   	  'e' -> Map(Tone.Mid -> 'e', Tone.Low -> 'è', Tone.High -> 'é'),
		  			   	  'i' -> Map(Tone.Mid -> 'i', Tone.Low -> 'ì', Tone.High -> 'í'),
		  			   	  'o' -> Map(Tone.Mid -> 'o', Tone.Low -> 'ò', Tone.High -> 'ó'),
		  			   	  'u' -> Map(Tone.Mid -> 'u', Tone.Low -> 'ù', Tone.High -> 'ú')		  			   
		  			 	)
  val allowed = characterMap flatten (_._2) map (l => l._2) toList
		  			 
  def normalise(char:Char):Char = {
    if (characterMap.get('a').exists(tone => tone.values.toList contains char)) 'a'
    else if (characterMap.get('e').exists(tone => tone.values.toList contains char)) 'e'
    else if (characterMap.get('i').exists(tone => tone.values.toList contains char)) 'i'
    else if (characterMap.get('o').exists(tone => tone.values.toList contains char)) 'o'
    else if (characterMap.get('u').exists(tone => tone.values.toList contains char)) 'u'
    else char
  }
  
  def as(tone:Tone, char:Char):Char = {
    if (!(allowed contains char)) ""
    characterMap.get(normalise(char)).get(tone)    
  }
  
  def get(char:Char):Tone = {
    if (!(allowed contains char)) ""    
    characterMap.get(normalise(char)).map(e => e map (_.swap)).get(char)
  }
}
import Tone._

/**
 * Word properties are tokens for word features (helps parsing)
 */
trait WordProperty { 
  def bias = Bias.Neutral
  def count = 1
  
  override def equals(o:Any):Boolean = { o match {
    case o:WordProperty => (o.bias == bias)
    case _ => false
    }
  }
}
case object Root extends WordProperty
case class Elided(override val bias:Bias, override val count:Int=1) extends WordProperty
case class Assimilated(override val bias:Bias, override val count:Int=1) extends WordProperty

/**
 * Yoruba contract. All Yoruba word objects must implement these features
 */
sealed trait Yoruba {
  def spelling:String
  def properties:Seq[WordProperty]
  def root: Yoruba

  def isRoot: Boolean = (properties contains Root)
  
  def isElidedLeft = properties contains Elided(Left)
  def isElidedRight = properties contains Elided(Right)
  def isElided:Boolean = (isElidedLeft || isElidedRight)
  
  def isAssimilatedLeft = properties contains Assimilated(Left)
  def isAssimilatedRight = properties contains Assimilated(Right)
  def isAssimilated:Boolean = (isAssimilatedLeft || isAssimilatedRight)
  
  def getAssimilation = properties.find(p => ((p == Assimilated(Left)) || (p == Assimilated(Right)))).get
  
  def abbreviation(word:String=spelling, props:List[WordProperty]=properties.toList):String = {
    props match {
      case (p:Elided) :: tail => { 
        if (p.bias == Bias.Left) { abbreviation(word.drop(p.count), tail) }
        else if (p.bias == Bias.Right) { abbreviation(word.dropRight(p.count), tail) }
        else { abbreviation(word, tail) }
      }
      case _ => word
    }
  }
  
  def assimilate(that:Yoruba, p:WordProperty=Assimilated(Right)):String = {
    var modChar = ' '
      
    p.bias match {
      case Left => { modChar = Tone.as(Tone.get(this.abbreviation().last), that.abbreviation().head)
        if (p.count > 1) {
          this.abbreviation().dropRight(1) + modChar + that.abbreviation().head + that.abbreviation().drop(1)
        }
        else this.abbreviation().dropRight(1) + modChar + that.abbreviation().drop(1)
      }
      case Right => { modChar = Tone.as(Tone.get(that.abbreviation().head), this.abbreviation().last) 
        if (p.count > 1) {
          this.abbreviation() + modChar + that.abbreviation().drop(1)
        }
        else this.abbreviation().dropRight(1) + modChar + that.abbreviation().drop(1)
      }
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
 
  def as(that:WordProperty):Yoruba = this.copy(properties = this.properties :+ that)
  def toYoruba:String = decomposition map { w => w.abbreviation() } mkString ""
  
  def test = {
    val indexed = decomposition.zipWithIndex.iterator.sliding(2).toList
    val contracted = indexed map { case left :: right => { 
      if (left._1.isAssimilatedRight) 
        (left._1.assimilate(right.head._1, left._1.getAssimilation), left._2)
      else if (right.head._1.isAssimilatedLeft) 
        (left._1.assimilate(right.head._1, right.head._1.getAssimilation), left._2)
      else
        left
    }}
    contracted map (pair => pair._1) mkString ""
        
      /*val arr = indexed.toArray
    val assimilations = indexed.filter(i => i._1.isAssimilated)
    for (asm <- assimilations) yield ( indexed.takeWhile(item => item._2 < asm._2),  
    								   asm._1.assimilate(arr(asm._2+1)._1, asm._1.getAssimilation),
    								   indexed.slice(asm._2, indexed.last._2+1)
    								 )*/
  }
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
    val word2 = Word("ade", List("à", word1 as Root))
    val word3 = Word("sade", List("ṣé" as Elided(Right), word2 as Root))
    val word4 = Word("kuule", List("kú" as Assimilated(Right, 2), "ilé"))
    val word5 = Word("abanisise", List("à", "bá" as Assimilated(Right), "eni" as Root, "ṣiṣẹ"))
    val word6 = Word("abanidije", List("a", "bá", "ni" as Root, "gbé" as Assimilated(Right), "íle"))
        
    println(word1, word2, word3)
    println(word6.test)
  }
}