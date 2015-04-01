
/**
 * Tone provides methods for interacting with accented characters
 */
object Tone extends Enumeration {
  type Tone = Value
  val Mid, Low, High = Value
  
  val characterMap = Map( 'a' -> Map(Tone.Mid -> 'a', Tone.Low -> 'à', Tone.High -> 'á'),
		  			   	  'e' -> Map(Tone.Mid -> 'e', Tone.Low -> 'è', Tone.High -> 'é'),
		  			   	  'ẹ' -> Map(Tone.Mid -> 'ẹ', Tone.Low -> "ẹ̀ ".head, Tone.High -> "ẹ́ ".head),
		  			   	  'i' -> Map(Tone.Mid -> 'i', Tone.Low -> 'ì', Tone.High -> 'í'),
		  			   	  'o' -> Map(Tone.Mid -> 'o', Tone.Low -> 'ò', Tone.High -> 'ó'),
		  			   	  'ọ' -> Map(Tone.Mid -> 'ọ', Tone.Low -> "ọ̀ ".head, Tone.High -> "ọ́ ".head),
		  			   	  'u' -> Map(Tone.Mid -> 'u', Tone.Low -> 'ù', Tone.High -> 'ú')		  			   
		  			 	)
  val allowed = characterMap flatten (_._2) map (toneChar => toneChar._2) toList
		  			 
  def normalise(char:Char):Char = {
    if (characterMap.get('a').exists(tone => tone.values.toList contains char)) 'a'
    else if (characterMap.get('e').exists(tone => tone.values.toList contains char)) 'e'
    else if (characterMap.get('ẹ').exists(tone => tone.values.toList contains char)) 'ẹ'
    else if (characterMap.get('i').exists(tone => tone.values.toList contains char)) 'i'
    else if (characterMap.get('o').exists(tone => tone.values.toList contains char)) 'o'
    else if (characterMap.get('ọ').exists(tone => tone.values.toList contains char)) 'ọ'
    else if (characterMap.get('u').exists(tone => tone.values.toList contains char)) 'u'
    else char
  }
  
  def as(tone:Tone, char:Char):Char = {
    if (!(allowed contains char)) 
      char
    else
      characterMap.get(normalise(char)).get(tone)    
  }
  
  def get(char:Char):Tone = {
    if (!(allowed contains char))
      Mid
    else
      characterMap.get(normalise(char)).map(e => e map (_.swap)).get(char)
  }
}
import Tone._

/**
 * Bias tells us which part of a word a SpeechProperty affects
 */
object Bias extends Enumeration {
  type Bias = Value
  val Left, Right, Neutral = Value  
}
import Bias._

/**
 * Word properties are tokens for word features (helps parsing)
 */
trait SpeechProperty { 
  def bias = Bias.Neutral
  def count = 1

  def opposite:SpeechProperty
}

case object Root extends SpeechProperty { override val opposite = this }
case object Connector extends SpeechProperty { override val opposite = this }

case class Elided(override val bias:Bias, override val count:Int=1) extends SpeechProperty {
  override def equals(o:Any):Boolean = o match {
    case o:Elided => (o.bias == bias)
    case _ => false
  }
  
  def opposite:SpeechProperty = this.bias match {
    case Left => this.copy(Right)
    case Right => this.copy(Left)
    case _ => this.copy(Neutral)
  }

  
}

case class Assimilated(override val bias:Bias, override val count:Int=1) extends SpeechProperty {
  override def equals(o:Any):Boolean = o match {
    case o:Assimilated => (o.bias == bias)
    case _ => false
  }
  
  def opposite:SpeechProperty = this.bias match {
    case Left => this.copy(Right)
    case Right => this.copy(Left)
    case _ => this.copy(Neutral)
  }  
}
