import collection._
import YorubaImplicits._
import DictionaryImplicits._


/**
 * 
 */
case class YorubaDictionary(val self:Map[WordEntry, List[Meaning]] = Map[WordEntry, List[Meaning]]()) extends MapProxy[WordEntry, List[Meaning]] {
  def lookup(word:Any):YorubaDictionary = self filterKeys (k => (k.word == word || k.word.spelling == (word.toString map {Tone.normalise(_)}))) 
  def lookupRelated(word:Any) = self filterKeys (k => (k.word.decomposition map {_.toYoruba} contains word) || (k.word == word))
  
  /*
  override def get(key: WordEntry): Option[List[Meaning]] = self get key
  override def iterator: Iterator[(WordEntry, List[Meaning])] = self.iterator
  
  override def +[B1 >: List[Meaning]](kv: (WordEntry, B1)) : Map[WordEntry, List[Meaning]] = {
    println("adding to map: " + kv + ", " + self)
    self + kv
  }
  
  override def -(key: WordEntry): Map[WordEntry, List[Meaning]] = {
    println("removing from map: " + key)
    self - key
  }
  */
   
}

/**
 * 
 */
object DictionaryImplicits {
  implicit def map2dict(map:Map[WordEntry, List[Meaning]]):YorubaDictionary = YorubaDictionary(map)
}

