import collection._
import YorubaImplicits._
import DictionaryImplicits._


/**
 * 
 */
case class YorubaDictionary(val self:Map[WordEntry, List[Meaning]] = Map[WordEntry, List[Meaning]]()) extends MapProxy[WordEntry, List[Meaning]] {
  def lookup(word:Any):YorubaDictionary = self filterKeys (k => (k.word == word || k.word.spelling == (word.toString map {Tone.normalise(_)}))) 
  def lookupRelated(word:Any) = self filterKeys (k => (k.word.decomposition map {_.toYoruba} contains word) || (k.word == word))
  
  override def +[T >: List[Meaning]] (kv:(WordEntry, T)) = {
    if (this contains kv._1) {
      self.updated(kv._1, this(kv._1) ::: kv._2.asInstanceOf[List[Meaning]])
    }
    else
      self.updated(kv._1, kv._2)
  }
}

/**
 * 
 */
object DictionaryImplicits {
  implicit def map2dict(map:Map[WordEntry, List[Meaning]]):YorubaDictionary = YorubaDictionary(map)
}

