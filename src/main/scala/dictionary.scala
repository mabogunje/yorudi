import collection._
import YorubaImplicits._
import DictionaryImplicits._


/**
 * 
 */
case class YorubaDictionary(val self:Map[WordEntry, List[Meaning]] = Map[WordEntry, List[Meaning]]()) extends MapProxy[WordEntry, List[Meaning]] {
     
  override def +[B1 >: List[Meaning]](kv: (WordEntry, B1)) : YorubaDictionary = {
    if(self.contains(kv._1)) {
      var meanings = self.getOrElse(kv._1, List()) ++: kv._2.asInstanceOf[List[Meaning]]
      self.updated(kv._1, meanings)
    } else {
      self.updated(kv._1, kv._2.asInstanceOf[List[Meaning]])
    }
  }
  
  def ++[B1 >: List[Meaning]](xs: GenTraversableOnce[(WordEntry, List[Meaning])]): YorubaDictionary = {
    var merged = for ((k,v) <- xs.toList.groupBy(_._1)) yield (k, v.distinct.flatMap(_._2))
    self ++ merged
  }
  
  def strictLookup(word:Any):YorubaDictionary = self filterKeys (
      k => (k.word.toYoruba == word)) 

  
  def lookup(word:Any):YorubaDictionary = self filterKeys (
      k => (k.word == word || k.word.spelling == (word.toString map {Tone.normalise(_)}))) 
  
  def lookupRelated(word:Any):YorubaDictionary = self filterKeys (
      k => (k.word.decomposition map {_.toYoruba} contains word) || (k.word == word))
}

/**
 * 
 */
object DictionaryImplicits {
  implicit def map2dict(map:Map[WordEntry, List[Meaning]]):YorubaDictionary = YorubaDictionary(map)
}

