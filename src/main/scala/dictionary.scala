import collection._
import YorubaImplicits._
import java.io.RandomAccessFile
import scala.io.Source

case class YorubaDictionary(val self:Map[WordEntry, List[Meaning]] = Map[WordEntry, List[Meaning]]()) extends MapProxy[WordEntry, List[Meaning]] {
  override def +[B1 >: List[Meaning]](kv: (WordEntry, B1)) : YorubaDictionary = {
    val (key, value) = kv
    if(self.contains(key)) {
      var meanings = self.getOrElse(key, List()) ++: value.asInstanceOf[List[Meaning]]
      YorubaDictionary(self.updated(key, meanings))
    } else {
      YorubaDictionary(self.updated(key, value.asInstanceOf[List[Meaning]]))
    }
  }
  
  def ++(xs: Map[WordEntry, List[Meaning]]): YorubaDictionary = {
    YorubaDictionary(self ++ xs)
  }
}

case class IndexedDictionary(val index:Map[String, Long], val filename:String) extends FileParser {

  def lookup(word:Any):YorubaDictionary = {
    def strip(yoruba:String):String = {
      import java.text.Normalizer
      Normalizer.normalize(yoruba, Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}+", "").toLowerCase()
    }

    val entries = index.keys.filter(entry => strip(entry) == strip(word.toString)).toList.distinct
    val result = entries.map { entry =>
      val offset = index.get(entry).get
      val file = new RandomAccessFile(filename, "r")
      file.seek(offset)
      val line = file.readLine()
      file.close()
      parse(wordEntry, new String(line.getBytes("ISO-8859-1"), "UTF-8")).get
    }.toMap
    YorubaDictionary(result)
  }

  def strictLookup(word:Any):YorubaDictionary = {
    val entries = index.keys.filter(entry => entry == word.toString).toList.distinct
     val result = entries.map { entry =>
      val offset = index.get(entry).get
      val file = new RandomAccessFile(filename, "r")
      file.seek(offset)
      val line = file.readLine()
      file.close()
      parse(wordEntry, new String(line.getBytes("ISO-8859-1"), "UTF-8")).get
    }.toMap
    YorubaDictionary(result)
  }

  private def readLineAtOffset(offset: Long): Option[(WordEntry, List[Meaning])] = {
    val file = new RandomAccessFile(filename, "r")
    try {
      file.seek(offset)
      val line = file.readLine()
      if (line != null) {
        val parsed = parse(wordEntry, new String(line.getBytes("ISO-8859-1"), "UTF-8"))
        if (parsed.successful) Some(parsed.get) else None
      } else None
    } finally {
      file.close()
    }
  }

  def lookupRelated(word:Any):YorubaDictionary = {
    val results = index.flatMap { case (entryWord, offset) =>
      readLineAtOffset(offset).flatMap { case (entry, meanings) =>
        if (entry.word.decomposition.map(_.toYoruba).contains(word.toString) || entry.word.toYoruba == word.toString) {
          Some(entry -> meanings)
        } else None
      }
    }.toMap
    YorubaDictionary(results)
  }

  def lookupDerivatives(word:Any):YorubaDictionary = {
    val results = index.flatMap { case (entryWord, offset) =>
      readLineAtOffset(offset).flatMap { case (entry, meanings) =>
        if (entry.word.root.toYoruba == word.toString) {
          Some(entry -> meanings)
        } else None
      }
    }.toMap
    YorubaDictionary(results)
  }
}

object DictionaryImplicits {
  implicit def map2dict(map:Map[WordEntry, List[Meaning]]):YorubaDictionary = YorubaDictionary(map)
}

