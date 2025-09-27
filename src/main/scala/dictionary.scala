/**
 * @author damola
 *
 */
package net.mabogunje.yorudi

import collection._
import YorubaImplicits._
import java.io.RandomAccessFile
import scala.io.Source
import java.text.Normalizer

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
  // Helper functions for string comparisons
  def strip(yoruba:String):String = {
    Normalizer.normalize(yoruba, Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}+", "").toLowerCase()
  }

  def standardizeDiacritics(yoruba:String):String = {
    Normalizer.normalize(yoruba, Normalizer.Form.NFC).toLowerCase()
  }

  // Helper function for retrieving data from files
  private def readLineAtOffset(file: RandomAccessFile, offset: Long): Option[(WordEntry, List[Meaning])] = {
    file.seek(offset)
    val line = file.readLine()
    if (line != null) {
      val parsed = parse(wordEntry, new String(line.getBytes("ISO-8859-1"), "UTF-8"))
      if (parsed.successful) Some(parsed.get) else None
    } else None
  }

  //Lookup functions
  def lookup(word:Any):YorubaDictionary = {

    val entries = index.keys.filter(entry => strip(entry) == strip(word.toString)).toList.sorted.distinct
    val file = new RandomAccessFile(filename, "r")
    try {
      val result = entries.map { entry =>
        val offset = index.get(entry).get
        readLineAtOffset(file, offset).get
      }.toMap
      YorubaDictionary(result)
    } finally {
      file.close()
    }
  }

  def strictLookup(word:Any):YorubaDictionary = {

    val entries = index.keys.filter(entry => standardizeDiacritics(entry.toString) == standardizeDiacritics(word.toString)).toList.sorted.distinct
    val file = new RandomAccessFile(filename, "r")
    try {
      val result = entries.map { entry =>
        val offset = index.get(entry).get
        readLineAtOffset(file, offset).get
      }.toMap
      YorubaDictionary(result)
    } finally {
      file.close()
    }
  }


  def lookupRelated(word:Any):YorubaDictionary = {
    val file = new RandomAccessFile(filename, "r")
    try {
      val results = index.flatMap { case (entryWord, offset) =>
        readLineAtOffset(file, offset).flatMap { case (entry, meanings) =>
        if (entry.word.decomposition.map(term => standardizeDiacritics(term.toYoruba)).contains(standardizeDiacritics(word.toString)) ||
          standardizeDiacritics(entry.word.toYoruba) == standardizeDiacritics(word.toString)
        ) {
            Some(entry -> meanings)
          } else None
        }
      }.toMap
      YorubaDictionary(results)
    } finally {
      file.close()
    }
  }

  def lookupDerivatives(word:Any):YorubaDictionary = {
    val file = new RandomAccessFile(filename, "r")
    try {
      val results = index.flatMap { case (entryWord, offset) =>
        readLineAtOffset(file, offset).flatMap { case (entry, meanings) =>
          if (standardizeDiacritics(entry.word.root.toYoruba) == standardizeDiacritics(word.toString)) {
            Some(entry -> meanings)
          } else None
        }
      }.toMap
      YorubaDictionary(results)
    } finally {
      file.close()
    }
  }
}

object DictionaryImplicits {
  implicit def map2dict(map:Map[WordEntry, List[Meaning]]):YorubaDictionary = YorubaDictionary(map)
}

