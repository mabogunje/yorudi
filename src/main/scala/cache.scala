/**
 * @author damola
 *
 */
package net.mabogunje.yorudi

import collection._
import YorubaImplicits._

object DictionaryCache {
    private val cache = mutable.Map[String, IndexedDictionary]()

    def getDictionary(name: String, path: String): IndexedDictionary = {
        cache.getOrElseUpdate(name, {
            val parser = Yorudi
            println(s"Loading dictionary '$name' from '$path' into cache.")
            IndexedDictionary(parser.index(path), path)
        })
    }
}
