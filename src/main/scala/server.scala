/**
 * @author damola
 *
 */

import javax.servlet.ServletContext
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra._
import org.scalatra.servlet.ScalatraListener
import org.json4s.{DefaultFormats, Formats, JArray, JString}
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import net.mabogunje.yorudi._

/**
  * 
  */
class YorubaController extends ScalatraServlet {

    protected implicit val jsonFormats: Formats = DefaultFormats

    //This collection represents a simple in-memory data source (i.e. it is mutable and not thread-safe)
    val dictionaryPaths = Map[String, String](
      ("cms", "cms.en.yor"),
      ("gpt", "gpt.en.yor"),
      ("names", "names.en.yor"),
      ("sample", "sample.en.yor")
    )

    error {
      case e: IllegalArgumentException =>
        halt(BadRequest(Map("error" -> "Invalid input", "message" -> e.getMessage)))
      case e: Exception =>
        halt(InternalServerError(Map("error" -> "An unexpected error occurred", "message" -> e.getMessage)))
    }


    val parser:FileParser = Yorudi
    val writer:JsonWriter = new JsonWriter()

    // Load dictionaries on-demand using the cache
    def getDictionary(name: String): IndexedDictionary = {
        val path = dictionaryPaths.getOrElse(name, "")
        if (path.isEmpty) {
            println(s"Error: Dictionary '$name' not found.")
            IndexedDictionary(Map(), "")
        } else {
            DictionaryCache.getDictionary(name, path)
        }
    }

    get("/word") {
        Ok(Serialization.write(JArray(List())))
    }

    get("/word/:word") {
        //Get parameters
        val dictName = params.getOrElse("dictionary", "cms");
        val mode = params.getOrElse("mode", "match");
        val word = params("word");

        // Retrieve the pre-loaded dictionary
        val dictionary = getDictionary(dictName)
        var results:YorubaDictionary = YorubaDictionary()

        // Depending on the mode, get appropriate results
        mode match {
            case "strict" => results = dictionary.strictLookup(word)
            case "related" => results = dictionary.lookupRelated(word)
            case "derivative" => results = dictionary.lookupDerivatives(word)
            case _ => results = dictionary.lookup(word)
        }

        // Return results
        if(results.size > 0) {
            val json = compact(render(writer.writeGlossary(results)))
            Ok(json)
        } else {
            val error = Map("error" -> "Word Not Found", "message" -> s"Yoruba word '${word}' not found in ${dictName} dictionary")
            val json = Serialization.write(error)
            NotFound(json)
        }        
    }
}

class ScalatraBootstrap extends LifeCycle {
    override def init(context: ServletContext) {
        context mount (new YorubaController, "/*")
    }
}

object YorubaRestService extends App {
    val port = 3330
    val server = new Server(port)

    val context = new WebAppContext()
    context.setContextPath("/")
    context.setResourceBase(".")
    context.setInitParameter(ScalatraListener.LifeCycleKey, "ScalatraBootstrap")
    context.setEventListeners(Array(new ScalatraListener))
    
    server.setHandler(context)
    server.start

    println("***** Supported operations *****")
    println("Word details: curl -v http://localhost:3330/word/<word>")
    println("********************************")
        
    server.join
}
