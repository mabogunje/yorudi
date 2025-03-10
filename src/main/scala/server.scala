import javax.servlet.ServletContext
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra._
import org.scalatra.servlet.ScalatraListener
import org.json4s.{DefaultFormats, Formats}
import scala.util.parsing.json._

/**
  * 
  */
class YorubaController extends ScalatraServlet {

    protected implicit val jsonFormats: Formats = DefaultFormats

    //This collection represents a simple in-memory data source (i.e. it is mutable and not thread-safe)
    val dictionaries = Map[String, String](
        ("sample", "src/main/resources/dicts/sample.en.yor"),
        ("cms", "src/main/resources/dicts/cms.en.yor"),
        ("names", "src/main/resources/dicts/names.en.yor")
    )
    
    val parser:FileParser = Yorudi
    val writer:JsonWriter = new JsonWriter()

    get("/word") {
        var json = JSONArray(List());
        Ok(json)
    }

    get("/word/:word") {
        //Get parameters
        var dict = params.getOrElse("dictionary", "cms");
        var mode = params.getOrElse("mode", "match");
        val word = params("word");

        // Read in the queried dictionary
        val dictionary:YorubaDictionary = parser.parse(dictionaries(dict))
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
            val json = writer.writeGlossary(results)
            Ok(json)
        } else {
            val error = new Error(s"Yoruba word not found in ${dict} dictionary")
            val json = error

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
