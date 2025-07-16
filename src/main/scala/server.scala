import javax.servlet.ServletContext
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra._
import org.scalatra.servlet.ScalatraListener
import org.json4s.{DefaultFormats, Formats, JArray, JString}
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization


/**
  * 
  */
class YorubaController extends ScalatraServlet {

    protected implicit val jsonFormats: Formats = DefaultFormats

    //This collection represents a simple in-memory data source (i.e. it is mutable and not thread-safe)
    val dictionaryPaths = Map[String, String](
        ("sample", "src/main/resources/dicts/sample.en.yor"),
        ("cms", "src/main/resources/dicts/cms.en.yor"),
        ("names", "src/main/resources/dicts/names.en.yor")
    )
    
    val parser:FileParser = Yorudi
    val writer:JsonWriter = new JsonWriter()

    // Load dictionaries once at startup
    val dictionaries: Map[String, YorubaDictionary] = dictionaryPaths.map { case (name, path) =>
        try {
            (name, parser.parse(path))
        } catch {
            case e: Exception =>
                println(s"Error loading dictionary '$name' from '$path': ${e.getMessage}")
                (name, YorubaDictionary()) // Return an empty dictionary on error
        }
    }

    get("/word") {
        Ok(Serialization.write(JArray(List())))
    }

    get("/word/:word") {
        //Get parameters
        var dictName = params.getOrElse("dictionary", "cms");
        var mode = params.getOrElse("mode", "match");
        val word = params("word");

        // Retrieve the pre-loaded dictionary
        val dictionary:YorubaDictionary = dictionaries.getOrElse(dictName, YorubaDictionary())
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
            val error = new JString(s"Yoruba word not found in ${dictName} dictionary")
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
