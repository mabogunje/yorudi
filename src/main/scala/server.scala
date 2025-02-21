import javax.servlet.ServletContext
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra._
import org.scalatra.servlet.ScalatraListener
import org.json4s.{DefaultFormats, Formats}



class YorubaController extends ScalatraServlet {

    protected implicit val jsonFormats: Formats = DefaultFormats

    //This collection represents a simple in-memory data source (i.e. it is mutable and not thread-safe)
    val dictionaries = Map[String, String](
        ("sample", "src/main/resources/dicts/sample.en.yor"),
        ("cms", "src/main/resources/dicts/cms.en.yor"),
        ("names", "src/main/resources/dicts/names.en.yor")
    )
    
    val parser:FileParser = Yorudi
    val dict:YorubaDictionary = parser.parse(dictionaries("cms"))
    val writer:JsonWriter = new JsonWriter()

    get("/words/:word") {
        val results:YorubaDictionary = dict.lookup(params("word").toString())
        
        if(results.size > 0) {
            var json = writer.writeGlossary(results)
            Ok(json)
        } else {
            NotFound("Yoruba word not found in sample dictionary")
        }        
    }
}

class ScalatraBootstrap extends LifeCycle {
    override def init(context: ServletContext) {
        context mount (new YorubaController, "/*")
    }
}

object YorubaRestService extends App {
    val port = 8080
    val server = new Server(port)

    val context = new WebAppContext()
    context.setContextPath("/")
    context.setResourceBase(".")
    context.setInitParameter(ScalatraListener.LifeCycleKey, "ScalatraBootstrap")
    context.setEventListeners(Array(new ScalatraListener))
    
    server.setHandler(context)
    server.start

    println("***** Supported operations *****")
    println("Word details: curl -v http://localhost:8080/words/<word>")
    println("********************************")
        
    server.join
}
