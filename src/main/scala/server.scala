/**
 * @author damola
 *
 */

import javax.servlet.ServletContext
import java.io.InputStream
import java.nio.file.{Files, Paths}
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra._
import org.scalatra.CorsSupport
import org.scalatra.servlet.ScalatraListener
import org.json4s.{DefaultFormats, Formats, JArray}
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import net.mabogunje.yorudi._

/**
  * 
  */
class YorubaController extends ScalatraServlet with CorsSupport {

    protected implicit val jsonFormats: Formats = DefaultFormats

    //This collection represents a simple in-memory data source (i.e. it is mutable and not thread-safe)
    val dictionaryPaths = Map[String, String](
      ("cms", "dicts/cms.en.yor"),
      ("gpt", "dicts/gpt.en.yor"),
      ("names", "dicts/names.en.yor"),
      ("sample", "dicts/sample.en.yor")
    )
    val staticContentTypes = Map(
      "favicon.ico" -> "image/x-icon",
      "index.html" -> "text/html;charset=utf-8",
      "logo.jpg" -> "image/jpeg",
      "style.css" -> "text/css;charset=utf-8"
    )

    error {
      case e: IllegalArgumentException =>
        halt(BadRequest(Map("error" -> "Invalid input", "message" -> e.getMessage)))
      case e: Exception =>
        halt(InternalServerError(Map("error" -> "An unexpected error occurred", "message" -> e.getMessage)))
    }

    options("/*") {
      response.setHeader("Access-Control-Allow-Headers", request.getHeader("Access-Control-Request-Headers"))
    }

    val parser:FileParser = Yorudi
    val writer:JsonWriter = new JsonWriter()

    def readAllBytes(stream:InputStream):Array[Byte] = {
        try {
            stream.readAllBytes()
        } finally {
            stream.close()
        }
    }

    def staticResource(filename:String):Option[Array[Byte]] = {
        Option(servletContext.getResourceAsStream("/" + filename)).map(readAllBytes).orElse {
            val path = Paths.get("src/main/webapp", filename)
            if (Files.isRegularFile(path)) Some(Files.readAllBytes(path)) else None
        }
    }

    def serveStaticResource(filename:String) = {
        staticResource(filename) match {
            case Some(bytes) => {
                contentType = staticContentTypes(filename)
                bytes
            }
            case None => NotFound()
        }
    }

    // Load dictionaries on-demand using the cache
    def getDictionary(name: String): IndexedDictionary = {
        val path = dictionaryPaths.getOrElse(name, "")
        if (path.isEmpty) {
            println(s"Error: Dictionary '$name' not found.")
            IndexedDictionary.empty
        } else {
            DictionaryCache.getDictionary(name, path)
        }
    }

    get("/") {
        serveStaticResource("index.html")
    }

    get("/word") {
        Ok(Serialization.write(JArray(List())))
    }

    get("/word/:word") {
        // Set CORS policy
        val allowedOrigins = Set("https://mabogunje.github.io", "http://localhost:3330") // Define your allowed origins
        
        request.getHeader("Origin") match {
          case origin if allowedOrigins.contains(origin) =>
            response.setHeader("Access-Control-Allow-Origin", origin)
            println(s"Accessing Yoruba Dictionary REST API from '$origin'.")
          case _ => println("Error: Access from origin not allowed.")
        }

        //Get parameters
        val dictName = params.getOrElse("dictionary", "gpt");
        val mode = params.getOrElse("mode", "match");
        val word = params("word").trim.toLowerCase();

        // Retrieve the pre-loaded dictionary
        val dictionary = getDictionary(dictName)

        // Depending on the mode, get appropriate results
        val results:YorubaDictionary = mode match {
            case "strict" => dictionary.strictLookup(word)
            case "related" => dictionary.lookupRelated(word)
            case "derivative" => dictionary.lookupDerivatives(word)
            case _ => dictionary.lookup(word)
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

    get("/index.html") {
        serveStaticResource("index.html")
    }

    get("/style.css") {
        serveStaticResource("style.css")
    }

    get("/logo.jpg") {
        serveStaticResource("logo.jpg")
    }

    get("/favicon.ico") {
        serveStaticResource("favicon.ico")
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
    context.setResourceBase("src/main/webapp")
    context.setWelcomeFiles(Array("index.html"))
    context.setInitParameter(ScalatraListener.LifeCycleKey, "ScalatraBootstrap")
    context.setEventListeners(Array(new ScalatraListener))
    
    server.setHandler(context)
    server.start

    println("***** Supported operations *****")
    println("Word details: curl -v http://localhost:3330/word/<word>")
    println("********************************")
        
    server.join
}
