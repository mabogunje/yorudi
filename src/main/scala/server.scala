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
    val lookupModes = Set("match", "strict", "related", "derivative")
    val allowedOrigins = Set("https://mabogunje.github.io", "http://localhost:3330")
    val staticContentTypes = Map(
      "favicon.ico" -> "image/x-icon",
      "index.html" -> "text/html;charset=utf-8",
      "logo.jpg" -> "image/jpeg",
      "style.css" -> "text/css;charset=utf-8"
    )

    error {
      case e: IllegalArgumentException =>
        jsonContent()
        halt(BadRequest(errorJson("Invalid input", e.getMessage)))
      case e: Exception =>
        jsonContent()
        halt(InternalServerError(errorJson("An unexpected error occurred", e.getMessage)))
    }

    options("/*") {
      response.setHeader("Access-Control-Allow-Headers", request.getHeader("Access-Control-Request-Headers"))
    }

    val writer:JsonWriter = new JsonWriter()

    def jsonContent() {
        contentType = "application/json;charset=utf-8"
    }

    def errorJson(error:String, message:String):String = {
        Serialization.write(Map("error" -> error, "message" -> message))
    }

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
        DictionaryCache.getDictionary(name, dictionaryPaths(name))
    }

    get("/") {
        serveStaticResource("index.html")
    }

    get("/word") {
        jsonContent()
        Ok(Serialization.write(JArray(List())))
    }

    get("/word/:word") {
        // Set CORS policy
        Option(request.getHeader("Origin")) match {
          case Some(origin) if allowedOrigins.contains(origin) =>
            response.setHeader("Access-Control-Allow-Origin", origin)
            println(s"Accessing Yoruba Dictionary REST API from '$origin'.")
          case Some(origin) => println(s"Error: Access from origin '$origin' not allowed.")
          case None => ()
        }
        jsonContent()

        //Get parameters
        val dictName = params.getOrElse("dictionary", "gpt");
        val mode = params.getOrElse("mode", "match");
        val word = params("word").trim.toLowerCase();

        if(!dictionaryPaths.contains(dictName)) {
            BadRequest(errorJson(
                "Invalid dictionary",
                s"Dictionary '${dictName}' is not supported. Supported dictionaries: ${dictionaryPaths.keys.toList.sorted.mkString(", ")}"
            ))
        } else if(!lookupModes.contains(mode)) {
            BadRequest(errorJson(
                "Invalid mode",
                s"Mode '${mode}' is not supported. Supported modes: ${lookupModes.toList.sorted.mkString(", ")}"
            ))
        } else {
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
