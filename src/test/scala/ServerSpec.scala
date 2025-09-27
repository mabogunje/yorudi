/**
 * @author damola
 *
 */

import org.json4s.DefaultFormats
import org.scalatra.test.scalatest._
import org.json4s.jackson.Serialization.write
import scala.util.parsing.json._

class YorubaControllerTests extends ScalatraFunSuite {

    implicit val formats = DefaultFormats

    addServlet(classOf[YorubaController], "/*")

    test("GET /word on YorubaRestService should return status 200 and an empty list") {
        get("/word") {
            val expected = JSONArray(List()).toString();

            status should equal (200);
            body should equal (expected);
        }
/*
    test("GET /word/aa on YorubaRestService should return a single definition with a single translation") {
        get("/word/aa") {
            val writer:YorudiWriter = new JsonWriter()
            val expected = WordEntry(Word("aa", List("aa" as Root)), List(Translation("word of exclamation").asInstanceOf[Meaning]))
        }
      }
*/
    }
}
