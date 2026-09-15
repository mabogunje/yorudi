/**
 * @author damola
 *
 */

import org.json4s.DefaultFormats
import org.json4s.JArray
import org.json4s.JBool
import org.json4s.JString
import org.json4s.jackson.JsonMethods._
import org.scalatra.test.scalatest._

class YorubaControllerTests extends ScalatraFunSuite {

    implicit val formats = DefaultFormats

    addServlet(classOf[YorubaController], "/*")

    test("GET / on YorubaRestService should return the homepage") {
        get("/") {
            status should equal (200);
            body should include ("<html lang=\"en\">");
            body should include ("Yor&ugrave;d&iacute;");
            body should include ("A clean, tone-aware dictionary");
        }
    }

    test("GET /style.css on YorubaRestService should return static styles") {
        get("/style.css") {
            status should equal (200);
            body should include ("body");
            body should include ("background");
        }
    }

    test("GET /word on YorubaRestService should return status 200 and an empty list") {
        get("/word") {
            val expected = "[]";

            status should equal (200);
            body should equal (expected);
        }
    }

    test("GET /word/:word on YorubaRestService should return matching definitions") {
        get("/word/ade?dictionary=sample&mode=match") {
            val json = parse(body)
            val JArray(results) = json
            val definition = results.head

            status should equal (200);
            (definition \ "definition") should equal (JString("àdé"));
            ((definition \ "decomposition")(0) \ "spelling") should equal (JString("à"));
            ((definition \ "decomposition")(0) \ "root") should equal (JBool(false));
            ((definition \ "meanings")(0) \ "description") should equal (JString("crown"));
        }
    }
}
