/**
 * @author damola
 *
 */
package net.mabogunje.yorudi

import org.json4s.JArray
import org.json4s.JObject
import org.json4s.JString
import org.scalatest.flatspec.AnyFlatSpec

class YorudiSpec extends AnyFlatSpec {
  "The Yorudi CLI parser" should "parse valid dictionary lookups" in {
    val result = Yorudi.parseOptions(Map(), List("--dict", "gpt", "-s", "--fmt", "json", "aba"))

    assert(result == Right(Map('dict -> "gpt", 'mode -> "strict", 'format -> "json", 'word -> "aba")))
  }

  it should "parse equals-style long options" in {
    val result = Yorudi.parseOptions(Map(), List("--dict=gpt", "--fmt=xml", "-d", "aba"))

    assert(result == Right(Map('dict -> "gpt", 'format -> "xml", 'mode -> "derivative", 'word -> "aba")))
  }

  it should "reject a missing dictionary option" in {
    val result = Yorudi.parseOptions(Map(), List("aba"))

    assert(result == Left("Missing required option: --dict"))
  }

  it should "reject a missing lookup word" in {
    val result = Yorudi.parseOptions(Map(), List("--dict", "gpt"))

    assert(result == Left("Missing word to look up"))
  }

  it should "reject missing option values" in {
    val result = Yorudi.parseOptions(Map(), List("--dict"))

    assert(result == Left("Missing value for --dict"))
  }

  it should "reject unknown dictionaries" in {
    val result = Yorudi.parseOptions(Map(), List("--dict", "unknown", "aba"))

    assert(result == Left("Unknown dictionary: unknown"))
  }

  it should "reject unknown formats" in {
    val result = Yorudi.parseOptions(Map(), List("--dict", "gpt", "--fmt", "yaml", "aba"))

    assert(result == Left("Unknown format: yaml"))
  }

  it should "reject unknown switches" in {
    val result = Yorudi.parseOptions(Map(), List("--dict", "gpt", "--verbose", "aba"))

    assert(result == Left("Invalid option: --verbose"))
  }

  it should "pretty print JSON output" in {
    val result = Yorudi.formatOutput("json", JArray(List(JObject("definition" -> JString("ade")))))

    assert(result.contains("\n"))
    assert(result.contains("\"definition\""))
    assert(result.contains("\"ade\""))
    assert(!result.startsWith("JArray"))
  }

  it should "write non-JSON output directly" in {
    val result = Yorudi.formatOutput("plain", "1 word(s) found")

    assert(result == "1 word(s) found")
  }
}
