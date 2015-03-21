import YorubaImplicits._
import Bias._
import Tone._
import org.scalatest._

class GrammarSpec extends FlatSpec {
  val rootWord = "dé";
  val linkedWord = Word("ade", List("à", rootWord as Root))
  val elidedWord = Word("sade", List("ṣé" as Elided(Right), linkedWord as Root))
  val assimilatedWordA = Word("kaabo", List("kú" as Assimilated(Right) as Root, "àbò" as Assimilated(Left, 3)))
  val assimilatedWordB = Word("kuule", List("kú" as Assimilated(Right, 3) as Root, "ilé" as Assimilated(Left)))    
  val complexWordA = Word("abanisise", List("a", "bá" as Assimilated(Right), "eni" as Assimilated(Left) as Root, "ṣiṣẹ"))
  val complexWordB = Word("abanigbele", List("a", "bá", "eni" as Elided(Left) as Root, "gbé", "íle" as Elided(Left)))
  
  "A root word" should "be it's own root" in {
    assert( Word(rootWord, Nil).root.toString() == "dé")
  }
  
  it should "have no decomposition" in {
    assert( Word(rootWord, Nil).decomposition == Nil)
  }
  
  it can "be complex" in {
    assert(elidedWord.root.toString() == linkedWord.toString())
  }
  
  "Words" can "be formed by linking" in {
    assert(linkedWord.toYoruba == "àdé")
    assert(linkedWord.root.toString() == rootWord)
  }
  
  it can "be formed by elision" in {
    assert(elidedWord.toYoruba == "ṣàdé")
  }
  
  it can "be formed by assimilation" in {
    assert(assimilatedWordA.toYoruba == "káàbò")
    assert(assimilatedWordA.root.toString() == "kú")
    assert(assimilatedWordB.toYoruba == "kúulé")
    assert(assimilatedWordB.root.toString() == "kú")
  }
  
  it can "be complexly constructed" in {
    assert(complexWordA.toYoruba == "abaniṣiṣẹ")
    assert(complexWordA.root.toString() == "eni")
    assert(complexWordB.toYoruba == "abánigbéle")
    assert(complexWordB.root.toString() == "eni")
  }
  
}