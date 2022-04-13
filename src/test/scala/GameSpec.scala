import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GameSpec extends AnyFlatSpec with Matchers {

  "throw a miss" should "add the zeros" in {
    val game1 = new Game()
    game1.recordThrow(0)
    game1.score shouldBe 0
  }

  "throw 1+2+6" should "add natural numbers" in {
    val game1 = new Game()
    val game2 = game1.recordThrow(1)
    val game3 = game2.recordThrow(2)
    val game4 = game3.recordThrow(6)
    game4.score shouldBe 9
  }


  "20 * 1 pins throw" should "add many natural number" in {
    var game = new Game
    (1 to 20).foreach(i => game = game.recordThrow(1))
    game.score shouldBe 20
  }

  "(/)4+6 + 1" should "process spare bonus properly" in {
    var game = new Game
    game.recordThrow(4)
    game.recordThrow(6)
    game.recordThrow(1)
    game.score shouldBe 12
  }

  "(/)0+10 + 1 + 1" should "identify spare properly" in {
    var game = new Game
    game.recordThrow(0)
    game.recordThrow(10)
    game.recordThrow(1)
    game.recordThrow(1)
    game.score shouldBe 13
  }

  "(X) + 1 + 1" should "identify strike properly" in {
    val game = new Game
    game.recordThrow(10)
    game.recordThrow(1)
    game.recordThrow(1)
    game.score shouldBe 14
  }

  "(X) + (X) + 1 + 1" should "process sequence of strikes" in {
    val game = new Game
    game.recordThrow(10)
    game.recordThrow(10)
    game.recordThrow(1)
    game.recordThrow(1)
    val s = game.score
    game.score shouldBe 35
  }

  "(/)5+5 + (/)4+6 + 1" should "keep the order in processing spares, summarize 26" in {
    val game = new Game
    game.recordThrow(5)
    game.recordThrow(5)
    game.recordThrow(4)
    game.recordThrow(6)
    game.recordThrow(1)
    val s = game.score
    game.score shouldBe 26
  }

  "(/)4+6 + (/)5+5 + 1" should "process sequence of spares in proper order, summarize 27" in {
    val game = new Game
    game.recordThrow(4)
    game.recordThrow(6)
    game.recordThrow(5)
    game.recordThrow(5)
    game.recordThrow(1)
    game.score shouldBe 27
  }

  "16 * 1 + (X) + 1 + 1" should "process strike bonus collected before endGame" in {
    val game = new Game
    (1 to 16).foreach(i => game.recordThrow(1))
    game.recordThrow(10)
    game.recordThrow(1)
    game.recordThrow(1)
    game.score shouldBe (30)
  }

  "16 * 1 + (/)7+3 + 1 + 1" should "process strike bonus collected before endGame" in {
    val game = new Game
    (1 to 16).foreach(i => game.recordThrow(1))
    game.recordThrow(7)
    game.recordThrow(3)
    game.recordThrow(1)
    game.recordThrow(1)
    game.score shouldBe (29)
  }

  "18 * 1 + (X)(X)(X)" should "process all strikes in endGame" in {
    val game = new Game
    (1 to 18).foreach(i => game.recordThrow(1))
    game.recordThrow(10)
    game.recordThrow(10)
    game.recordThrow(10)
    game.score shouldBe (48)
  }

  "perfect game" should "ever be your best memory :) 300 points" in {
    val game = new Game
    (1 to 12).foreach(i => game.recordThrow(10))
    game.score shouldBe 300
  }

}
