/**
 * meszaros.otto@gmail.com
 * Wed Apr 13 22:40:39 CEST 2022
 */
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GameSpec extends AnyFlatSpec with Matchers {

  private def throwLoop(n: Int, pins: Int, count: Int = 1, game: Game = Game()): Game = {
    if(count > n)
      game
    else
      throwLoop(n, pins, count +1, game.recordThrow(pins))
  }

  "throw a miss" should "add the zeros" in {
    val game1 = Game()
    game1.recordThrow(0).score shouldBe 0
  }

  "throw 1+2+6" should "add natural numbers" in {
    val game1 = Game()
    val game2 = game1.recordThrow(1)
    val game3 = game2.recordThrow(2)
    val game4 = game3.recordThrow(6)
    game4.score shouldBe 9
  }

  "20 * 1 pins throw" should "add many natural number" in {
    throwLoop(20, 1)
      .score shouldBe 20
  }

  "(/)4+6 + 1" should "process spare bonus properly" in {
    Game().recordThrow(4)
      .recordThrow(6)
      .recordThrow(1)
      .score shouldBe 12
  }

  "(/)0+10 + 1 + 1" should "identify spare properly" in {
    Game().recordThrow(0)
      .recordThrow(10)
      .recordThrow(1)
      .recordThrow(1)
      .score shouldBe 13
  }

  "(X) + 1 + 1" should "identify strike properly" in {
    Game().recordThrow(10)
      .recordThrow(1)
      .recordThrow(1)
      .score shouldBe 14
  }

  "(X) + (X) + 1 + 1" should "process sequence of strikes" in {
    Game().recordThrow(10)
      .recordThrow(10)
      .recordThrow(1)
      .recordThrow(1)
      .score shouldBe 35
  }

  "(/)5+5 + (/)4+6 + 1" should "keep the order in processing spares, summarize 26" in {
    Game().recordThrow(5)
      .recordThrow(5)
      .recordThrow(4)
      .recordThrow(6)
      .recordThrow(1)
      .score shouldBe 26
  }

  "(/)4+6 + (/)5+5 + 1" should "process sequence of spares in proper order, summarize 27" in {
    Game().recordThrow(4)
      .recordThrow(6)
      .recordThrow(5)
      .recordThrow(5)
      .recordThrow(1)
      .score shouldBe 27
  }

  "16 * 1 + (X) + 1 + 1" should "process strike bonus collected before endGame" in {
    throwLoop(16,1)
      .recordThrow(10)
      .recordThrow(1)
      .recordThrow(1)
      .score shouldBe (30)
  }

  "16 * 1 + (/)7+3 + 1 + 1" should "process strike bonus collected before endGame" in {
    throwLoop(16,1)
      .recordThrow(7)
      .recordThrow(3)
      .recordThrow(1)
      .recordThrow(1)
      .score shouldBe (29)
  }

  "18 * 1 + (X)(X)(X)" should "process all strikes in endGame" in {
    throwLoop(18,1)
      .recordThrow(10)
      .recordThrow(10)
      .recordThrow(10)
      .score shouldBe (48)
  }

  "perfect game" should "ever be your best memory :) 300 points" in {
    throwLoop(12,10)
      .score shouldBe 300
  }
}
