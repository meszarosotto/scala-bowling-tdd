/**
 * meszaros.otto@gmail.com
 * Wed Apr 13 22:40:39 CEST 2022
 */
case class Frame(rolls: List[Int] = List.empty) {
  def isOpen: Boolean = !isStrike && rolls.size <= 1 || rolls.isEmpty
  def isSpare: Boolean = !isStrike && rolls.size == 2 && rolls.sum == 10
  def isStrike: Boolean = rolls.head == 10
}

case class Game(frames: List[Frame] = List.empty[Frame]) {

  private def endFrame: Boolean = frames.size >= 10

  def recordThrow(pins: Int): Game = {
    if (endFrame)
      Game(frames = frames.init :+ frames.last.copy(rolls = frames.last.rolls :+ pins))
    else
      frames.find(_.isOpen) match {
        case Some(frame) =>
          Game(frames = frames.filterNot(_.isOpen) :+ frame.copy(rolls = frame.rolls :+ pins))
        case None =>
          Game(frames :+ Frame(List(pins)))
      }
  }

  def score: Int = {
    @scala.annotation.tailrec
    def recursiveScore(frm: List[Frame], bonus: List[Int] = List(1, 1), acc: Int = 0): Int = {

      frm match {
        case x :: xs =>

          val sumRolls =
            (0 /: (x.rolls zip bonus)) {
              case (a, (b, c)) => a + b * c
            }

          val bonusLogic = x match {
            case frame if frame.isSpare  => List(1 + bonus.tail.head, 1)
            case frame if frame.isStrike => List(1 + bonus.tail.head, 2)
            case _                       => List(bonus.tail.head, 1)
          }

          if (endFrame)
            recursiveScore(xs, bonusLogic :+ 1, acc + sumRolls)
          else
            recursiveScore(xs, bonusLogic, acc + sumRolls)

        case Nil => acc
      }
    }
    recursiveScore(frames)
  }
}