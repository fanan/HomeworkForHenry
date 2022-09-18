package math.arithmetic

import utils.pdf.Document
import utils.{DateUtils, WithClose}

import scala.collection.mutable

object Homework {
  private val easyPuzzles = mutable.Queue.empty[Puzzle]
  private val hardPuzzles = mutable.Queue.empty[Puzzle]

  def generateDocument(config: Config): Unit = {
    WithClose(Document(config.output)) {
      document => {
        val oneDayHomeworks = generate(config)
        oneDayHomeworks.foreach(_.generatePage(document))
      }
    }
  }

  private def generate(config: Config): Seq[OneDayHomework] = {
    val base = if (config.start.nonEmpty) {
      config.start
    } else {
      DateUtils.today()
    }
    utils.DateUtils.getDays(base, config.days, true).map { day => generateOneDay(config, day) }
  }

  private def generateOneDay(config: Config, day: String): OneDayHomework = {
    fillPuzzles(config)
    val easy = config.number * config.easy / (config.easy + config.hard)
    val hard = config.number - easy

    assert(easyPuzzles.length >= easy)
    assert(hardPuzzles.length >= hard)

    val puzzles = mutable.ListBuffer.empty[Puzzle]
    0.until(easy).foreach(_ => {
      puzzles append easyPuzzles.dequeue()
    })
    0.until(hard).foreach(_ => {
      puzzles append hardPuzzles.dequeue()
    })
    OneDayHomework(day = day, puzzles = puzzles.toArray, config.cols, config.showDate)
  }

  private def fillPuzzles(config: Config): Unit = {
    val easy = config.number * config.easy / (config.easy + config.hard)
    val hard = config.number - easy
    if (easyPuzzles.length >= easy && hardPuzzles.length >= hard) {
      return
    }
    val r = scala.util.Random
    val puzzles = Puzzle.generate(config.min, config.max).filter(puzzle => {
      val result = puzzle.result
      result >= config.resultMin && result <= config.resultMax
    })
    val (easyAll, hardAll) = puzzles.partition(_.level < 1)
    assert(easyAll.length >= easy && hardAll.length >= hard)
    easyPuzzles appendAll r.shuffle(easyAll.filter(p => !easyPuzzles.contains(p)))
    hardPuzzles appendAll r.shuffle(hardAll.filter(p => !hardPuzzles.contains(p)))
  }
}
