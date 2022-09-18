package math.arithmetic

import scopt.OptionParser

case class Config(
                   output: String = "1.pdf",
                   start: String = "",
                   days: Int = 8,
                   number: Int = 40,
                   min: Int = 0,
                   max: Int = 100,
                   easy: Int = 1,
                   hard: Int = 3,
                   cols: Int = 2,
                   resultMin: Int = Int.MinValue,
                   resultMax: Int = Int.MaxValue,
                   showDate: Boolean = true
                 ) {
}

object Config {
  def parseOpt(args: Array[String]): Option[Config] = {
    val parser = new OptionParser[Config]("homework") {
      override def errorOnUnknownArgument: Boolean = false

      opt[String]("output").optional().action((x, c) => c.copy(output = x))
      opt[String]("start").optional().action((x, c) => c.copy(start = x))
      opt[Int]("days").optional().action((x, c) => c.copy(days = x))
      opt[Int]("number").optional().action((x, c) => c.copy(number = x))
      opt[Int]("min").optional().action((x, c) => c.copy(min = x))
      opt[Int]("max").optional().action((x, c) => c.copy(max = x))
      opt[Int]("easy").optional().action((x, c) => c.copy(easy = x))
      opt[Int]("hard").optional().action((x, c) => c.copy(hard = x))
      opt[Int]("cols").optional().action((x, c) => c.copy(cols = x))
      opt[Int]("resultMin").optional().action((x, c) => c.copy(resultMin = x))
      opt[Int]("resultMax").optional().action((x, c) => c.copy(resultMax = x))
      opt[Boolean]("showDate").optional().action((x, c) => c.copy(showDate = x))
    }
    parser.parse(args, Config())
  }
}