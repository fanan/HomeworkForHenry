package math.arithmetic

object Runner {
  def main(args: Array[String]): Unit = {
    val config = Config.parseOpt(args).get
    Homework.generateDocument(config)
  }
}
