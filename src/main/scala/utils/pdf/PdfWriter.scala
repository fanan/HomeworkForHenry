package utils.pdf

import org.apache.pdfbox.pdmodel.font.PDType1Font
import utils.WithClose

object PdfWriter {
  def main2(args: Array[String]): Unit = {
    val output = if (args.nonEmpty) {
      args(0)
    } else {
      "1.pdf"
    }
    WithClose(Document(output = output)) {
      document =>
        val font = PDType1Font.HELVETICA
        WithClose(new Page(document)) {
          page => {
            page.cell("hello", page.box, font, 16)
            page.verticalSplit(page.box.w / 2, 0, page.box.h)
          }
        }
    }
  }
}
