package utils.pdf

import org.apache.pdfbox.pdmodel.font.PDType1Font
import org.apache.pdfbox.pdmodel.{PDPage, PDPageContentStream};

class Page(
            document: Document
          ) extends AutoCloseable {
  private[this] val page: PDPage = {
    val tmp = new PDPage()
    document.addPage(tmp)
    tmp
  }
  private[this] val stream: PDPageContentStream = new PDPageContentStream(document, page)

  def box: PDRect =
    PDRect(page.getMediaBox)

  def close(): Unit = {
    stream.close()
  }

  def cell(text: String, rect: PDRect, font: PDType1Font = document.font, fontSize: Int = document.fontSize): Unit = {
    val textWidth = font.getStringWidth(text) / 1000f * fontSize
    val textHeight = font.getHeight(fontSize)
    assert(textWidth <= rect.w)
    assert(textHeight <= rect.h)
    val paddingX = (rect.w - textWidth) / 2 + rect.x
    val paddingY = (rect.h - textHeight) / 2 + rect.y
    stream.beginText()
    stream.newLineAtOffset(paddingX, paddingY)
    stream.setFont(font, fontSize)
    stream.showText(text)
    stream.endText()
  }

  def verticalSplit(x: Float, begin: Float, end: Float): Unit = {
    stream.setLineDashPattern(Array(5), 0)
    stream.moveTo(x, begin)
    stream.lineTo(x, end)
    stream.stroke()
  }

  def horizontalSplit(y: Float, begin:Float, end:Float):Unit = {
    stream.setLineDashPattern(Array(5), 0)
    stream.moveTo(begin, y)
    stream.lineTo(end, y)
    stream.stroke()
  }
}

object Page {
  def apply(document: Document): Page = new Page(document)
}