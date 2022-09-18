package utils.pdf

import org.apache.pdfbox.pdmodel.common.PDRectangle

case class PDRect(
                   x: Float = 0,
                   y: Float = 0,
                   w: Float = 0,
                   h: Float = 0
                 ) {
}

object PDRect {
  def apply(rect: PDRectangle): PDRect = {
    apply(rect.getLowerLeftX, rect.getLowerLeftY, rect.getWidth, rect.getHeight)
  }
}
