package utils.pdf

import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.pdmodel.font.PDType1Font

case class Document(
                     output: String,
                     font: PDType1Font = PDType1Font.TIMES_ROMAN,
                     fontSize: Int = 16
                   ) extends PDDocument with AutoCloseable {
  override def close(): Unit = {
    save(output)
    super.close()
  }
}
