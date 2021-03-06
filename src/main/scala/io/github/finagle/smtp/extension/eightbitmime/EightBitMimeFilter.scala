package io.github.finagle.smtp.extension.eightbitmime

import com.twitter.finagle.{Service, SimpleFilter}
import io.github.finagle.smtp.{Mime, Reply, Request, TransferEncoding}
import org.jboss.netty.util.CharsetUtil

/**
 * Filter that is applied when ''BITMIME'' extension is supported.
 * Transforms all 8-bit text data into a MIME-message with corresponding headers.
 */
object EightBitMimeFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case Request.TextData(text, enc) =>
      if (enc != CharsetUtil.US_ASCII) {
        val mime = Mime.plainText(text.mkString("\r\n"), enc)
          .setContentTransferEncoding(TransferEncoding.EightBit)
        service(Request.MimeData(mime))
      }
      else service(request)

    case _ => service(request)
  }
}
