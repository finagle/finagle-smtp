package io.github.finagle.smtp.extension.eightbitmime

import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.util.Future
import io.github.finagle.smtp.extension.ExtendedMailingSession
import io.github.finagle.smtp.{Reply, Request, RequestNotAllowed}
import org.jboss.netty.util.CharsetUtil

/**
 * Filter that is applied when 8BITMIME extension is not supported.
 * Removes BODY=8BITMIME extension from MAIL FROM command.
 * Rejects requests with 8-bit data with [[io.github.finagle.smtp.RequestNotAllowed]].
 */
object NoEightBitMimeFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case ExtendedMailingSession(sender, ext) => {
      ext.get("BODY") match {
        case Some("8BITMIME") => service(ExtendedMailingSession(sender, ext - "BODY"))
        case _ => service(ExtendedMailingSession(sender, ext))
      }
    }

    case Request.TextData(text, enc) =>
      if (enc != CharsetUtil.US_ASCII) {
        Future.exception(new RequestNotAllowed)
      }
      else service(request)

    case Request.MimeData(mime) => {
      if (mime.contentTransferEncoding == "8bit") Future.exception(new RequestNotAllowed)
      else service(request)
    }

    case _ => service(request)
  }
}
