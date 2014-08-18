package com.twitter.finagle.smtp.extension.binarymime

import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.smtp.extension.ExtendedMailingSession
import com.twitter.finagle.smtp.{Reply, Request, RequestNotAllowed, TransferEncoding}
import com.twitter.util.Future

/**
 * Filter that is applied when ''BINARYMIME'' extension is not supported.
 * Removes ''BODY=BINARYMIME'' extension from ''MAIL FROM'' command and rejects
 * binary MIME messages with [[com.twitter.finagle.smtp.RequestNotAllowed]].
 */
object NoBinaryMimeFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case ExtendedMailingSession(sender, ext) => {
      ext.get("BODY") match {
        case Some("BINARYMIME") => service(ExtendedMailingSession(sender, ext - "BODY"))
        case _ => service(request)
      }
    }

    case Request.MimeData(mime) => {
      if (mime.contentTransferEncoding == TransferEncoding.Binary.value)
        Future.exception(new RequestNotAllowed)
      else service(request)
    }

    case _ => service(request)
  }
}
