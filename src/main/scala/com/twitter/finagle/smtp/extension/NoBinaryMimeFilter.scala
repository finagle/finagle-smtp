package com.twitter.finagle.smtp.extension

import com.twitter.finagle.smtp.{TransferEncoding, Request}
import com.twitter.finagle.smtp.reply.{Reply, RequestNotAllowed}
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.util.Future

/**
 * Filter that is applied when BINARYMIME extension is not supported.
 * Removes BODY=BINARYMIME extension from MAIL FROM command and rejects
 * binary MIME messages with RequestNotAllowed error.
 */
object NoBinaryMimeFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case Request.NewMailingSession(sender, ext) => {
      ext.get("BODY") match {
        case Some("BINARYMIME") => service(Request.NewMailingSession(sender, ext - "BODY"))
        case _ => service(Request.NewMailingSession(sender, ext))
      }
    }

    case Request.MimeData(mime) => {
      if (mime.contentTransferEncoding == TransferEncoding.Binary.value) Future.exception(new RequestNotAllowed)
      else service(request)
    }

    case _ => service(request)
  }
}
