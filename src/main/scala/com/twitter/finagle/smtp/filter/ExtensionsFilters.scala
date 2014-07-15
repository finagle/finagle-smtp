package com.twitter.finagle.smtp.filter

import com.twitter.finagle.smtp._
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.smtp.reply.{InsufficientStorageError, Reply}
import org.jboss.netty.util.CharsetUtil
import com.twitter.util.{Future, Base64StringEncoder}
import com.twitter.finagle.smtp.SmtpExtensions

/*
* Filter that is applied when 8BITMIME extension is not supported.
* Transforms all 8-bit data into 7-bit. In case of a MIME message
* encodes the content with base64 codec.
* */
object NoEightBitMimeFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case Request.TextData(text, enc) =>
      if (enc != CharsetUtil.US_ASCII) {
        val sevenBit = Request.TextData(text, CharsetUtil.US_ASCII)
        service(sevenBit)
      }
      else service(request)

    case Request.MimeData(mime) =>
      if (mime.contentTransferEncoding == "8bit") {
        val base64content = Base64StringEncoder.encode(mime.content).getBytes(CharsetUtil.US_ASCII)
        val base64mime = mime.copy(content = base64content).setContentTransferEncoding(TransferEncoding.Base64)
        service(Request.MimeData(base64mime))
      }
      else service(request)

    case _ => service(request)
  }
}

/*
* Filter that is applied when 8BITMIME extension is supported.
* Transforms all 8-bit text data into a MIME-message with corresponding headers.
* */
object EightBitMimeFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case Request.TextData(text, enc) =>
      if (enc != CharsetUtil.US_ASCII) {
        val mime = Mime.plainText(text.mkString("\r\n"), enc).setContentTransferEncoding(TransferEncoding.EightBit)
        service(Request.MimeData(mime))
      }
      else service(request)

    case _ => service(request)
  }
}

/*
* Filter that is applied when SIZE extension is not supported.
* Transforms a NewMailingSession request that declares message size
* into AddSender request, which does not.
* */
object NoSizeDeclarationFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case Request.NewMailingSession(sender, _) => service(Request.AddSender(sender))

    case _ => service(request)
  }
}

/*
* Filter that is applied when SIZE extension is supported.
* If declared message size is more than the value set by extension,
* completes the future with InsufficientStorageError
* and doesn't send anything.
* */
class SizeDeclarationFilter(maxSize: Int) extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case Request.NewMailingSession(sender, msgSize) => 
      if (maxSize > 0 && msgSize > maxSize)
        Future.exception(InsufficientStorageError("Message size is more than the server can accept"))
      else service(request)

    case _ => service(request)
  }
}