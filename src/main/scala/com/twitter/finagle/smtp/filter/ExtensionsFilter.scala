package com.twitter.finagle.smtp.filter

import com.twitter.finagle.smtp._
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.smtp.reply.Reply
import org.jboss.netty.util.CharsetUtil
import com.twitter.util.Base64StringEncoder
import com.twitter.finagle.smtp.SmtpExtensions

class ExtensionsFilter(supportedExtensions: SmtpExtensions) extends SimpleFilter[Request, Reply] {
  // filters applied for available extensions
  lazy val extFilters = Map[String, SimpleFilter[Request, Reply]] {
    "8BITMIME" -> EightBitMimeFilter
  }
  // filters applied in case there are no extensions with such names
  lazy val noExtFilters = Map[String, SimpleFilter[Request, Reply]] {
    "8BITMIME" -> NoEightBitMimeFilter
  }

  def apply(request: Request, service: Service[Request, Reply]) = {
    val hasExt = for {
      (extension, filter) <- extFilters
      if supportedExtensions.extensionList contains extension
    } yield filter

    val noExt = for {
      (extension, filter) <- noExtFilters
      if !(supportedExtensions.extensionList contains extension)
    } yield filter

    val extendedService = (hasExt ++ noExt).foldRight(service) { _ andThen _}
    extendedService(request)
  }
}

/*
* Filter that is applied when 8BITMIME extension is missing.
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