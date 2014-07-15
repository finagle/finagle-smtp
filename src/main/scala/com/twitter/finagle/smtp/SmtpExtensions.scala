package com.twitter.finagle.smtp

import com.twitter.finagle.{Filter, SimpleFilter}
import com.twitter.finagle.smtp.reply.Reply
import com.twitter.finagle.smtp.filter._

case class SmtpExtensions (supported: Seq[Extension] = Seq.empty)

case class Extension(keyword: String, params: Seq[String])

object GetExtensionFilter {
  val forSupported: PartialFunction[Extension, SimpleFilter[Request, Reply]] = {
    case Extension("8BITMIME", _) => EightBitMimeFilter
    case Extension("SIZE", size::_) => new SizeDeclarationFilter(size.toInt)
  }

  // filters applied in case there are no extensions with such names
  val forUnsupportedExtensions = Map[String, SimpleFilter[Request, Reply]] {
    "8BITMIME" -> NoEightBitMimeFilter
    "SIZE"     -> NoSizeDeclarationFilter
  }
}