package com.twitter.finagle.smtp

import com.twitter.finagle.{Filter, SimpleFilter}
import com.twitter.finagle.smtp.reply.Reply
import com.twitter.finagle.smtp.filter._

case class SmtpExtensions (supported: Seq[Extension] = Seq.empty)

object SmtpExtensions {
  val EIGHTBITMIME = "8BITMIME"
  val SIZE = "SIZE"
  val CHUNKING = "CHUNKING"
  val BINARYMIME = "BINARYMIME"
  val PIPELINING = "PIPELINING"
}

case class Extension(keyword: String, params: Seq[String])

object GetExtensionFilter {
  import SmtpExtensions._
  val forSupported: PartialFunction[Extension, SimpleFilter[Request, Reply]] = {
    case Extension(EIGHTBITMIME, _) => EightBitMimeFilter
    case Extension(SIZE, size::_) => new SizeDeclarationFilter(size.toInt)
    case Extension(CHUNKING, _) => ChunkingFilter
  }

  // filters applied in case there are no extensions with such names
  val forUnsupportedExtensions = Map[String, SimpleFilter[Request, Reply]] {
    EIGHTBITMIME -> NoEightBitMimeFilter
    SIZE         -> NoSizeDeclarationFilter
    CHUNKING     -> NoChunkingFilter
    BINARYMIME   -> NoBinaryMimeFilter
  }
}