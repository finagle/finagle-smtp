package com.twitter.finagle.smtp.extension

import com.twitter.finagle.SimpleFilter
import com.twitter.finagle.smtp.reply.Reply
import com.twitter.finagle.smtp.Request

/**
 * Created by lenovo on 31.07.2014.
 */
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
    PIPELINING   -> NoPipeliningFilter
  }
}
