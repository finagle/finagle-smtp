package com.twitter.finagle.smtp.extension

import com.twitter.finagle.SimpleFilter
import com.twitter.finagle.smtp.extension.auth.{AuthFilter, NoAuthFilter}
import com.twitter.finagle.smtp.extension.binarymime.NoBinaryMimeFilter
import com.twitter.finagle.smtp.extension.chunking.{NoChunkingFilter, ChunkingFilter}
import com.twitter.finagle.smtp.extension.eightbitmime.{NoEightBitMimeFilter, EightBitMimeFilter}
import com.twitter.finagle.smtp.extension.expn.NoExpnFilter
import com.twitter.finagle.smtp.extension.pipelining.{PipeliningFilter, NoPipeliningFilter}
import com.twitter.finagle.smtp.extension.size.{SizeDeclarationFilter, NoSizeDeclarationFilter}
import com.twitter.finagle.smtp.{Reply, Request}

object GetExtensionFilter {
  import com.twitter.finagle.smtp.extension.SmtpExtensions._
  val forSupported: PartialFunction[Extension, SimpleFilter[Request, Reply]] = {
    case Extension(EIGHTBITMIME, _) => EightBitMimeFilter
    case Extension(SIZE, size::_) => new SizeDeclarationFilter(size.toInt)
    case Extension(CHUNKING, _) => ChunkingFilter
    case Extension(PIPELINING, _) => PipeliningFilter
    case Extension(AUTH, _) => AuthFilter
  }

  // filters applied in case there are no extensions with such names
  val forUnsupportedExtensions = Map[String, SimpleFilter[Request, Reply]] {
    EIGHTBITMIME -> NoEightBitMimeFilter
    SIZE         -> NoSizeDeclarationFilter
    CHUNKING     -> NoChunkingFilter
    BINARYMIME   -> NoBinaryMimeFilter
    PIPELINING   -> NoPipeliningFilter
    AUTH         -> NoAuthFilter
    EXPN         -> NoExpnFilter
  }
}
