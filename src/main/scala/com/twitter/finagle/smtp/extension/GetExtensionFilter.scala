package com.twitter.finagle.smtp.extension

import com.twitter.finagle.SimpleFilter
import com.twitter.finagle.smtp.extension.auth.{AuthFilter, NoAuthFilter}
import com.twitter.finagle.smtp.extension.binarymime.NoBinaryMimeFilter
import com.twitter.finagle.smtp.extension.chunking.{ChunkingFilter, NoChunkingFilter}
import com.twitter.finagle.smtp.extension.eightbitmime.{EightBitMimeFilter, NoEightBitMimeFilter}
import com.twitter.finagle.smtp.extension.expn.NoExpnFilter
import com.twitter.finagle.smtp.extension.pipelining.{NoPipeliningFilter, PipeliningFilter}
import com.twitter.finagle.smtp.extension.size.{NoSizeDeclarationFilter, SizeDeclarationFilter}
import com.twitter.finagle.smtp.{Reply, Request}

/**
 * Associates extensions with their corresponding filters,
 * depending on whether given extension is supported or not.
 */
object GetExtensionFilter {
  import com.twitter.finagle.smtp.extension.SmtpExtensions._
  /**
   * Associates given [[com.twitter.finagle.smtp.extension.Extension]]
   * with its corresponding filter given that it is supported by server.
   */
  val forSupported: PartialFunction[Extension, SimpleFilter[Request, Reply]] = {
    case Extension(EIGHTBITMIME, _) => EightBitMimeFilter
    case Extension(SIZE, size::_) => new SizeDeclarationFilter(size.toInt)
    case Extension(CHUNKING, _) => ChunkingFilter
    case Extension(PIPELINING, _) => PipeliningFilter
    case Extension(AUTH, _) => AuthFilter
  }

  /**
   * Associates given extension name with corresponding to this extension
   * filter, given that this extension is not supported by server.
   */
  val forUnsupportedExtensions: Map[String, SimpleFilter[Request, Reply]] = Map(
    EIGHTBITMIME -> NoEightBitMimeFilter,
    SIZE         -> NoSizeDeclarationFilter,
    CHUNKING     -> NoChunkingFilter,
    BINARYMIME   -> NoBinaryMimeFilter,
    PIPELINING   -> NoPipeliningFilter,
    AUTH         -> NoAuthFilter,
    EXPN         -> NoExpnFilter
  )
}
