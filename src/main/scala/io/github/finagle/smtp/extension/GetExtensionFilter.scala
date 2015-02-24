package io.github.finagle.smtp.extension

import com.twitter.finagle.SimpleFilter
import io.github.finagle.smtp.extension.auth.{AuthFilter, NoAuthFilter}
import io.github.finagle.smtp.extension.binarymime.NoBinaryMimeFilter
import io.github.finagle.smtp.extension.chunking.{ChunkingFilter, NoChunkingFilter}
import io.github.finagle.smtp.extension.eightbitmime.{EightBitMimeFilter, NoEightBitMimeFilter}
import io.github.finagle.smtp.extension.expn.NoExpnFilter
import io.github.finagle.smtp.extension.pipelining.{NoPipeliningFilter, PipeliningFilter}
import io.github.finagle.smtp.extension.size.{NoSizeDeclarationFilter, SizeDeclarationFilter}
import io.github.finagle.smtp.{Reply, Request}

/**
 * Associates extensions with their corresponding filters,
 * depending on whether given extension is supported or not.
 */
object GetExtensionFilter {
  import io.github.finagle.smtp.extension.Extension._
  /**
   * Associates given [[io.github.finagle.smtp.extension.Extension]]
   * with its corresponding filter given that it is supported by server.
   */
  val forSupported: PartialFunction[Extension, SimpleFilter[Request, Reply]] = {
    case EightBitMime => EightBitMimeFilter
    case Size(size) => new SizeDeclarationFilter(size)
    case Chunking => ChunkingFilter
    case Pipelining => PipeliningFilter
    case Auth => AuthFilter
  }

  /**
   * Associates given extension name with corresponding to this extension
   * filter, given that this extension is not supported by server.
   */
  val forUnsupportedExtensions: Map[Extension, SimpleFilter[Request, Reply]] = Map(
    EightBitMime -> NoEightBitMimeFilter,
    Size() -> NoSizeDeclarationFilter,
    Chunking -> NoChunkingFilter,
    BinaryMime -> NoBinaryMimeFilter,
    Pipelining -> NoPipeliningFilter,
    Auth -> NoAuthFilter,
    Expn -> NoExpnFilter
  )
}
