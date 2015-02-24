package io.github.finagle.smtp.extension.chunking

import io.github.finagle.smtp.BeginDataRequest

/**
 * Contains classes for requests used with CHUNKING extension
 */
object ChunkingReq {
  /**
   * A chunk of data with given size
   *
   * @param size The number of bytes in the chunk
   */
  case class BeginDataChunk(size: Int) extends BeginDataRequest("BDAT %d" format size)

  /**
   * The last chunk of data in a sequence, that has given size
   *
   * @param size The number of bytes in the chunk
   */
  case class BeginLastDataChunk(size: Int) extends BeginDataRequest("BDAT %d LAST" format size)
}
