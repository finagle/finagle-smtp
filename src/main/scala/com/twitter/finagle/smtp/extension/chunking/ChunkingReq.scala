package com.twitter.finagle.smtp.extension.chunking

import com.twitter.finagle.smtp.BeginDataRequest

object ChunkingReq {
  case class BeginDataChunk(size: Int) extends BeginDataRequest("BDAT %d" format size) //for CHUNKING
  case class BeginLastDataChunk(size: Int) extends BeginDataRequest("BDAT %d LAST" format size) //for CHUNKING; only for the last part
}
