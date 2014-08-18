package com.twitter.finagle.smtp.extension.chunking

import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.smtp.{Reply, Request, RequestNotAllowed}
import com.twitter.util.Future

/**
* Filter that is applied when ''CHUNKING'' extension is not supported.
* Rejects any message related to chunking with [[com.twitter.finagle.smtp.RequestNotAllowed]].
*/
object NoChunkingFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case ChunkingReq.BeginDataChunk(_) => Future.exception(new RequestNotAllowed)
    case ChunkingReq.BeginLastDataChunk(_) => Future.exception(new RequestNotAllowed)

    case _ => service(request)
  }
}
