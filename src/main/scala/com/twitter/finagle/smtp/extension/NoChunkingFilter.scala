package com.twitter.finagle.smtp.extension

import com.twitter.finagle.smtp.Request
import com.twitter.finagle.smtp.reply.{Reply, RequestNotAllowed}
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.util.Future

/**
* Filter that is applied when CHUNKING extension is not supported.
* Rejects any message related to chunking with RequestNotAllowed error.
* */
object NoChunkingFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case Request.BeginDataChunk(_) => Future.exception(new RequestNotAllowed)
    case Request.BeginLastDataChunk(_) => Future.exception(new RequestNotAllowed)

    case _ => service(request)
  }
}
