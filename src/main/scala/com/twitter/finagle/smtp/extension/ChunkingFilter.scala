package com.twitter.finagle.smtp.extension

import com.twitter.finagle.smtp.Request
import com.twitter.finagle.smtp.reply.Reply
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.util.Await

/**
 * Filter that is applied when CHUNKING extension is supported.
 * Resets the session in case of failed BDAT and DATA commands.
 * */
object ChunkingFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case Request.BeginDataChunk(_) => {
      service(request) onFailure {
        case _ => service(Request.Reset)
      }
    }
    case Request.BeginLastDataChunk(_) => {
      service(request) onFailure {
        case _ => service(Request.Reset)
      }
    }
    case Request.BeginData => {
      service(request) onFailure {
        case _ => service(Request.Reset)
      }
    }

    case _ => service(request)
  }
}
