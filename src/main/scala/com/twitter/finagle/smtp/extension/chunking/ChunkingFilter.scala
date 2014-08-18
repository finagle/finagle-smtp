package com.twitter.finagle.smtp.extension.chunking

import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.smtp.{Reply, Request}

/**
 * Filter that is applied when ''CHUNKING'' extension is supported.
 * Resets the session in case of failed ''BDAT'' and ''DATA'' commands.
 */
object ChunkingFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case ChunkingReq.BeginDataChunk(_) => {
      service(request) onFailure {
        case _ => service(Request.Reset)
      }
    }
    case ChunkingReq.BeginLastDataChunk(_) => {
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
