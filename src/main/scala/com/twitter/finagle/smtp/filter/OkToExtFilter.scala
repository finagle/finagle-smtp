package com.twitter.finagle.smtp.filter

import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.smtp.Request
import com.twitter.finagle.smtp.reply._
import com.twitter.util.Future
import com.twitter.finagle.smtp.reply.OK
import com.twitter.finagle.smtp.reply.Extensions

object OkToExtFilter extends SimpleFilter[Request, Reply]{
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case Request.Hello => service(request) flatMap {
      case ok: OK =>
        val ext = new Extensions(ok.info) {
          override val isMultiline = ok.isMultiline
          override val lines = ok.lines
        }
        Future.value(ext)
      case other => Future.exception(InvalidReply("Invalid reply to an EHLO command: %s" format other))
    }

    case _ => service(request)
  }
}