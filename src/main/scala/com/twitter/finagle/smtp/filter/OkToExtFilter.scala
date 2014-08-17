package com.twitter.finagle.smtp.filter

import com.twitter.finagle.smtp._
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.util.Future

object OkToExtFilter extends SimpleFilter[Request, Reply]{
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case Request.Hello => service(request) flatMap {
      case ok: OK =>
        val ext = new ExtensionsReply(ok.info)
        Future.value(ext)
      case other => Future.exception(InvalidReply(other.toString))
    }

    case _ => service(request)
  }
}