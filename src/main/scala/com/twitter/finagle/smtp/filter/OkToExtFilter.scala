package com.twitter.finagle.smtp.filter

import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.smtp._
import com.twitter.util.Future

/**
 * Transforms [[com.twitter.finagle.smtp.OK]] in reply to
 * [[com.twitter.finagle.smtp.Request.Hello]] into
 * [[com.twitter.finagle.smtp.ExtensionsReply]]
 */
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