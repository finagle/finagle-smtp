package com.twitter.finagle.smtp.extension

import com.twitter.finagle.smtp._
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.util.Future

/**
 * Filter that is applied when PIPELINING extension is not supported.
 * Rejects grouped requests with RequestNotAllowed error.
 */
object NoPipeliningFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case RequestGroup(_) =>  Future.exception(new RequestNotAllowed)
    case GroupPart(req) => service(req)
    case _ => service(request)
  }
}
