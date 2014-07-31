package com.twitter.finagle.smtp.extension

import com.twitter.finagle.smtp.reply.{Reply, RequestNotAllowed}
import com.twitter.finagle.smtp.{GroupedRequest, Request}
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.util.Future

/**
 * Filter that is applied when PIPELINING extension is not supported.
 * Rejects grouped requests with RequestNotAllowed error.
 */
object NoPipeliningFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case GroupedRequest(_) =>  Future.exception(new RequestNotAllowed)
    case _ => service(request)
  }
}
