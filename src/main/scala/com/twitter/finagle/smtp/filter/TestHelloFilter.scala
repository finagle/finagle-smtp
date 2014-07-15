package com.twitter.finagle.smtp.filter

import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.smtp.Request
import com.twitter.finagle.smtp.reply.{Extensions, OK, RequestNotAllowed, Reply}
import com.twitter.util.Future

object TestHelloFilter extends SimpleFilter[Request, Reply]{
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case Request.Hello => service(request)
    case Request.Quit => service(request)

    case _ => Future.exception(new RequestNotAllowed)
  }
}
