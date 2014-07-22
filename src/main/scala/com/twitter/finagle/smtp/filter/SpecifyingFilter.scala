package com.twitter.finagle.smtp.filter

import com.twitter.finagle.{Service, Filter}
import com.twitter.finagle.smtp.Request
import com.twitter.finagle.smtp.reply.{UnspecifiedReply, Reply}
import com.twitter.util.Future

object SpecifyingFilter extends Filter[Request, Reply, Request, UnspecifiedReply]{
  def apply(request: Request, service: Service[Request, UnspecifiedReply]) = {
    service(request) flatMap { unspec =>
      Future.value(Reply(unspec))
    }
  }
}
