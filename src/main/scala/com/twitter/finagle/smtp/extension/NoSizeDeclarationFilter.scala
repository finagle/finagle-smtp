package com.twitter.finagle.smtp.extension

import com.twitter.finagle.smtp.{Reply, Request}
import com.twitter.finagle.{Service, SimpleFilter}

/**
* Filter that is applied when SIZE extension is not supported.
* Removes SIZE extension from MAIL FROM command.
* */
object NoSizeDeclarationFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case Request.NewMailingSession(sender, ext) => service(Request.NewMailingSession(sender, ext - "SIZE"))

    case _ => service(request)
  }
}
