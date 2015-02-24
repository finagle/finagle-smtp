package io.github.finagle.smtp.extension.expn

import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.util.Future
import io.github.finagle.smtp.{Reply, Request, RequestNotAllowed}

/**
 * Filter that is applied when ''EXPN'' extension is not supported.
 * Rejects ''EXPN'' commands (expand mailing list) with
 * [[io.github.finagle.smtp.RequestNotAllowed]].
 */
object NoExpnFilter extends SimpleFilter[Request, Reply]{
  def apply(request: Request, service: Service[Request, Reply]): Future[Reply] = request match {
    case Request.ExpandMailingList(_) => Future.exception(new RequestNotAllowed)
    case _ => service(request)
  }
}
