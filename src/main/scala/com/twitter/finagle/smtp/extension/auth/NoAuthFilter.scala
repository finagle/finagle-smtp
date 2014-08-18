package com.twitter.finagle.smtp.extension.auth

import com.twitter.finagle.smtp.extension.ExtendedMailingSession
import com.twitter.finagle.smtp.{Reply, Request, RequestNotAllowed}
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.util.Future

/**
 * Filter that is applied when ''AUTH'' extension is not supported.
 * Removes ''AUTH'' extension from ''MAIL FROM'' command and rejects
 * authentication requests with [[com.twitter.finagle.smtp.RequestNotAllowed]].
 */
object NoAuthFilter extends SimpleFilter[Request, Reply]{
  def apply(request: Request, service: Service[Request, Reply]): Future[Reply] = request match {
    case AuthRequest(mechanism) => Future.exception(new RequestNotAllowed)
    case ExtendedMailingSession(sender, ext) =>
      service(ExtendedMailingSession(sender, ext - "AUTH"))

    case _ => service(request)
  }
}
