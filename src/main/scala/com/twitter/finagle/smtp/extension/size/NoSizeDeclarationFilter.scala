package com.twitter.finagle.smtp.extension.size

import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.smtp.extension.ExtendedMailingSession
import com.twitter.finagle.smtp.{Reply, Request}

/**
 * Filter that is applied when ''SIZE'' extension is not supported.
 * Removes ''SIZE'' extension from ''MAIL FROM'' command.
 */
object NoSizeDeclarationFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case ExtendedMailingSession(sender, ext) => service(ExtendedMailingSession(sender, ext - "SIZE"))
    case _ => service(request)
  }
}
