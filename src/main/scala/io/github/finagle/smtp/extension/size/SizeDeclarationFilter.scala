package io.github.finagle.smtp.extension.size

import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.util.Future
import io.github.finagle.smtp._
import io.github.finagle.smtp.extension.ExtendedMailingSession

/**
 * Filter that is applied when ''SIZE'' extension is supported.
 * If declared message size is more than the value set by extension,
 * completes the future with InsufficientStorageError.
 */
class SizeDeclarationFilter(maxSize: Int) extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case ExtendedMailingSession(sender, ext) =>
      if (maxSize > 0 && ext.getOrElse("SIZE", "0").toInt > maxSize)
        Future.exception(InsufficientStorageError("Message size is more than the server can accept"))
      else service(request)

    case _ => service(request)
  }
}
