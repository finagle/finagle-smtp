package io.github.finagle.smtp.extension.pipelining

import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.util.Future
import io.github.finagle.smtp._

/**
 * Filter that is applied when ''PIPELINING'' extension is not supported.
 * Rejects grouped requests with [[io.github.finagle.smtp.RequestNotAllowed]].
 */
object NoPipeliningFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]) = request match {
    case RequestGroup(_) =>  Future.exception(new RequestNotAllowed)
    case GroupPart(req) => service(req)
    case _ => service(request)
  }
}
