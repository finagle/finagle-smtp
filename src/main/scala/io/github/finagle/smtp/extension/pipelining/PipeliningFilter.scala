package io.github.finagle.smtp.extension.pipelining

import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.util.Future
import io.github.finagle.smtp._
import io.github.finagle.smtp.extension.auth.AuthRequest

/**
 * Filter that is applied when ''PIPELINING'' extension is supported.
 * Rejects group requests with the wrong order of commands (see [[http://tools.ietf.org/html/rfc2920]])
 * with [[io.github.finagle.smtp.BadCommandSequence]].
 */
object PipeliningFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, pipeliningService: Service[Request, Reply]) = request match {
    case RequestGroup(reqs:Seq[Request]) =>  {
      // Check command order
      val withoutLast = reqs dropRight 1
      withoutLast find {
        case _: HelloRequest => true
        case _: BeginDataRequest => true
        case Request.VerifyAddress(_) => true
        case Request.ExpandMailingList(_) => true
        case Request.Quit => true
        case Request.Noop => true
        case AuthRequest(_) => true
        case _ => false
      } match {
        case Some(_) => Future.exception(BadCommandSequence("Bad command sequence in a request group"))
        // Specify and collect replies in a GroupedReply
        case None => {
          val reps = reqs map { req =>
            pipeliningService(GroupPart(req)) map { rep => Reply(rep) }
          }
          Future.value(GroupedReply(reps))
        }
      }
    }

    case _ => pipeliningService(request)
  }
}
