package com.twitter.finagle.smtp.extension.pipelining

import com.twitter.finagle.smtp._
import com.twitter.finagle.smtp.extension.auth.AuthRequest
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.util.Future

/**
 * Filter that is applied when PIPELINING extension is supported.
 * Rejects group requests with the wrong order of commands (see [[http://tools.ietf.org/html/rfc2920]])
 * with BadCommandSequence reply.
 */
object PipeliningFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, pipeliningService: Service[Request, Reply]) = request match {
    case RequestGroup(reqs) =>  {
      // Check command order
      val withoutLast = reqs dropRight 1
      withoutLast find {
        case _: HelloRequest => true
        case _: BeginDataRequest => true
        case Request.VerifyAddress(_) => true
        case Request.ExpandMailingList(_) => true
        case Request.Quit => true
        case Request.Noop => true
        case AuthRequest(_, _) => true
        case _ => false
      } match {
        case Some(_) => Future.exception(BadCommandSequence("Bad command sequence in a request group"))

        // Specify and collect replies in a GroupedReply
        case None => {
          val reps = reqs map { req =>
            pipeliningService(GroupPart(req)) flatMap { rep =>
              Future.value(Reply(rep))
            }
          }
          Future.value(GroupedReply(reps))
        }
      }
    }

    case _ => pipeliningService(request)
  }
}
