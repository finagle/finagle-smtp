package com.twitter.finagle.smtp.extension

import com.twitter.finagle.smtp.reply.{BadCommandSequence, GroupedReply, Reply}
import com.twitter.finagle.smtp.{BeginDataRequest, GroupedRequest, HelloRequest, Request}
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.util.Future

/**
 * Filter that is applied when PIPELINING extension is supported.
 * Rejects group requests with the wrong order of commands (see [[http://tools.ietf.org/html/rfc2920]])
 * with BadCommandSequence reply.
 * This filter is only added to pipelining SMTP client.
 */
object PipeliningFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, pipeliningService: Service[Request, Reply]) = request match {
    case GroupedRequest(reqs) =>  {
      // Check command order
      val withoutLast = reqs dropRight 1
      withoutLast find {
        case _: HelloRequest => true
        case _: BeginDataRequest => true
        case Request.VerifyAddress(_) => true
        case Request.ExpandMailingList(_) => true
        case Request.Quit => true
        case Request.Noop => true
        case _ => false
      } match {
        case Some(_) => Future.exception(BadCommandSequence("Bad command sequence in a request group"))

        // Specify and collect replies in a GroupedReply
        case None => {
          val reps = reqs map {
            pipeliningService(_) flatMap { rep =>
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
