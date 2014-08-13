package com.twitter.finagle.smtp.extension.auth

import com.twitter.finagle.smtp.{InvalidReply, Reply, Request}
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.util.Future
import org.apache.commons.codec.binary.Base64

/**
 * Filter that is applied when AUTH extension is supported.
 * Cancels authentication in case of improperly encoded challenge
 * and transforms server challenges sent in response to non-auth
 * requests to [[com.twitter.finagle.smtp.InvalidReply]]
 */
object AuthFilter extends SimpleFilter[Request, Reply] {
  def apply(request: Request, service: Service[Request, Reply]): Future[Reply] = {
    val expectChallenge = request match {
      case AuthRequest(_,_) => true
      case ChallengeResponse(_) => true
      case _ => false
    }

    service(request) flatMap {
      case ch@ServerChallenge(challenge) => {
        if (expectChallenge)
          if (Base64.isBase64(challenge) && !challenge.dropRight(1).contains('=')) Future.value(ch)
          else {
            service(ChallengeResponse.cancelAuth)
          }
        else Future.exception(InvalidReply("%d %s".format(ch.code, ch.info)))
      }
      case other => Future.value(other)
    }
  }
}