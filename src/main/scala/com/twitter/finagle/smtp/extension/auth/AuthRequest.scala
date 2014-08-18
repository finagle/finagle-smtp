package com.twitter.finagle.smtp.extension.auth

import com.twitter.finagle.smtp.TextRequest

/**
 * Authentication request
 *
 * @param mechanism The SASL mechanism to use. If the mechanism can use
 * initial response, but it contains no data, '=' is used.
 */
case class AuthRequest(mechanism: AuthMechanism)
  extends TextRequest(
    mechanism.initialResponse match {
      case Some("") => "AUTH %s =".format(mechanism.name)
      case Some(resp) => "AUTH %s %s".format(mechanism.name, resp)
      case None => "AUTH %s".format(mechanism.name)
    }
  )

/**
 * The response to a server challenge received during authentication
 */
case class ChallengeResponse(resp: String) extends TextRequest(resp)

object ChallengeResponse {
  /**
   * The response used to cancel authentication.
   */
  val cancelAuth = ChallengeResponse("*")
}