package com.twitter.finagle.smtp.extension.auth

import com.twitter.finagle.smtp.TextRequest

case class AuthRequest(mechanism: AuthMechanism, initialResponse: String = "=")
  extends TextRequest(if (mechanism.hasInitialResponse) "AUTH %s %s".format(mechanism.name, initialResponse)
                      else "AUTH %s" format mechanism.name)

case class ChallengeResponse(resp: String) extends TextRequest(resp)

object ChallengeResponse {
  val cancelAuth = ChallengeResponse("*")
}