package com.twitter.finagle.smtp.extension.auth

case class AuthMechanism (
  name: String,
  reply: ServerChallenge => ChallengeResponse,
  hasInitialResponse: Boolean
  )

object AuthMechanism {
  def plain(login: String, password: String) = AuthMechanism(
    "PLAIN",
    ch => ChallengeResponse(login + '\0' + password),
    true
  )
}