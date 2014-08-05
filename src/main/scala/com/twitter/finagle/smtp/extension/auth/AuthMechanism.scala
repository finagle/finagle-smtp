package com.twitter.finagle.smtp.extension.auth

case class AuthMechanism (
  name: String,
  reply: ServerChallenge => ChallengeResponse,
  hasInitialResponse: Boolean
  )
