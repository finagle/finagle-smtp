package com.twitter.finagle.smtp.extension.auth

import com.twitter.finagle.smtp.{PermanentNegativeCompletionReply, PositiveCompletionReply, PositiveIntermediateReply, Reply}

trait AuthReply extends Reply

object AuthReplyCode {
  val SERVER_CHALLENGE = 334
  val AUTH_REJECTED    = 535
  val AUTH_REQUIRED = 530
  val AUTH_SUCCESSFUL  = 235
}

case class ServerChallenge(challenge: String)
    extends PositiveIntermediateReply with AuthReply {
    val code = AuthReplyCode.SERVER_CHALLENGE
    val info = challenge
  }

case class AuthRejected(info: String)
    extends PermanentNegativeCompletionReply with AuthReply {
  val code = AuthReplyCode.AUTH_REJECTED
}

case class AuthRequired(info: String)
  extends PermanentNegativeCompletionReply with AuthReply {
  val code = AuthReplyCode.AUTH_REQUIRED
}

case class AuthSuccessful(info: String)
    extends PositiveCompletionReply with AuthReply {
  val code = AuthReplyCode.AUTH_SUCCESSFUL
}