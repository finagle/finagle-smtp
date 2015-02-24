package io.github.finagle.smtp.extension.auth

import io.github.finagle.smtp.PermanentNegativeCompletionReply
import io.github.finagle.smtp.{PositiveCompletionReply, PositiveIntermediateReply, Reply}

/**
 * The base trait for all replies related to authentication.
 */
trait AuthReply extends Reply

/**
 * Contains reply codes of authentication replies. These
 * replies have '3' as their second digit.
 */
object AuthReplyCode {
  val SERVER_CHALLENGE = 334
  val AUTH_REJECTED = 535
  val AUTH_REQUIRED = 530
  val AUTH_SUCCESSFUL = 235
}

/**
 * The server challenge sent to client during authentication.
 */
case class ServerChallenge(challenge: String)
  extends PositiveIntermediateReply with AuthReply {
    val code = AuthReplyCode.SERVER_CHALLENGE
    val info = challenge
}

/**
 * Indicates that authentication has failed.
 */
case class AuthRejected(info: String)
  extends PermanentNegativeCompletionReply with AuthReply {
    val code = AuthReplyCode.AUTH_REJECTED
}

/**
 * Indicates that authentication is required to complete the action.
 */
case class AuthRequired(info: String)
  extends PermanentNegativeCompletionReply with AuthReply {
    val code = AuthReplyCode.AUTH_REQUIRED
}

/**
 * Indicates that authentication was performed successfully.
 */
case class AuthSuccessful(info: String)
  extends PositiveCompletionReply with AuthReply {
    val code = AuthReplyCode.AUTH_SUCCESSFUL
}