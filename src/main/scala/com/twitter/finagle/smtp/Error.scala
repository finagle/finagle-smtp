package com.twitter.finagle.smtp

/**
 * Basic trait for error SMTP replies.
 */
trait Error extends Exception with Reply

/**
 * A reply that is either not syntactically an SMTP reply
 * or is not expected in given circumstances.
 *
 * @param info The string representation of what was received
 *                or another useful information.
 */
case class InvalidReply(info: String) extends Error {
  val code = ReplyCode.INVALID_REPLY_CODE
}

/**
 * A syntactically correct SMTP reply with unknown reply code.
 */
case class UnknownReplyCodeError(override val code: Int, info: String) extends Error

//This may be due to the necessary extension not being supported by server
class RequestNotAllowed extends PermanentNegativeCompletionReply {
  val code = ReplyCode.INVALID_REPLY_CODE
  val info = "The request you were trying to send is not allowed by client."
}
