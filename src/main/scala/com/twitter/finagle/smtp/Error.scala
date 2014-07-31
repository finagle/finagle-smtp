package com.twitter.finagle.smtp

trait Error extends Exception with Reply

case class InvalidReply(content: String) extends Error {
  val code = ReplyCode.INVALID_REPLY_CODE
  val info = content
}
case class UnknownReplyCodeError(override val code: Int, info: String) extends Error

//This may be due to the necessary extension not being supported by server
class RequestNotAllowed extends PermanentNegativeCompletionReply {
  val code = ReplyCode.INVALID_REPLY_CODE
  val info = "The request you were trying to send is not allowed by client."
}