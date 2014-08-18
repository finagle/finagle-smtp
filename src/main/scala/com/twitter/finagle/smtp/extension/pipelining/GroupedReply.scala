package com.twitter.finagle.smtp.extension.pipelining

import com.twitter.finagle.smtp.Reply
import com.twitter.util.Future

/**
 * A group of replies to the request group using ''PIPELINING'' extension
 */
case class GroupedReply(reps: Seq[Future[Reply]]) extends Reply {
  val code = 000
  val info = ""
}