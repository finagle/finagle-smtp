package io.github.finagle.smtp.extension.pipelining

import com.twitter.util.Future
import io.github.finagle.smtp.Reply

/**
 * A group of replies to the request group using ''PIPELINING'' extension
 */
case class GroupedReply(reps: Seq[Future[Reply]]) extends Reply {
  val code = 0
  val info = ""
}
