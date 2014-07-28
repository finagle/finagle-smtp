package com.twitter.finagle.smtp

import com.twitter.finagle.{Filter, SimpleFilter}
import com.twitter.finagle.smtp.reply.Reply
import com.twitter.finagle.smtp.filter._

case class SmtpExtensions (supported: Seq[Extension] = Seq.empty)

object SmtpExtensions {
  val EIGHTBITMIME = "8BITMIME"
  val SIZE = "SIZE"
  val CHUNKING = "CHUNKING"
  val BINARYMIME = "BINARYMIME"
  val PIPELINING = "PIPELINING"
}

case class Extension(keyword: String, params: Seq[String])