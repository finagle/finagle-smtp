package com.twitter.finagle.smtp.extension

case class SmtpExtensions (supported: Seq[Extension] = Seq.empty)

object SmtpExtensions {
  val EIGHTBITMIME = "8BITMIME"
  val SIZE         = "SIZE"
  val CHUNKING     = "CHUNKING"
  val BINARYMIME   = "BINARYMIME"
  val PIPELINING   = "PIPELINING"
  val AUTH         = "AUTH"
  val EXPN         = "EXPN"
}

case class Extension(keyword: String, params: Seq[String])