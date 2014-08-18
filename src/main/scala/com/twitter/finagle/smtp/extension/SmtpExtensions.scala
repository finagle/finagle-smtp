package com.twitter.finagle.smtp.extension

/**
 * Incapsulates a set of SMTP extensions.
 */
case class SmtpExtensions(supported: Extension*) {
  def lines(): Seq[String] = for {
      Extension(keyword, params) <- supported
    } yield "%s %s".format(keyword, params mkString " ")
}

/**
 * Contains aliases for extension keywords
 */
object SmtpExtensions {
  val EIGHTBITMIME = "8BITMIME"
  val SIZE         = "SIZE"
  val CHUNKING     = "CHUNKING"
  val BINARYMIME   = "BINARYMIME"
  val PIPELINING   = "PIPELINING"
  val AUTH         = "AUTH"
  val EXPN         = "EXPN"
}

/**
 * An SMTP extension.
 *
 * @param keyword The keyword which appears in a successful reply to
 *                [[com.twitter.finagle.smtp.Request.Hello]] if the
 *                extension is supported.
 * @param params The sequence of string representation of parameters
 *               sent with the keyword.
 */
case class Extension(keyword: String, params: Seq[String] = Seq.empty)