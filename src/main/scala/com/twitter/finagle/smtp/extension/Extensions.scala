package com.twitter.finagle.smtp.extension

/**
 * Incapsulates a set of SMTP extensions.
 */
case class Extensions(list: Extension*) {
  def lines(): Seq[String] = for {
      ext <- list
    } yield "%s %s".format(ext.keyword, ext.params mkString " ")
}

/**
 * Contains aliases for extension keywords
 */
object ExtensionKeywords {
  val EIGHTBITMIME = "8BITMIME"
  val SIZE = "SIZE"
  val CHUNKING = "CHUNKING"
  val BINARYMIME = "BINARYMIME"
  val PIPELINING = "PIPELINING"
  val AUTH = "AUTH"
  val EXPN = "EXPN"
}

object Extensions {
  import ExtensionKeywords._

  case object EightBitMime extends Extension {
    val keyword = EIGHTBITMIME
    val params = Seq.empty
  }

  case class Size(size: Int = 0) extends Extension {
    val keyword = SIZE
    val params = Seq(size.toString)
  }

  case object Chunking extends Extension {
    val keyword = CHUNKING
    val params = Seq.empty
  }

  case object BinaryMime extends Extension {
    val keyword = BINARYMIME
    val params = Seq.empty
  }

  case object Pipelining extends Extension {
    val keyword = PIPELINING
    val params = Seq.empty
  }

  case object Auth extends Extension {
    val keyword = AUTH
    val params = Seq.empty
  }

  case object Expn extends Extension {
    val keyword = EXPN
    val params = Seq.empty
  }
}

/**
 * An SMTP extension.
 */
trait Extension {

  /**
   * The keyword which appears in a successful reply to
   * [[com.twitter.finagle.smtp.Request.Hello]] if the
   * extension is supported.
   */
  val keyword: String

  /**
   * The sequence of string representation of parameters
   * sent with the keyword.
   */
  val params: Seq[String]
}

object Extension {
  import ExtensionKeywords._

  def apply(kwd: String, prms: Seq[String] = Seq.empty) = kwd match {
    case EIGHTBITMIME => Extensions.EightBitMime
    case SIZE => Extensions.Size(prms(0).toInt)
    case CHUNKING => Extensions.Chunking
    case BINARYMIME => Extensions.BinaryMime
    case PIPELINING => Extensions.Pipelining
    case AUTH => Extensions.Auth
    case EXPN => Extensions.Expn

    case _ => new Extension {
      val keyword = kwd
      val params = prms
    }
  }

}