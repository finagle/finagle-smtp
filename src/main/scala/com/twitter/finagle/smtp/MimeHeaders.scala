package com.twitter.finagle.smtp

// This file defines three most frequently used headers:
// Content-Type, Content-Transfer-Encoding and Content-Disposition

/**
 * A trait for values of MIME headers
 */
trait MimeHeaderValue {
  def value: String
}

/**
 * A MIME header.
 */
trait MimeHeader extends MimeHeaderValue{
  def name: String
}

/**
 * Parameters of a MIME header.
 */
trait MimeParams {
  /**
   * The parameters of a header
   */
  val params: Map[String, String]

  /**
   * Provides a string representation of parameters of a header
   * that should be added to its value. The values of parameters
   * are quoted to allow 8-bit characters.
   */
  def paramsString(): String = {
    if (params.isEmpty) ""
    else params
      .map {case (k: String, v: String) => "%s=\"%s\"".format(k, v)}
      .mkString("; ", "; ", "")
  }
}

/**
 * ''Content-Transfer-Encoding'' MIME header.
 */
sealed trait TransferEncoding extends MimeHeader {
  val name = "Content-Transfer-Encoding"
}

object TransferEncoding {
  case object SevenBit extends TransferEncoding { val value = "7bit" }
  case object EightBit extends TransferEncoding { val value = "8bit" }
  case object QuotedPrintable extends TransferEncoding { val value = "quoted-printable" }
  case object Binary extends TransferEncoding { val value = "binary" }
  case object Base64 extends TransferEncoding { val value = "base64" }

  /**
   * The default content transfer encoding
   */
  val default = SevenBit
}

/**
 * ''Content-Type'' MIME header.
 */
case class ContentType(mediatype: String, subtype: String, params: Map[String, String] = Map.empty)
  extends MimeHeader with MimeParams {
  val name: String = "Content-Type"
  val value: String = mediatype + "/" + subtype + paramsString()
}

object ContentType {

  /**
   * Parses given String into [[com.twitter.finagle.smtp.ContentType]].
   */
  def parse(typestring: String): ContentType = {

    /**
       Catch all quoted params with "; " to not split on it.
       We seek for groups like 'param="_value_"', where
       _value_ consists at least of one character and has
       arbitrary number of '; ' somewhere before or after it.
    */
    val qParamsRegExp = "[^; ]+=\"(.*; .*)*.+(.*; .*)*\"".r

    // Remove problem quoted params and split what is left
    // to find the main value (type and subtype)
    val normal = qParamsRegExp replaceAllIn (typestring, "")
    val normalSplit = normal split "; "

    // The text before the first ';' should be in the form of 'type/subtype'
    val fulltype = normalSplit.head split "/"

    // Transforms params to tuples (key, value)
    def paramTuples(params: Array[String]) =
      params map { par =>
        val sp = par split "="
        // Remove quotes, if the value is quoted
        val value = if (sp(1) contains "\"") {
          sp(1).tail dropRight 1
        } else sp(1)

        (sp(0), value)
        }

    val normParams = paramTuples(normalSplit.tail filterNot (_.isEmpty))
    val quotedParams = paramTuples((qParamsRegExp findAllIn typestring).toArray[String])

    ContentType(fulltype(0), fulltype(1), normParams.toMap[String, String] ++ quotedParams)
  }

  /**
   * The default content type
   */
  val default = ContentType("text", "plain")
}

/**
 * Content presentation value of ''Content-Disposition'' header.
 */
sealed trait ContentPresentation extends MimeHeaderValue

object ContentPresentation {
  case object Inline extends ContentPresentation { val value = "inline" }
  case object Attachment extends ContentPresentation { val value = "attachment" }
}

/**
 * ''Content-Disposition'' MIME header.
 */
case class ContentDisposition(pres: ContentPresentation, params: Map[String, String] = Map.empty)
  extends MimeHeader with MimeParams {
  def name = "Content-Disposition"
  def value = pres.value + paramsString()
}

object ContentDisposition {
  import ContentPresentation._

  /**
   * The default content disposition (inline)
   */
  val default = inline

  /**
   * The content disposition for inline data
   */
  val inline = ContentDisposition(Inline)

  /**
   * Constructs a ''Content-Disposition'' header for an attachment.
   *
   * @param filename The name of the attached file
   */
  def attachment(filename: String): ContentDisposition =
    ContentDisposition(Attachment, Map("filename" -> filename))

  /**
   * Constructs a ''Content-Disposition'' header for an attachment
   * without specifying the name of the file to attach.
   */
  def attachment(): ContentDisposition = ContentDisposition(Attachment)
}
