package com.twitter.finagle.smtp

trait HeaderValue {
  def value: String
}

trait MimeHeader extends HeaderValue{
  def name: String
}

trait MimeParams {
  val params: Map[String, String]
  def paramsString = {
    if (params.isEmpty) ""
    else params
      .map {case (k: String, v: String) => "%s=\"%s\"".format(k, v)}
      .mkString("; ", "; ", "")
  }
}

sealed trait TransferEncoding extends MimeHeader {
  def name = "Content-Transfer-Encoding"
}
object TransferEncoding {
  case object SevenBit        extends TransferEncoding { def value = "7bit" }
  case object EightBit        extends TransferEncoding { def value = "8bit" }
  case object QuotedPrintable extends TransferEncoding { def value = "quoted-printable" }
  case object Binary          extends TransferEncoding { def value = "binary" }
  case object Base64          extends TransferEncoding { def value = "base64" }

  val default = SevenBit
}

case class ContentType(mediatype: String, subtype: String, params: Map[String, String] = Map.empty)
  extends MimeHeader with MimeParams {
  def name = "Content-Type"
  def value = mediatype + "/" + subtype + paramsString
}

object ContentType {
  def parse(typestring: String) = {
    val qParamsReg = "[^; ]+=\"(.*; .*)*.+(.*; .*)*\"".r //catch all quoted params with "; " to not split on it
    val normal = qParamsReg replaceAllIn (typestring, "")
    val normalSplit = normal split "; "
    val fulltype = normalSplit.head split "/"

    def paramTuples(params: Array[String]) = params map { par =>
                                      val sp = par split "="
                                      //get rid of quotes
                                      val value = if (sp(1) contains "\"") sp(1).tail dropRight 1
                                                  else sp(1)
                                      (sp(0), value)
                                      }
    val normParams = paramTuples { normalSplit.tail filterNot (_.isEmpty) }
    val quotedParams = paramTuples { (qParamsReg findAllIn typestring).toArray[String] }

    ContentType(fulltype(0), fulltype(1), normParams.toMap[String, String] ++ quotedParams)
  }

  val default = ContentType("text", "plain")
}

sealed trait ContentPresentation extends HeaderValue

object ContentDisposition {
  case object InlinePres extends ContentPresentation { def value = "inline" }
  case object AttachmentPres extends ContentPresentation { def value = "attachment" }

  val default = inline
  val inline = ContentDisposition(InlinePres)
  def attachment(filename: String) = ContentDisposition(AttachmentPres, Map("filename" -> filename))
  def attachment = ContentDisposition(AttachmentPres)
}

case class ContentDisposition(pres: ContentPresentation, params: Map[String, String] = Map.empty)
  extends MimeHeader with MimeParams {
  def name = "Content-Disposition"
  def value = pres.value + paramsString
}