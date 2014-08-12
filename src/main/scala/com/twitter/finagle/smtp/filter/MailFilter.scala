package com.twitter.finagle.smtp.filter

import com.twitter.finagle.smtp._
import com.twitter.finagle.smtp.extension.{ExtendedMailingSession, BodyEncoding}
import com.twitter.finagle.{Filter, Service}
import com.twitter.util.{JavaTimer, Future}
import com.twitter.util.TimeConversions._

/**
 * Sends [[com.twitter.finagle.smtp.EmailMessage]], transforming it to a sequence of SMTP commands.
 */
object MailFilter extends Filter[EmailMessage, Unit, Request, Reply]{
  implicit val timer = new JavaTimer

  override def apply(msg: EmailMessage, send: Service[Request, Reply]): Future[Unit] = {
    val body = msg.body
    val bodyEnc = body.contentTransferEncoding match {
      case "8bit"   => BodyEncoding.EightBit
      case "binary" => BodyEncoding.Binary
      case _        => BodyEncoding.SevenBit
    }
    val envelope: Seq[Request] =
      Seq(ExtendedMailingSession(msg.sender)
                 .messageSize(body.size)
                 .bodyEncoding(bodyEnc)) ++
        msg.to.map(Request.AddRecipient(_))  ++
        msg.cc.map(Request.AddRecipient(_))  ++
        msg.bcc.map(Request.AddRecipient(_))

    val data: Seq[Request] = body match {
      case mimepart: MimePart => Seq(Request.MimeData(mimepart))
      case multipart: MimeMultipart =>
        Seq(Request.TextData(multipart.getMimeHeaders)) ++ {
        for (part <- multipart.parts)
        yield Seq (
          Request.TextData(Seq(multipart.delimiter)),
          Request.MimeData(part)
        )
        }.flatten ++
        Seq(Request.TextData(Seq(multipart.closingDelimiter)))
    }

    val reqs: Seq[Request] =
      Seq(Request.Reset) ++
      envelope ++
      Seq(Request.BeginData) ++
      data

    val freqs = for (req <- reqs) yield send(req)

    val resetResponse = send(Request.Reset)
    val envelopeResponse = envelope map { send(_) within 5.minutes }
    val dataInitResponse = send(Request.BeginData) within 2.minutes
    val dataResponse = data map { send(_) within 3.minutes }


     Future.join(freqs)

   }
 }