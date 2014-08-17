package com.twitter.finagle.smtp.filter

import com.twitter.finagle.smtp._
import com.twitter.finagle.smtp.extension.{BodyEncoding, ExtendedMailingSession}
import com.twitter.finagle.{Filter, Service}
import com.twitter.util.{Future}

/**
 * Sends [[com.twitter.finagle.smtp.EmailMessage]], transforming it to a sequence of SMTP commands.
 */
object MailFilter extends Filter[EmailMessage, Unit, Request, Reply]{
  override def apply(msg: EmailMessage, send: Service[Request, Reply]): Future[Unit] = {
    val body = msg.getBody
    val bodyEnc = body.contentTransferEncoding match {
      case "8bit"   => BodyEncoding.EightBit
      case "binary" => BodyEncoding.Binary
      case _        => BodyEncoding.SevenBit
    }
    val envelope: Seq[Request] =
      Seq(ExtendedMailingSession(msg.getSender)
                 .messageSize(body.size)
                 .bodyEncoding(bodyEnc)) ++
        msg.getTo.map(Request.AddRecipient(_))  ++
        msg.getCc.map(Request.AddRecipient(_))  ++
        msg.getBcc.map(Request.AddRecipient(_))

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

     Future.join(freqs)

   }
 }
