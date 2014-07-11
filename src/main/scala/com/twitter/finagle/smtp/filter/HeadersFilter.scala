package com.twitter.finagle.smtp.filter

import java.text.SimpleDateFormat
import java.util.Locale

import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.smtp.{EmailBuilder, MailingAddress, DefaultEmail, EmailMessage}
import com.twitter.util.Future

/**
 * Adds email headers to message body.
 */
object HeadersFilter extends SimpleFilter[EmailMessage, Unit] {
   def apply(msg: EmailMessage, send: Service[EmailMessage, Unit]): Future[Unit] = {
     val fields = Map[String, String](
         "Date"     -> new SimpleDateFormat("EE, dd MMM yyyy HH:mm:ss ZZ", Locale.forLanguageTag("eng")).format(msg.getDate),
         "From"     -> MailingAddress.mailboxList(msg.getFrom),
         "To"       -> MailingAddress.mailboxList(msg.getTo),
         "Cc"       -> MailingAddress.mailboxList(msg.getCc),
         "Bcc"      -> MailingAddress.mailboxList(msg.getBcc),
         "Reply-To" -> MailingAddress.mailboxList(msg.getReplyTo),
         "Subject"  -> msg.getSubject
       ).filter(!_._2.isEmpty)

     val fieldsWithSender = if (msg.getFrom.length > 1) fields.updated("Sender", msg.getSender.mailbox)
                            else fields

     val richmsg = EmailBuilder(msg)
                     .setBody(msg.getBody.addHeaders(fieldsWithSender))
                     .build

     send(richmsg)
   }
 }
