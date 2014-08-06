package com.twitter.finagle.smtp.filter

import java.text.SimpleDateFormat
import java.util.Locale

import com.twitter.finagle.smtp.{EmailBuilder, EmailMessage, MailingAddress}
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.util.Future

import scala.collection.immutable.Iterable

/**
 * Adds email headers to message body.
 */
object HeadersFilter extends SimpleFilter[EmailMessage, Unit] {
   def apply(msg: EmailMessage, send: Service[EmailMessage, Unit]): Future[Unit] = {
     val fields = msg.headers groupBy { case (k, v) => k } map {
       case (key, values) => key -> values.map(_._2).mkString(",")
     }

     val richmsg = EmailBuilder(msg)
                     .setBody(msg.body.addHeaders(fields))
                     .build

     send(richmsg)
   }
 }
