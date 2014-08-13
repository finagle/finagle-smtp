package com.twitter.finagle.smtp.filter

import com.twitter.finagle.smtp.{DefaultEmail, EmailMessage}
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.util.Future

/**
 * Adds email headers to message body.
 */
object HeadersFilter extends SimpleFilter[EmailMessage, Unit] {
   def apply(msg: EmailMessage, send: Service[EmailMessage, Unit]): Future[Unit] = {
     val fields = msg.headers groupBy { case (k, v) => k } map {
       case (key, values) => key -> values.map(_._2).mkString(",")
     }

     val richMsg = DefaultEmail(msg) setBody msg.body.addHeaders(fields)

     send(richMsg)
   }
 }
