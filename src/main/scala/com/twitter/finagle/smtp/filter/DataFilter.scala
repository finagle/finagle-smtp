package com.twitter.finagle.smtp.filter

import com.twitter.finagle.smtp._
import com.twitter.finagle.smtp.reply.Reply
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.util.Future

/**
 * Duplicates dots in the beginning of each line of email body for transparency
 * (see [[http://tools.ietf.org/search/rfc5321#section-4.5.2]]) and adds a terminating
 * <CRLF>.<CRLF>
 */
object DataFilter extends SimpleFilter[Request, Reply] {
   override def apply(req: Request, send: Service[Request, Reply]): Future[Reply] = req match {
     case Request.TextData(lines) => {
       //duplicate leading dot
       val shieldedLines = for (line <- lines) yield if (line.head == '.') (".." + line.tail) else line
       //add dot at the end
       val lastLine = "."
       val body = shieldedLines ++ Seq(lastLine)
       send(Request.TextData(body))
     }

     case Request.MimeData(data) =>
       //convert to text data to check all lines
       val textReq = Request.TextData(data.getMimeHeaders ++ data.message.split("\r\n"))
       apply(textReq, send)

     case other => send(other)
   }
 }
