package com.twitter.finagle.smtp.filter

import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.smtp._
import com.twitter.util.Future
import com.twitter.finagle.smtp.reply.Reply

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
