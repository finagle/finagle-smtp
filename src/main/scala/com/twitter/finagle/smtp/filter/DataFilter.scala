package com.twitter.finagle.smtp.filter

import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.finagle.smtp._
import com.twitter.util.Future
import com.twitter.finagle.smtp.reply.Reply
import scala.collection.immutable.IndexedSeq

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

     case Request.MimeData(data) => {
       val cr = "\r".getBytes("US-ASCII")(0)
       val lf = "\n".getBytes("US-ASCII")(0)
       val dot = ".".getBytes("US-ASCII")(0)

       val shieldedBytes = { for ( i <- data.content.indices drop 2 )
                           yield if (data.content(i-2) == cr && data.content(i-1) == lf && data.content(i) == dot) Seq(dot, dot)
                                 else Seq(data.content(i))
                         }.flatten.toArray
       val shieldedMime = MimePart(shieldedBytes, data.headers)

       send(Request.MimeData(shieldedMime))
     }

     case other => send(other)
   }
 }
