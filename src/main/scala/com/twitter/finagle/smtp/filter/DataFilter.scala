package com.twitter.finagle.smtp.filter

import com.twitter.finagle.smtp._
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.util.Future

object DataFilter extends SimpleFilter[Request, Reply] {
   override def apply(req: Request, send: Service[Request, Reply]): Future[Reply] = req match {
     case Request.TextData(lines, _) => {
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

       val shieldedBytes: Seq[Byte] = { for ( i <- data.content.indices )
                           yield if (i >= 2 && data.content.indexOfSlice(Seq(cr, lf, dot), i-2) == i-2 ||
                                     i == 0 && data.content(i) == dot
                                    ) Seq(dot, dot)
                                 else Seq(data.content(i))
                         }.flatten
       //add last dot; the bytes are copied as-is, so we need to add <CRLF> after the dot too
       val withLastDot = shieldedBytes ++ Seq(cr, lf, dot, cr, lf)
       val shieldedMime = MimePart(withLastDot.toArray[Byte], data.headers)

       send(Request.MimeData(shieldedMime))
     }

     case other => send(other)
   }
 }
