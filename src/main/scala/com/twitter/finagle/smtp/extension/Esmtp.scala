package com.twitter.finagle.smtp.extension

import com.twitter.finagle.smtp.filter.OkToExtFilter
import com.twitter.finagle.smtp.{ExtensionsReply, Reply, Request}
import com.twitter.finagle.{Filter, Service}
import com.twitter.util.Future

//adds extensions to SMTP clients
case class Esmtp(extensions: SmtpExtensions) {
  // filters for supported SMTP extensions
  lazy val supportedExtFilters = for {
    extension <- extensions.supported
    if GetExtensionFilter.forSupported.isDefinedAt(extension)
  } yield GetExtensionFilter forSupported extension

  // filters applied when some SMTP extensions are not supported
  lazy val unsupportedExtFilters = for {
    (extension, filter) <- GetExtensionFilter.forUnsupportedExtensions
    if !(extensions.supported.map(_.keyword) contains extension)
  } yield filter

  lazy val extFilters = (supportedExtFilters ++ unsupportedExtFilters)
                         .reduceRight[Filter[Request, Reply, Request, Reply]] { _ andThen _}

  def extend(client: Service[Request, Reply]): Service[Request, Reply] = {
    extFilters andThen
    client
  }
}

object Esmtp {
  /**
   * Sends an EHLO request and processes its reply
   *
   * @param service The service used to send requests and receive replies
   * @return Future containing extensions supported by server
   */
  def greet(
    service: Service[Request, Reply],
    clientSupported: Seq[String]): Future[SmtpExtensions] = clientSupported match {
    case Seq() => service(Request.SimpleHello) map { _ => SmtpExtensions()}
    case _ =>
      val extService = OkToExtFilter andThen service
      extService(Request.Hello) map {
        //if everything is all right, add available extensions
        case ext: ExtensionsReply => {
          val lines = ext.lines
          val supported = for {
            line <- lines.tail map { _.toUpperCase split " " }
            if clientSupported contains line.head
          } yield Extension(line.head, line.tail)

          // BINARYMIME can not be supported without CHUNKING
          val binary = supported exists { _.keyword == SmtpExtensions.BINARYMIME }
          val chunking = supported exists { _.keyword == SmtpExtensions.CHUNKING }

          val bothSupport = if (binary && !chunking) {
            supported filterNot {
              case Extension(SmtpExtensions.BINARYMIME, _) => true
              case _ => false
            }
          } else supported

          SmtpExtensions(bothSupport:_*)
        }
        // else no extensions are supported and HELO is sent
      } rescue {
        case _ => service(Request.SimpleHello) map { _ => SmtpExtensions() }
      }
  }
}
