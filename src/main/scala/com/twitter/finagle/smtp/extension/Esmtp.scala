package com.twitter.finagle.smtp.extension

import com.twitter.finagle.{Filter, Service}
import com.twitter.finagle.smtp.{ExtensionsReply, Reply, Request}
import com.twitter.finagle.smtp.filter.OkToExtFilter
import com.twitter.util.Future

/**
 * Decorates SMTP client with extension-related behavior.
 */
case class Esmtp(extensions: Extensions) {
  // Filters for supported SMTP extensions
  lazy val supportedExtFilters = for {
    extension <- extensions.list
    if GetExtensionFilter.forSupported.isDefinedAt(extension)
  } yield GetExtensionFilter forSupported extension

  // Filters ensuring unsupported extensions are not used
  lazy val unsupportedExtFilters = {
    val supportedKeywords = extensions.list.map(_.keyword)
    for {
      (extension, filter) <- GetExtensionFilter.forUnsupportedExtensions
      if !(supportedKeywords contains extension.keyword)
    } yield filter
  }

  // Filters above combined into one
  lazy val extFilters = (supportedExtFilters ++ unsupportedExtFilters)
                         .reduceRight[Filter[Request, Reply, Request, Reply]] { _ andThen _ }

  /**
   * Decorates given service with behavior according to supported ''extensions''.
   *
   * For each extension there are generally two filters: one for the
   * case when it is supported, and the other for the case when it's not.
   * Based on the given extension list, a proper filter for every known extension
   * is chosen, if exists, and added to the service.
   * */
  def extend(client: Service[Request, Reply]): Service[Request, Reply] = {
    extFilters andThen
    client
  }
}

object Esmtp {
  /**
   * Sends an EHLO request and processes its reply. If EHLO fails,
   * a HELO request (indicating that extensions should not be supported) is sent.
   *
   * @param service The service used to send requests and receive replies
   * @param clientExtensions The sequence of extensios that can be supported by client
   * @return Future containing extensions supported by server
   */
  def greet(
    service: Service[Request, Reply],
    clientExtensions: Extensions): Future[Extensions] = clientExtensions.list match {
    // If no extensions are supported by client, just send HELO
    case Seq() => service(Request.SimpleHello) map { _ => Extensions()}
    case _ =>
      val extService = OkToExtFilter andThen service
      extService(Request.Hello) map {

        // If everything is all right, add available extensions
        case ext: ExtensionsReply => {
          val lines = ext.lines()
          val clientSupportedKeywords = clientExtensions.list map { _.keyword }
          val supported = for {
            line <- lines.tail map { _.toUpperCase split " " }
            if clientSupportedKeywords contains line.head
          } yield Extension(line.head, line.tail)

          // BINARYMIME can not be supported without CHUNKING
          val binary = supported contains Extensions.BinaryMime
          val chunking = supported contains Extensions.Chunking

          val bothSupport = supported filterNot {
            _.keyword == ExtensionKeywords.BINARYMIME && binary && !chunking
          }

          Extensions(bothSupport:_*)
        }

        // Else no extensions are supported and HELO is sent
      } rescue {
        case _ => service(Request.SimpleHello) map { _ => Extensions() }
      }
  }
}
