package io.github.finagle.smtp.extension

import com.twitter.finagle.Stack.Role
import com.twitter.finagle._
import com.twitter.util.Future
import io.github.finagle.smtp.extension.Extensions.ExtensionList
import io.github.finagle.smtp.filter.{DataFilter, OkToExtFilter}
import io.github.finagle.smtp.{ExtensionsReply, Reply, Request}

/**
 * Decorates SMTP client with extension-related behavior.
 */
case class Esmtp(extensions: ExtensionList) {
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
  object Stackable extends Stack.Module1[ExtensionList, ServiceFactory[Request, Reply]] {
    def make(p1: ExtensionList, next: ServiceFactory[Request, Reply]) = {
      new ServiceFactoryProxy[Request, Reply](next){
        override def apply(conn: ClientConnection) = {
          self.apply(conn) flatMap { service =>
            // Send EHLO and get extensions supported by server
            greet(service, p1) map { extensions =>
              // Shield message body
              DataFilter andThen
                // Transform OK replies to Extensions replies
                OkToExtFilter andThen
                // Make behaviour changes according to supported extensions
                Esmtp(extensions).extend(service)
            }}}}
    }

    val role = Role("EsmtpGreeter")
    val description = "Greets SMTP server and receives the list of extensions supported by it"
  }

  /**
   * Sends a simple HELO greeting.
   *
   * @return Future containing empty
   *         [[io.github.finagle.smtp.extension.Extensions.ExtensionList]] object
   */
  def simpleGreet(service: Service[Request, Reply]): Future[ExtensionList] =
    service(Request.SimpleHello) map { _ => ExtensionList()}

  /**
   * Sends an EHLO request and processes its reply. If EHLO fails,
   * a HELO request (indicating that extensions should not be supported) is sent.
   *
   * @param service The service used to send requests and receive replie
<<<<<<< HEAD
   * @param clientExtensions The sequence of extensios that can be supported by client
   * @return Future containing extensions supported by server
   */
  def greet(
    service: Service[Request, Reply],
    clientExtensions: ExtensionList): Future[ExtensionList] = clientExtensions.list match {
    // If no extensions are supported by client, just send HELO
    case Seq() => simpleGreet(service)
    case _ =>
      val extService = OkToExtFilter andThen service
      extService(Request.Hello) map {

        // If everything is all right, add available extensions
        case ext: ExtensionsReply => {
          val clientSupportedKeywords = clientExtensions.list map { _.keyword }
          val agreement = for {
            line <- ext.lines().tail map { _.toUpperCase split " " }
            if clientSupportedKeywords contains line.head
          } yield Extension(line.head, line.tail)

          // BINARYMIME can not be supported without CHUNKING
          val hasBinary = agreement contains Extension.BinaryMime
          val hasChunking = agreement contains Extension.Chunking

          val bothSupport = agreement filterNot {
            _.keyword == ExtensionKeywords.BINARYMIME && hasBinary && !hasChunking
          }

          Extensions.ExtensionList(bothSupport:_*)
        }

        // Else no extensions are supported and HELO is sent
      } rescue {
        case _ => simpleGreet(service)
      }
  }
}
