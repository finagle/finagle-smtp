package com.twitter.finagle.smtp.extension

import com.twitter.finagle.smtp.Request
import com.twitter.finagle.smtp.filter.DataFilter
import com.twitter.finagle.smtp.reply.Reply
import com.twitter.finagle.{Filter, ServiceFactory}

/**
 * Created by lenovo on 31.07.2014.
 */
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

  def extend(client: ServiceFactory[Request, Reply]): ServiceFactory[Request, Reply] = {
    DataFilter andThen
    OkToExtFilter andThen
    extFilters andThen
    client
  }
}
