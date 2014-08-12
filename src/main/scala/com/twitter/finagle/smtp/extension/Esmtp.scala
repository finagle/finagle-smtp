package com.twitter.finagle.smtp.extension

import com.twitter.finagle.smtp.{Reply, Request}
import com.twitter.finagle.{Filter, Service}

//adds extensions to SMTP clients
case class Esmtp(extensions: SmtpExtensions) {
  // filters for supported SMTP extensions
  lazy val supportedExtFilters = for {
    extension <- extensions.supported
    if GetExtensionFilter.forSupported.isDefinedAt(extension)
  } yield GetExtensionFilter forSupported extension

  GetExtensionFilter.forUnsupportedExtensions foreach println

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
