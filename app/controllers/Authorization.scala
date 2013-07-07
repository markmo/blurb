package controllers

import securesocial.core.{Authorization, Identity}

/**
 * User: markmo
 * Date: 8/07/13
 * Time: 12:10 AM
 */
case class WithDomain(domain: String) extends Authorization {

  def isAuthorized(user: Identity) = user.email.getOrElse("").endsWith(domain)

}
