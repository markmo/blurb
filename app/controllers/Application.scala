package controllers

import play.api.mvc._

object Application extends Controller with securesocial.core.SecureSocial {

  val Home = Redirect(routes.Blurbs.index())

  def index = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Home
  }

  def search = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Ok(views.html.facetview(request.user))
  }

}
