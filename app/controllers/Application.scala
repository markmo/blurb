package controllers

import play.api.mvc._
import service.Repository

object Application extends Controller with securesocial.core.SecureSocial {

  val Home = Redirect(routes.Blurbs.index())

  def UhOh(message: String) =
    Redirect(routes.Blurbs.index()).flashing("error" -> message)

  def index = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Home
  }

  def search = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Ok(views.html.facetview(Repository.getEntities, request.user))
  }

}
