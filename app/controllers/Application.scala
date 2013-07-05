package controllers

import play.api.mvc._

object Application extends Controller with securesocial.core.SecureSocial {

  def index = SecuredAction { implicit request =>
    Ok(views.html.index("hello"))
  }

}