package controllers

import play.api.mvc._
import play.api.libs.ws.WS
import play.api.libs.Jsonp
import play.api.Play
import scala.concurrent.ExecutionContext.Implicits.global
import service.ReactiveMongoRepository._

object Application extends Controller with securesocial.core.SecureSocial {

  val Home = Redirect(routes.Blurbs.index())

  def UhOh(message: String) =
    Redirect(routes.Blurbs.index()).flashing("error" -> message)

  def index = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Home
  }

  def search = //SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Blurbs.SetMongoCollectionsAction { implicit request =>
    Async {
      getDistinctEntityTypes map { entityTypes =>
        Ok(views.html.facetview(entityTypes, request.user))
      }
    }
  }

  def execSearch(callback: String, source: String) = Blurbs.SetMongoCollectionsAction { implicit request =>
    Async {
      //val client = Play.current.configuration.getString("elasticsearch.client").getOrElse("localhost:9200")
      //val client = "ec2-54-252-198-226.ap-southeast-2.compute.amazonaws.com:9200"
      val client = "fqfxuqfj.api.qbox.io:80"
      val index = Play.current.configuration.getString("elasticsearch.index.name").getOrElse("blurb")
      val url = "http://" + client + "/" + index + "/_search"
      //WS.url("http://localhost:9200/blurb/_search").withQueryString(
      WS.url(url).withQueryString(
        "source" -> source
      ).get map { response =>
        Ok(Jsonp(callback, response.json))
      } recover {
        case e =>
          e.printStackTrace()
          BadRequest(e.getMessage)
      }
    }
  }

}
