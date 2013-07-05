package controllers

import play.api.mvc.{Action, Controller}
import play.api.data.Form
import play.api.data.Forms._
import models.Blurb
import service.Repository

/**
 * Created by shine on 5/07/13.
 */
object Blurbs extends Controller {

  val form = Form(
    mapping(
      "id" -> text,
      "question" -> nonEmptyText,
      "answer" -> nonEmptyText
//      "tags" -> ,
//      "createBy" -> ,
//      "createdDate" -> ,
//      "lastModifiedBy" -> ,
//      "lastModifiedDate" ->
    )(Blurb.apply)(Blurb.unapply)
  )


  def list() = Action {
    val blurbs = Repository.getBlurbs
    Ok(views.html.blurbs(blurbs.toSeq))
  }

  def create() = Action {
    Ok(views.html.blurbForm("", form, true))
  }

  def edit(id: String) = Action {
    Ok(views.html.blurbForm("", form, false))
  }

  def update(id: String) = Action { implicit request =>
    Ok(views.html.blurbs(Seq()))
  }
}
