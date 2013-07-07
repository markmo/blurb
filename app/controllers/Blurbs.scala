package controllers

import play.api.mvc.{Action, Controller}
import play.api.data.Form
import play.api.data.Forms._
import models.Blurb
import service.Repository._
import org.bson.types.ObjectId
import service.Repository

/**
 * Created by markmo on 5/07/13.
 */
object Blurbs extends Controller {

  val blurbForm = Form(
    mapping(
      "_id" -> text.transform(
        (stringId: String) => if (stringId.isEmpty) null else new ObjectId(stringId),
        (objectId: ObjectId) => if (objectId == null) "" else objectId.toString
      ),
      "question" -> nonEmptyText,
      "answer" -> nonEmptyText,
      "tags" -> text.transform(
        (csv: String) => csv.split(","),
        (ary: Array[String]) => if (ary == null) "" else ary.mkString(",")
      )
//      "createBy" -> ,
//      "createdDate" -> ,
//      "lastModifiedBy" -> ,
//      "lastModifiedDate" ->
    )(Blurb.apply)(Blurb.unapply)
  )

  def index() = Action {
    Ok(views.html.blurbs(getBlurbs.toSeq))
  }

  def create() = Action {
    Ok(views.html.blurbForm("", blurbForm, Repository.getTags, true))
  }

  def edit(id: String) = Action {
    val blurb = Repository.getBlurb(id)
    Ok(views.html.blurbForm("", blurbForm.fill(blurb), Repository.getTags, false))
  }

  def update = Action { implicit request =>
    blurbForm.bindFromRequest.fold(
      formWithErrors => BadRequest,
      blurb => {
        upsertBlurb(blurb)
        Blurb.index(blurb)
        Ok(views.html.blurbs(getBlurbs.toSeq))
      }
    )
  }
}
