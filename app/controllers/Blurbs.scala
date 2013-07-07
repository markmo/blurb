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
object Blurbs extends Controller with securesocial.core.SecureSocial  {

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

  def index() = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Ok(views.html.blurbs(getBlurbs.toSeq, request.user))
  }

  def create() = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Ok(views.html.blurbForm("", blurbForm, Repository.getTags, true, request.user))
  }

  def edit(id: String) = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    val blurb = Repository.getBlurb(id)
    Ok(views.html.blurbForm("", blurbForm.fill(blurb), Repository.getTags, false, request.user))
  }

  def update = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    blurbForm.bindFromRequest.fold(
      formWithErrors => BadRequest,
      blurb => {
        upsertBlurb(blurb)
        Blurb.index(blurb)
        Application.Home
      }
    )
  }
}
