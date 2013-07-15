package controllers

import play.api.mvc._

import Application._
import models.Blurb._
import service.{ReactiveMongoRepository, Repository}
import service.ReactiveMongoRepository._

/**
 * Created by markmo on 5/07/13.
 */
object Blurbs extends Controller with securesocial.core.SecureSocial {

  val pageSize = 10

  def index(page: Int, orderBy: String, orderDirection: Int, filter: String) =
    SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Async {
      blurbsPage(page, orderBy, orderDirection, filter, pageSize) map { page =>
        Ok(views.html.blurbs(page, orderBy, orderDirection, filter, request.user))
      } recover {
        case e =>
          e.printStackTrace()
          UhOh(e.getMessage)
      }
    }
  }

  def versions(id: String) = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Async {
      findRevisions(id) map { revisions =>
        revisions match {
          case Nil => UhOh("Could not find any revisions")
          case _ => Ok(views.html.versions(revisions, request.user))
        }
      } recover {
        case e =>
          e.printStackTrace()
          UhOh(e.getMessage)
      }
    }
  }

  def create() = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Ok(views.html.blurbForm("", form, Repository.getTags, isNew = true, request.user))
  }

  def edit(id: String) = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Async {
      // using for-comprehensions to compose futures
      // (see http://doc.akka.io/docs/akka/2.0.3/scala/futures.html#For_Comprehensions for more information)
      for {
        maybe <- getBlurb(id)
      } yield {
        maybe map { blurb =>
          Ok(views.html.blurbForm("", form.fill(blurb), Repository.getTags, isNew = false, request.user))
        } getOrElse {
          UhOh("Could not find blurb with id:" + id)
        }
      }
    }
  }

  def update = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    form.bindFromRequest.fold(
      formWithErrors => BadRequest(
        views.html.blurbForm("", formWithErrors, Repository.getTags, isNew = false, request.user)
      ),
      bound => AsyncResult {
        bound.key match {

          // Insert
          case None =>
            insertBlurb(bound, request.user) map {_ =>
              Home.flashing("success" -> "Successfully created new blurb")
            } recover {
              case e =>
                e.printStackTrace()
                UhOh(e.getMessage)
            }

          // Update
          case Some(id) =>
            updateBlurb(bound, request.user) map {_ =>
              Home.flashing("success" -> "Successfully updated blurb")
            } recover {
              case e =>
                e.printStackTrace()
                UhOh(e.getMessage)
            }
        }
      }
    )
  }

  def delete(id: String) = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Async {
      deleteBlurb(id) map {_ =>
        Home.flashing("success" -> s"Blurb with id($id) has been deleted")
      } recover {
        case e =>
          e.printStackTrace()
          UhOh(e.getMessage)
      }
    }
  }

  def updateTag = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Async {
      val params = request.body.asFormUrlEncoded.get
      val oldName = params("oldName").head
      val newName = params("newName").head
      ReactiveMongoRepository.updateTag(oldName, newName) map {_ =>
        Home.flashing("success" -> s"Successfully changed tag name from $oldName -> $newName")
      } recover {
        case e =>
          e.printStackTrace()
          UhOh(e.getMessage)
      }
    }
  }

  def restore(id: String) = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Async {
      restoreRevision(id) map {_ =>
        Home.flashing("success" -> s"Successfully restored revision($id)")
      } recover {
        case e =>
          e.printStackTrace()
          UhOh(e.getMessage)
      }
    }
  }
}
