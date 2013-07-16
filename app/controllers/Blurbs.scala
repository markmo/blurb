package controllers

import play.api.mvc._
import play.api.Play
import securesocial.core.SecuredRequest
import scala.util.matching.Regex

import Application._
import models.Blurb._
import service.ReactiveMongoRepository
import service.ReactiveMongoRepository._

/**
 * Created by markmo on 5/07/13.
 */
object Blurbs extends Controller with securesocial.core.SecureSocial {

  val pageSize = 10

  def SetMongoCollectionsAction(f: SecuredRequest[AnyContent] => Result): Action[AnyContent] = {
    SecuredAction { implicit request =>
      val conf = Play.current.configuration
      val user = request.user
      val Pattern = new Regex(conf.getString("private.domain").getOrElse(".*@datascience\\.co\\.nz"))
      val privateCollectionName = conf.getString("private.collection").getOrElse("private_blurbs")
      val publicCollectionName = conf.getString("public.collection").getOrElse("public_blurbs")
      user.email match {
        case Some(Pattern(_)) =>
          collectionName = privateCollectionName
          historyName = conf.getString("private.history").getOrElse("private_history")
        case _ =>
          collectionName = publicCollectionName
          historyName = conf.getString("public.history").getOrElse("public_history")
      }
      f(request)
    }
  }

  def index(page: Int, orderBy: String, orderDirection: Int, filter: String) =
    //SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    SetMongoCollectionsAction { implicit request =>
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

  def versions(id: String) = //SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    SetMongoCollectionsAction { implicit request =>
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

  def create() = //SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    SetMongoCollectionsAction { implicit request =>
      Async {
        getDistinctTags map { tags =>
          Ok(views.html.blurbForm("", form, tags, isNew = true, request.user))
        } recover {
          case e =>
            e.printStackTrace()
            UhOh(e.getMessage)
        }
      }
    }

  def edit(id: String) = //SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    SetMongoCollectionsAction { implicit request =>
    Async {
      // using for-comprehensions to compose futures
      // (see http://doc.akka.io/docs/akka/2.0.3/scala/futures.html#For_Comprehensions for more information)
      for {
        maybe <- getBlurb(id)
        tags <- getDistinctTags
      } yield {
        maybe map { blurb =>
          Ok(views.html.blurbForm("", form.fill(blurb), tags, isNew = false, request.user))
        } getOrElse {
          UhOh("Could not find blurb with id:" + id)
        }
      }
    }
  }

  def update = //SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    SetMongoCollectionsAction { implicit request =>
    form.bindFromRequest.fold(
      formWithErrors => AsyncResult {
        getDistinctTags map { tags => BadRequest(
          views.html.blurbForm("", formWithErrors, tags, isNew = false, request.user))
        } recover {
          case e =>
            e.printStackTrace()
            UhOh(e.getMessage)
        }
      },
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

  def delete(id: String) = //SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    SetMongoCollectionsAction { implicit request =>
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

  def updateTag = //SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    SetMongoCollectionsAction { implicit request =>
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

  def restore(id: String) = //SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    SetMongoCollectionsAction { implicit request =>
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
