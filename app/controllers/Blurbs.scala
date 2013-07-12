package controllers

import org.bson.types.ObjectId
import org.joda.time.DateTime
import play.api.libs.json.Json
import play.api.mvc._

// Reactive Mongo imports
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.api._
import reactivemongo.bson._
import reactivemongo.core.commands.Count

// Reactive Mongo plugin, including the JSON-specialized collection
import play.modules.reactivemongo.json.collection.JSONCollection
import play.modules.reactivemongo.MongoController

import scala.concurrent.Future

import models.{Page, Blurb}
import models.Blurb._
import service.Repository

/**
 * Created by markmo on 5/07/13.
 */
object Blurbs extends Controller with securesocial.core.SecureSocial with MongoController {

  def collection = db[JSONCollection]("blurbs")

  def history = db[JSONCollection]("history")

  def count: Future[Int] = db.command(Count("blurbs"))

  val pageSize = 10

  def index(page: Int, orderBy: String, orderDirection: Int, filter: String) = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Async {
      val offset = pageSize * page
      val query = Json.obj("$or" -> Json.arr(
          Json.obj("question" -> Json.obj("$regex" -> filter)),
          Json.obj("answer" -> Json.obj("$regex" -> filter))))
//      val query = BSONDocument(
//        "$orderby" -> BSONDocument(orderBy -> BSONInteger(orderDirection)),
//        "$query" -> BSONDocument("$or" -> BSONArray(
//          BSONDocument("question" -> BSONDocument("$regex" -> BSONString(filter))),
//          BSONDocument("answer" -> BSONDocument("$regex" -> BSONString(filter)))
//        )))
      val cursor = collection
        .find(query)
        .sort(Json.obj(orderBy -> orderDirection))
        .options(QueryOpts(skipN = offset, batchSizeN = pageSize))
        .cursor[Blurb]
      cursor.toList.flatMap { list =>
        count.map { count =>
          Ok(views.html.blurbs(Page(list, page, offset, count), orderBy, orderDirection, filter, request.user))
        }
      }.recover {
        case e =>
          e.printStackTrace()
          BadRequest(e.getMessage())
      }
    }
  }

  def create() = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Ok(views.html.blurbForm("", form, Repository.getTags, true, request.user))
  }

  def edit(id: String) = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Async {
      val objectId = new ObjectId(id)
      val futureBlurb = collection.find(Json.obj("_id" -> objectId)).one[Blurb]
      //val objectId = new BSONObjectID(id)
      //val futureBlurb = collection.find(BSONDocument("_id" -> objectId)).one[Blurb]

      // using for-comprehensions to compose futures
      // (see http://doc.akka.io/docs/akka/2.0.3/scala/futures.html#For_Comprehensions for more information)
      for {
        maybeBlurb <- futureBlurb
      } yield {
        maybeBlurb.map { blurb =>
          Ok(views.html.blurbForm("", form.fill(blurb), Repository.getTags, false, request.user))
        }.getOrElse(NotFound)
      }
    }
  }

  def update = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    val user = request.user
    form.bindFromRequest.fold(
      formWithErrors => BadRequest,
      boundBlurb => AsyncResult {
        //val futureResult =
        boundBlurb.key match {
          case None => {
            // Insert
            val newBlurb = boundBlurb.copy(
              key = Some(new ObjectId(BSONObjectID.generate.stringify)),
              createdBy = Some(user),
              createdDate = Some(DateTime.now()),
              lastModifiedBy = Some(user),
              lastModifiedDate = Some(DateTime.now())
            )
            collection.insert(newBlurb).map(_ => {
              Blurb.index(newBlurb)
              Application.Home
            })
          }
          case Some(id) =>
            // Update
            collection.find(Json.obj("_id" -> id)).one[Blurb] flatMap { result =>
            //collection.find(BSONDocument("_id" -> id)).one[Blurb] map { result =>
              result match {
                case Some(oldBlurb) => {
                  val newBlurb = boundBlurb.copy(
                    createdBy = oldBlurb.createdBy,
                    createdDate = oldBlurb.createdDate,
                    lastModifiedBy = Some(user),
                    lastModifiedDate = Some(DateTime.now())
                  )
                  collection.save(newBlurb).map(_ => {
                    Blurb.index(newBlurb)
                    Application.Home
                  })
                }
                case _ => Future(NotFound)
              }
            }
        }
//        futureResult.map(_ => {
//          Application.Home
//        })
      }
    )
  }

  def delete(id: String) = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Async {
      val objectId = new ObjectId(id)
      collection.remove(Json.obj("_id" -> objectId)).map(_ =>
        Application.Home
      ).recover {
        case _ => InternalServerError
      }
    }
  }

}
