package controllers

import name.fraser.neil.plaintext.diff_match_patch
import name.fraser.neil.plaintext.diff_match_patch.Diff
import name.fraser.neil.plaintext.diff_match_patch.Operation._
import org.bson.types.ObjectId
import org.joda.time.DateTime
import play.api.libs.json.Json
import play.api.mvc._

// Reactive Mongo imports
import reactivemongo.api._
import reactivemongo.bson._
import reactivemongo.core.commands.Count

// Reactive Mongo plugin, including the JSON-specialized collection
import play.modules.reactivemongo.json.collection.JSONCollection
import play.modules.reactivemongo.MongoController

import scala.collection.JavaConversions._
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

import models._
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

  def index(page: Int, orderBy: String, orderDirection: Int, filter: String) =
    SecuredAction(WithDomain("shinetech.com")) { implicit request =>
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

      cursor.toList flatMap { list =>
        count map { count =>
          Ok(views.html.blurbs(Page(list, page, offset, count), orderBy, orderDirection, filter, request.user))
        }
      } recover {
        case e =>
          e.printStackTrace()
          BadRequest(e.getMessage)
      }
    }
  }

  def versions(id: String) = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Async {
      val objectId = new ObjectId(id)
      val differ = new diff_match_patch()
      val future = collection.find(Json.obj("_id" -> objectId)).one[Blurb]
      val cursor = history
        .find(Json.obj("originalId" -> objectId))
        .sort(Json.obj("revisionDate" -> 1))
        .cursor[OldBlurb]

      cursor.toList flatMap { versions =>
        for {
          maybe <- future
        } yield {
          maybe map { currentBlurb =>
//            val latest = OldBlurb(
//              id = None,
//              originalId = currentBlurb.key.get,
//              revisionDate = DateTime.now(),
//              changes = currentBlurb
//            )
//            val list = (versions :+ latest).sliding(2).map { w =>
            val rest = versions.sliding(2).map { w =>
              w match {
                case List(v1, v2) =>
                  val prev = v1.changes
                  val next = v2.changes
                  Some(
                    (
                      v2,
                      differ.diff_main(prev.question, next.question).toList,
                      differ.diff_main(prev.answer, next.answer).toList,
                      prev.tags.toList.diff(next.tags)
                    )
                  )
                case _ => None
              }
            }
            val first = (
              versions.head,
              List(new Diff(EQUAL, versions.head.changes.question)),
              List(new Diff(EQUAL, versions.head.changes.answer)),
              List()
            )
            val list = rest.toList.flatten.reverse :+ first
            Ok(views.html.versions(list, request.user))
          } getOrElse {
            NotFound
          }
        }
      } recover {
        case e =>
          e.printStackTrace()
          BadRequest(e.getMessage)
      }
    }
  }

  def create() = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Ok(views.html.blurbForm("", form, Repository.getTags, isNew = true, request.user))
  }

  def edit(id: String) = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Async {
      val objectId = new ObjectId(id)
      val future = collection.find(Json.obj("_id" -> objectId)).one[Blurb]
      //val objectId = new BSONObjectID(id)
      //val future = collection.find(BSONDocument("_id" -> objectId)).one[Blurb]

      // using for-comprehensions to compose futures
      // (see http://doc.akka.io/docs/akka/2.0.3/scala/futures.html#For_Comprehensions for more information)
      for {
        maybe <- future
      } yield {
        maybe map { blurb =>
          Ok(views.html.blurbForm("", form.fill(blurb), Repository.getTags, isNew = false, request.user))
        } getOrElse {
          NotFound
        }
      }
    }
  }

  def update = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    val user = request.user
    val revisionDate = DateTime.now()
    form.bindFromRequest.fold(
      formWithErrors => BadRequest,
      boundBlurb => AsyncResult {
        boundBlurb.key match {
          case None => {
            // Insert
            getEntities(boundBlurb.answer) flatMap { entities =>
              val newBlurb = boundBlurb.copy(
                key = Some(new ObjectId(BSONObjectID.generate.stringify)),
                entities = entities,
                createdBy = Some(user),
                createdDate = Some(revisionDate),
                lastModifiedBy = Some(user),
                lastModifiedDate = Some(revisionDate)
              )
              collection.insert(newBlurb) map {_ =>
                Blurb.index(newBlurb)
                Application.Home
              }
            }
          }
          case Some(id) =>
            // Update
            collection.find(Json.obj("_id" -> id)).one[Blurb] flatMap { result =>
            //collection.find(BSONDocument("_id" -> id)).one[Blurb] map { result =>
              result match {
                case Some(current) => {
                  val newBlurb = if (current.answer != boundBlurb.answer) {
                    val entities = Await.result(getEntities(boundBlurb.answer), 15.seconds)
                    boundBlurb.copy(
                      entities = entities,
                      createdBy = current.createdBy,
                      createdDate = current.createdDate,
                      lastModifiedBy = Some(user),
                      lastModifiedDate = Some(revisionDate),
                      version = current.version + 1
                    )
                  } else {
                    boundBlurb.copy(
                      entities = current.entities,
                      createdBy = current.createdBy,
                      createdDate = current.createdDate,
                      lastModifiedBy = Some(user),
                      lastModifiedDate = Some(revisionDate),
                      version = current.version + 1
                    )
                  }
                  collection.save(newBlurb) flatMap {_ =>
                    // determine changes (reflection?)
                    // they're not really changes
//                    val changes = BlurbChanges(
//                      if (current.question != newBlurb.question) Some(current.question) else None,
//                      if (current.answer != newBlurb.answer) Some(current.answer) else None,
//                      if (current.tags.diff(newBlurb.tags).isEmpty) None else Some(current.tags),
//                      current.lastModifiedBy.get,
//                      current.lastModifiedDate.get,
//                      current.version
//                    )
                    val oldBlurb = OldBlurb(
                      Some(new ObjectId(BSONObjectID.generate.stringify)),
                      id,
                      revisionDate,
                      current
                    )
                    Blurb.index(newBlurb)
                    history.save(oldBlurb) map {_ =>
                      Application.Home
                    }
                  }
                }
                case _ => Future(NotFound)
              }
            }
        }
      }
    )
  }

  def delete(id: String) = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Async {
      val objectId = new ObjectId(id)
      collection.remove(Json.obj("_id" -> objectId)) map {_ =>
        Blurb.delete(id)
        Application.Home
      } recover {
        case _ => InternalServerError
      }
    }
  }

  def updateTag = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Async {
      val params = request.body.asFormUrlEncoded.get
      val oldName = params("oldName").head
      val newName = params("newName").head
      val modifier = Json.obj(
        "$set" -> Json.obj("tags.$" -> newName)
      )
      collection.update(Json.obj("tags" -> oldName), modifier, multi = true) map {_ =>
        Application.Home
      }
    }
  }

  def restore(id: String) = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Async {
      val objectId = new ObjectId(id)

      history
        .find(Json.obj("id" -> objectId))
        .one[OldBlurb] flatMap { maybe =>
          maybe map { oldBlurb =>

            collection
              .find(Json.obj("_id" -> oldBlurb.originalId))
              .one[Blurb] flatMap { maybe =>
                maybe map { blurb =>
                  val v = oldBlurb.changes

                  getEntities(v.answer) flatMap { entities =>
                    val newBlurb = blurb.copy(
                      question = v.question,
                      answer = v.answer,
                      tags = v.tags,
                      entities = entities,
                      lastModifiedBy = v.lastModifiedBy,
                      lastModifiedDate = v.lastModifiedDate
                    )
                    val newVersion = OldBlurb(
                      Some(new ObjectId(BSONObjectID.generate.stringify)),
                      blurb.key.get,
                      DateTime.now,
                      blurb
                    )
                    for {
                      versionLastError <- history.save(newVersion)
                      lastError <- collection.save(newBlurb)
                    } yield {
                      Application.Home
                    }
                  }

                } getOrElse {
                  Future(NotFound)
                }
            }

          } getOrElse {
            Future(NotFound)
          }
      }
    }
  }

}
