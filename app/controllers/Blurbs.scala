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

import Application._
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
      val query =
        Json.obj(
          "$or" -> Json.arr(
            Json.obj("question" -> Json.obj("$regex" -> filter)),
            Json.obj("answer" -> Json.obj("$regex" -> filter))
          )
        )
      val cursor = collection
        .find(query)
        .sort(Json.obj(orderBy -> orderDirection))
        .options(QueryOpts(skipN = offset, batchSizeN = pageSize))
        .cursor[Blurb]

      cursor.toList flatMap { list =>
        count map { count =>
          Ok(views.html.blurbs(
            Page(list, page, offset, count),orderBy, orderDirection, filter, request.user)
          )
        }
      } recover {
        case e =>
          e.printStackTrace()
          UhOh(e.getMessage)
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
        .cursor[Revision]

      cursor.toList flatMap { revisions =>
        for {
          maybe <- future
        } yield {
          maybe map { latest =>
            val rest = revisions.sliding(2).map { w =>
              w match {
                case List(r1, r2) =>
                  val prev = r1.state
                  val next = r2.state
                  Some(
                    (
                      r2,
                      differ.diff_main(prev.question, next.question).toList,
                      differ.diff_main(prev.answer, next.answer).toList,
                      prev.tags.toList.diff(next.tags)
                    )
                  )
                case _ => None
              }
            }
            val head = revisions.head
            val first = (
              head,
              List(new Diff(EQUAL, head.state.question)),
              List(new Diff(EQUAL, head.state.answer)),
              Nil
            )
            val list = rest.toList.flatten.reverse :+ first
            Ok(views.html.versions(list, request.user))
          } getOrElse {
            UhOh("Could not find any revisions")
          }
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
      val objectId = new ObjectId(id)
      val future = collection.find(Json.obj("_id" -> objectId)).one[Blurb]

      // using for-comprehensions to compose futures
      // (see http://doc.akka.io/docs/akka/2.0.3/scala/futures.html#For_Comprehensions for more information)
      for {
        maybe <- future
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
    val user = request.user
    val revisionDate = DateTime.now()
    form.bindFromRequest.fold(
      formWithErrors => BadRequest(
        views.html.blurbForm("", formWithErrors, Repository.getTags, isNew = false, request.user)
      ),
      bound => AsyncResult {
        bound.key match {

          // Insert
          case None => {
            getEntities(bound.answer) flatMap { entities =>
              val blurb = bound.copy(
                key = Some(new ObjectId(BSONObjectID.generate.stringify)),
                entities = entities,
                createdBy = Some(user),
                createdDate = Some(revisionDate),
                lastModifiedBy = Some(user),
                lastModifiedDate = Some(revisionDate)
              )
              collection.insert(blurb) map {_ =>
                Blurb.index(blurb)
                Home.flashing("success" -> "Successfully created new blurb")
              }
            }
          }

          // Update
          case Some(id) =>
            collection
              .find(Json.obj("_id" -> id))
              .one[Blurb] flatMap { maybe =>
                maybe match {
                  case Some(latest) => {
                    val blurb = if (latest.answer != bound.answer) {
                      val entities = Await.result(getEntities(bound.answer), 15.seconds)
                      bound.copy(
                        entities = entities,
                        createdBy = latest.createdBy,
                        createdDate = latest.createdDate,
                        lastModifiedBy = Some(user),
                        lastModifiedDate = Some(revisionDate),
                        version = latest.version + 1
                      )
                    } else {
                      bound.copy(
                        entities = latest.entities,
                        createdBy = latest.createdBy,
                        createdDate = latest.createdDate,
                        lastModifiedBy = Some(user),
                        lastModifiedDate = Some(revisionDate),
                        version = latest.version + 1
                      )
                    }
                    collection.save(blurb) flatMap {_ =>
                      val rev = Revision(
                        Some(new ObjectId(BSONObjectID.generate.stringify)),
                        id,
                        revisionDate,
                        latest
                      )
                      Blurb.index(blurb)
                      history.save(rev) map {_ =>
                        Home.flashing("success" -> "Successfully updated blurb")
                      }
                    }
                  }
                  case _ => Future(UhOh("Could not find blurb with id:" + id))
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
      val modifier = Json.obj(
        "$set" -> Json.obj("tags.$" -> newName)
      )
      collection.update(Json.obj("tags" -> oldName), modifier, multi = true) map {_ =>
        Home.flashing("success" -> s"Successfully changed tag name from $oldName -> $newName")
      }
    }
  }

  def restore(id: String) = SecuredAction(WithDomain("shinetech.com")) { implicit request =>
    Async {
      val objectId = new ObjectId(id)

      history
        .find(Json.obj("id" -> objectId))
        .one[Revision] flatMap { maybe =>
          maybe map { rev =>

            collection
              .find(Json.obj("_id" -> rev.originalId))
              .one[Blurb] flatMap { maybe =>
                maybe map { blurb =>
                  val v = rev.state

                  getEntities(v.answer) flatMap { entities =>
                    val newBlurb = blurb.copy(
                      question = v.question,
                      answer = v.answer,
                      tags = v.tags,
                      entities = entities,
                      lastModifiedBy = v.lastModifiedBy,
                      lastModifiedDate = v.lastModifiedDate
                    )
                    val revision = Revision(
                      Some(new ObjectId(BSONObjectID.generate.stringify)),
                      blurb.key.get,
                      DateTime.now,
                      blurb
                    )
                    for {
                      revisionLastError <- history.save(revision)
                      lastError <- collection.save(newBlurb)
                    } yield {
                      Home.flashing("success" -> s"Successfully restored revision($id)")
                    }
                  }

                } getOrElse {
                  Future(UhOh("Could not find blurb with id:" + rev.originalId))
                }
            }

          } getOrElse {
            Future(UhOh("Could not find revision with id:" + id))
          }
      }
    }
  }

}
