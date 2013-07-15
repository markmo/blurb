package service

import name.fraser.neil.plaintext.diff_match_patch
import name.fraser.neil.plaintext.diff_match_patch.Diff
import name.fraser.neil.plaintext.diff_match_patch.Operation._
import org.bson.types.ObjectId
import org.joda.time.DateTime
import play.api.libs.json.Json
import play.api.Play.current
import securesocial.core.Identity

// Reactive Mongo plugin, including the JSON-specialized collection
import play.modules.reactivemongo.json.collection.JSONCollection
import play.modules.reactivemongo.ReactiveMongoPlugin

// Reactive Mongo imports
import reactivemongo.api.QueryOpts
import reactivemongo.bson.BSONObjectID
import reactivemongo.core.commands.Count

import scala.collection.JavaConversions._
import scala.concurrent.{Await, Future, ExecutionContext}
import scala.concurrent.duration._

import models.{Blurb, Page, Revision}
import models.Blurb._

/**
 * Created by shine on 15/07/13.
 */
object ReactiveMongoRepository {

  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  def db = ReactiveMongoPlugin.db

  var collectionName = "public_blurbs"

  var historyName = "public_history"

  def collection = db[JSONCollection](collectionName)

  def history = db[JSONCollection](historyName)

  def count: Future[Int] = db.command(Count("blurbs"))

  val pageSize = 10

  def blurbsPage(page: Int = 0, orderBy: String = "lastModifiedDate",
                 orderDirection: Int = -1, filter: String = ".*",
                 pageSize: Int = pageSize) = {
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
        Page(list, page, offset, count)
      }
    }
  }

  def findRevisions(id: String) = {
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
          rest.toList.flatten.reverse :+ first
        } getOrElse Nil
      }
    }
  }

  def getBlurb(id: String) = {
    val objectId = new ObjectId(id)
    collection.find(Json.obj("_id" -> objectId)).one[Blurb]
  }

  def insertBlurb(bound: Blurb, user: Identity) = {
    val revisionDate = DateTime.now()
    getEntities(bound.answer) map { entities =>
      val blurb = bound.copy(
        key = Some(new ObjectId(BSONObjectID.generate.stringify)),
        entities = entities,
        createdBy = Some(user),
        createdDate = Some(revisionDate),
        lastModifiedBy = Some(user),
        lastModifiedDate = Some(revisionDate)
      )
      Blurb.index(blurb)
      collection.insert(blurb)
    }
  }

  def updateBlurb(bound: Blurb, user: Identity) = {
    val revisionDate = DateTime.now()
    collection.find(Json.obj("_id" -> bound.key)).one[Blurb] flatMap { maybe =>
      maybe map { latest =>
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
        collection.save(blurb) map {_ =>
          val rev = Revision(
            Some(new ObjectId(BSONObjectID.generate.stringify)),
            bound.key.get,
            revisionDate,
            latest
          )
          Blurb.index(blurb)
          history.save(rev)
        }
      } getOrElse {
        Future.failed(new Exception("Could not find blurb with id:" + bound.key))
      }
    }
  }

  def deleteBlurb(id: String) = {
    val objectId = new ObjectId(id)
    collection.remove(Json.obj("_id" -> objectId)) map {_ =>
      Blurb.delete(id)
    }
  }

  def updateTag(oldName: String, newName: String) = {
    val modifier = Json.obj(
      "$set" -> Json.obj("tags.$" -> newName)
    )
    collection.update(Json.obj("tags" -> oldName), modifier, multi = true)
  }

  def restoreRevision(id: String) = {
    val objectId = new ObjectId(id)

    history.find(Json.obj("id" -> objectId)).one[Revision] flatMap { maybe =>
      maybe map { rev =>

        collection.find(Json.obj("_id" -> rev.originalId)).one[Blurb] flatMap { maybe =>
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
                lastError
              }
            }

          } getOrElse {
            Future.failed(new Exception("Could not find blurb with id:" + rev.originalId))
          }
        }

      } getOrElse {
        Future.failed(new Exception("Could not find revision with id:" + id))
      }
    }
  }
}
