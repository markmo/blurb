package service

import com.mongodb.{Mongo, DB}
import org.bson.types.ObjectId
import org.jongo.{MongoCollection, Jongo}
import scala.collection.JavaConversions._

import models.{Page, Blurb}

/**
 * Created by markmo on 5/07/13.
 */
object Repository {

  val db: DB = new Mongo().getDB("blurb")
  val jongo: Jongo = new Jongo(db)
  val blurbs: MongoCollection = jongo.getCollection("blurbs")

  def getBlurbs: Iterable[Blurb] = blurbs.find().as(classOf[Blurb])

  def pageBlurbs(page: Int = 0, pageSize: Int = 10,
                 orderBy: String = "lastModifiedDate", orderDirection: Int = -1,
                 filter: String = ".*"): Page[Blurb] = {
    val offset = pageSize * page
    val count = blurbs.count()
    val list =
      blurbs.find("{$or: [{question: {$regex: #}}, {answer: {$regex: #}}]}", filter, filter)
        .skip(offset).limit(pageSize)
        .sort(s"{$orderBy: $orderDirection}")
        .as(classOf[Blurb]).toSeq
    Page(list, page, offset, count)
  }

  def getBlurb(oid: ObjectId) = blurbs.findOne(oid).as(classOf[Blurb])

  def getBlurb(id: String): Blurb = getBlurb(new ObjectId(id))

  def upsertBlurb(blurb: Blurb) = blurb.key match {
      case None => blurbs.insert(blurb)
      case _ => blurbs.save(blurb)
    }

  def getTags = blurbs.distinct("tags").as(classOf[String]).toList

}
