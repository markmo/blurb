package service

import com.mongodb.{Mongo, DB}
import org.jongo.{MongoCollection, Jongo}
import models.Blurb
import scala.collection.JavaConversions._
import org.bson.types.ObjectId

/**
 * Created by markmo on 5/07/13.
 */
object Repository {

  val db: DB = new Mongo().getDB("blurb")
  val jongo: Jongo = new Jongo(db)

  def getBlurbs: Iterable[Blurb] = {
    val blurbs: MongoCollection = jongo.getCollection("blurbs")
    blurbs.find("{}").as(classOf[Blurb])
  }

  def getBlurb(id: String) = {
    val blurbs: MongoCollection = jongo.getCollection("blurbs")
    blurbs.findOne(new ObjectId(id)).as(classOf[Blurb])
  }

  def upsertBlurb(blurb: Blurb) = {
    val blurbs: MongoCollection = jongo.getCollection("blurbs")
    blurb._id match {
      case null => blurbs.insert(blurb)
      case _ => blurbs.save(blurb)
    }
  }

  def getTags = {
    val blurbs: MongoCollection = jongo.getCollection("blurbs")
    blurbs.distinct("tags").as(classOf[String]).toList
  }

}
