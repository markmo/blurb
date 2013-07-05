package service

import com.mongodb.{Mongo, DB}
import org.jongo.{MongoCollection, Jongo}
import models.Blurb
import scala.collection.JavaConversions._

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

  def saveBlurb(blurb: Blurb) = {
    val blurbs: MongoCollection = jongo.getCollection("blurbs")
    blurbs.save(blurb)
  }

  def updateBlurb(blurb: Blurb) = {
    val blurbs: MongoCollection = jongo.getCollection("blurbs")
    if (blurb.id == null) {
      blurbs.insert(blurb)
    } else {
      blurbs.update(blurb.id).merge(blurb)
    }
  }

}
