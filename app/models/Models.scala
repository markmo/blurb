package models

import com.github.cleverage.elasticsearch.ScalaHelpers._
import org.bson.types.ObjectId
import com.fasterxml.jackson.annotation.{JsonProperty, JsonCreator}
import service.Repository
import play.api.libs.json._
import com.github.cleverage.elasticsearch.annotations.IndexMapping

/**
 * Created by markmo on 5/07/13.
 */
@IndexMapping(value =
  "{columns: {" +
    "properties: {" +
      "id: {type: \"string\"}," +
      "question: {type: \"string\"}," +
      "answer: {type: \"string\"}," +
      "tags: {type: \"string\", index_name: \"tag\"}" +
    "}}")
case class Blurb @JsonCreator()(@JsonProperty("_id") _id: ObjectId,
                                @JsonProperty("question") question: String,
                                @JsonProperty("answer") answer: String,
                                @JsonProperty("tags") tags: Array[String]) extends Indexable {

  def id = _id.toString
}

object Blurb extends IndexableManager[Blurb] {

  implicit val objectIdFormat: Format[ObjectId] = new Format[ObjectId] {

    def reads(json: JsValue) = {
      json match {
        case jsString: JsString => {
          if (ObjectId.isValid(jsString.value)) JsSuccess(new ObjectId(jsString.value))
          else JsError("Invalid ObjectId")
        }
        case other => JsError("Can't parse JSON path as an ObjectId. JSON content = " + other.toString())
      }
    }

    def writes(oid: ObjectId): JsValue = {
      JsString(oid.toString)
    }
  }

  val indexType = "blurb"

  val reads: Reads[Blurb] = Json.reads[Blurb]

  val writes: Writes[Blurb] = Json.writes[Blurb]

  def tagOptions = Repository.getTags.map(tag => (tag, tag)).toMap
}




//                  createdBy: UserId, createdDate: Date, lastModifiedBy: UserId, lastModifiedDate: Date)
