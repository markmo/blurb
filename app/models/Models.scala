package models

import com.github.cleverage.elasticsearch.ScalaHelpers._
import org.bson.types.ObjectId
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.format.Formats._
import play.api.data.validation.Constraints._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.ws.WS
import scala.concurrent.{ExecutionContext, Future}
import securesocial.core._

/**
 * Created by markmo on 5/07/13.
 */
case class Blurb(key: Option[ObjectId],
                 question: String,
                 answer: String,
                 tags: Array[String],
                 entities: Option[Array[SemanticEntity]],
                 createdBy: Option[Identity],
                 createdDate: Option[DateTime],
                 lastModifiedBy: Option[Identity],
                 lastModifiedDate: Option[DateTime],
                 version: Int = 1) extends Indexable {

  def id = key.map(_.toString).getOrElse("")
}

case class SemanticEntity(entityType: String,
                          relevance: Double,
                          count: Int,
                          text: String)

case class Revision(id: Option[ObjectId],
                    originalId: ObjectId,
                    revisionDate: DateTime,
                    state: Blurb)

case class Page[A](items: Seq[A], page: Int, offset: Long, total: Long) {

  lazy val prev = Option(page - 1).filter(_ >= 0)

  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)

}

object Blurb extends IndexableManager[Blurb] {

  implicit val userIdReads = Json.reads[UserId]
  implicit val userIdWrites = Json.writes[UserId]

  implicit object authMethodReads extends Reads[AuthenticationMethod] {
    def reads(json: JsValue) = json match {
      case JsString(s) => JsSuccess(AuthenticationMethod(s))
      case other =>
        JsError("Can't parse JSON String as an AuthenticationMethod. JSON content = " +
          other.toString())
    }
  }

  implicit object authMethodWrites extends Writes[AuthenticationMethod] {
    def writes(authMethod: AuthenticationMethod) = JsString(authMethod.method)
  }

  implicit val identityReads: Reads[Identity] = (
    (__ \ "id").read[UserId] ~
    (__ \ "firstName").read[String] ~
    (__ \ "lastName").read[String] ~
    (__ \ "fullName").read[String] ~
    (__ \ "email").readNullable[String] ~
    (__ \ "avatarUrl").readNullable[String] ~
    (__ \ "authMethod").read[AuthenticationMethod]
    )((id, firstName, lastName, fullName, email, avatarUrl, authMethod) =>
      SocialUser(
        id = id,
        firstName = firstName,
        lastName = lastName,
        fullName = fullName,
        email = email,
        avatarUrl = avatarUrl,
        authMethod = authMethod
      )
    )

  implicit object identityWrites extends Writes[Identity] {
    def writes(identity: Identity) =
      Json.obj(
        "id" -> identity.id,
        "firstName" -> identity.firstName,
        "lastName" -> identity.lastName,
        "fullName" -> identity.fullName,
        "email" -> identity.email.getOrElse[String](""),
        "avatarUrl" -> identity.avatarUrl.getOrElse[String](""),
        "authMethod" -> identity.authMethod.method
      )
  }

  implicit object objectIdReads extends Reads[ObjectId] {
    def reads(json: JsValue) = json match {
      case JsObject(("$oid", JsString(v)) +: Nil) =>
        if (ObjectId.isValid(v)) JsSuccess(new ObjectId(v))
        else JsError("Invalid ObjectId")
      case other =>
        JsError("Can't parse JSON path as an ObjectId. JSON content = " +
          other.toString())
    }
  }

  implicit object objectIdWrites extends Writes[ObjectId] {
    def writes(oid: ObjectId) = Json.obj("$oid" -> oid.toString)
  }

  implicit object dateTimeReads extends Reads[DateTime] {
    def reads(json: JsValue) = json match {
      case jsString: JsString =>
        JsSuccess(
          DateTime.parse(jsString.value, ISODateTimeFormat.dateTime())
        )
      case other =>
        JsError("Can't parse JSON path as a DateTime. JSON content = " +
          other.toString())
    }
  }

  implicit object dateTimeWrites extends Writes[DateTime] {
    def writes(dt: DateTime) =
      JsString(dt.toString(ISODateTimeFormat.dateTime()))
  }

  implicit val semanticEntityReads: Reads[SemanticEntity] = (
    (__ \ "type").read[String] ~
    (__ \ "relevance").read[Double] ~
    (__ \ "count").read[Int] ~
    (__ \ "text").read[String]
    )(SemanticEntity)

  implicit val semanticEntityWrites: Writes[SemanticEntity] = (
    (__ \ "type").write[String] ~
    (__ \ "relevance").write[Double] ~
    (__ \ "count").write[Int] ~
    (__ \ "text").write[String]
    )(unlift(SemanticEntity.unapply))

  implicit val blurbReads: Reads[Blurb] = (
    // https://groups.google.com/forum/#!topic/play-framework/njps4vDRZNo
    (__ \ "_id").readNullable[ObjectId] ~
    (__ \ "question").read[String] ~
    (__ \ "answer").read[String] ~
    (__ \ "tags").read[Array[String]] ~
    (__ \ "entities").readNullable[Array[SemanticEntity]] ~
    (__ \ "createdBy").readNullable[Identity] ~
    (__ \ "createdDate").readNullable[DateTime] ~
    (__ \ "lastModifiedBy").readNullable[Identity] ~
    (__ \ "lastModifiedDate").readNullable[DateTime] ~
    (__ \ "version").read[Int]
    )(Blurb.apply _)

  implicit val blurbWrites: Writes[Blurb] = (
    (__ \ "_id").writeNullable[ObjectId] ~
    (__ \ "question").write[String] ~
    (__ \ "answer").write[String] ~
    (__ \ "tags").write[Array[String]] ~
    (__ \ "entities").writeNullable[Array[SemanticEntity]] ~
    (__ \ "createdBy").writeNullable[Identity] ~
    (__ \ "createdDate").writeNullable[DateTime] ~
    (__ \ "lastModifiedBy").writeNullable[Identity] ~
    (__ \ "lastModifiedDate").writeNullable[DateTime] ~
    (__ \ "version").write[Int]
    )(unlift(Blurb.unapply))

  // reads and writes are required by the IndexableManager trait and are
  // specific to play-elasticsearch
  // blurbReads/Writes above provide general JSON ser/de as implicit values
  val reads: Reads[Blurb] = (
    (__ \ "_id").read[String] ~
    (__ \ "question").read[String] ~
    (__ \ "answer").read[String] ~
    (__ \ "tags").read[Array[String]] ~
    (__ \ "entities").read[Array[SemanticEntity]]
    )((id, question, answer, tags, entities) =>
      Blurb(
        Some(new ObjectId(id)),
        question,
        answer,
        tags,
        Some(entities),
        None, None, None, None
      )
    )

  val writes = new Writes[Blurb] {
    def writes(blurb: Blurb) = {
      val entities = blurb.entities.map(a => {
        a.groupBy(_.entityType).map(t => {
          t._1 -> t._2.map(_.text)
        })
      }).getOrElse(Map())
      val entitiesJson = Json.toJson(entities)
      Json.obj(
        "question" -> blurb.question,
        "answer" -> blurb.answer,
        "tags" -> blurb.tags,
        "author" -> blurb.createdBy.map(_.fullName),
        "createdDate" -> blurb.createdDate,
        "createdYearMonth" -> blurb.createdDate.map(dt => dt.getYear + "-" + dt.getMonthOfYear),
        "lastEditedBy" -> blurb.lastModifiedBy.map(_.fullName),
        "lastModifiedDate" -> blurb.lastModifiedDate,
        "lastModifiedYearMonth" -> blurb.lastModifiedDate.map(dt => dt.getYear + "-" + dt.getMonthOfYear)
      ) ++ entitiesJson.as[JsObject]
    }
  }

  implicit val oldBlurbReads = Json.reads[Revision]

  implicit val oldBlurbWrites = Json.writes[Revision]

  val form = Form(
    mapping(
      "id" -> optional(of[String] verifying pattern(
        """[a-fA-F0-9]{24}""".r,
        "constraint.objectId",
        "error.objectId")),
      "question" -> nonEmptyText,
      "answer" -> nonEmptyText,
      "tags" -> text.transform(
        (csv: String) => csv.split(','),
        (arr: Array[String]) => if (arr == null) "" else arr.mkString(",")),
      "entities" -> optional(text),
      "createdBy" -> optional(text),
      "createdDate" -> optional(of[Long]),
      "lastModifiedBy" -> optional(text),
      "lastModifiedDate" -> optional(of[Long])
    ) { (id, question, answer, tags, entities,
         createdBy, createdDate,
         lastModifiedBy, lastModifiedDate) =>
      Blurb(
        id.map(new ObjectId(_)),
        question,
        answer,
        tags,
        None,
        createdBy.map(json => Json.parse(json).as[Identity]),
        createdDate.map(new DateTime(_)),
        lastModifiedBy.map(json => Json.parse(json).as[Identity]),
        lastModifiedDate.map(new DateTime(_))
      )
    } { blurb =>
      Some(
        (
          blurb.key.map(_.toString),
          blurb.question,
          blurb.answer,
          blurb.tags,
          None,
          blurb.createdBy.map(user => Json.stringify(Json.toJson(user))),
          blurb.createdDate.map(_.getMillis),
          blurb.lastModifiedBy.map(user => Json.stringify(Json.toJson(user))),
          blurb.lastModifiedDate.map(_.getMillis)
        )
      )
    }
  )

  val indexType = "blurb"

  // Alchemy API
//  def getEntities(text: String): Future[Array[SemanticEntity]] = {
//    WS.url("http://access.alchemyapi.com/calls/text/TextGetRankedNamedEntities").post(Map(
//      "apikey" -> Seq("fdcd4e5268c7352c74685b472f7c02a97a27be7c"),
//      "text" -> Seq(text),
//      "outputMode" -> Seq("json")
//    )).map(result => {
//      val entities: Seq[JsValue] = result.json \ "entities" \\ "entity"
//      entities.map(_.as[SemanticEntity]).toArray
//    })
//  }

  // Open Calais
  def getEntities(text: String): Future[Option[Array[SemanticEntity]]] = {
    import ExecutionContext.Implicits.global

    WS.url("http://api.opencalais.com/tag/rs/enrich")
      .withHeaders(
        "x-calais-licenseID" -> "3ab4m8fybbsgtmxcvfm46uxu",
        "Content-Type" -> "text/raw",
        "Accept" -> "application/json",
        "Cache-Control" -> "no-cache",
        "Origin" -> "play-framework",
        "Accept-Language" -> "en-GB,en-US;q=0.8,en;q=0.6",
        "Host" -> "api.opencalais.com")
      .post(text)
      .map(response => {
        response.status match {
          case 200 => {
            val entities = response.json.as[JsObject].values map { t =>
              t \ "_typeGroup" match {
                case JsUndefined(_) => None
                case JsString("entities") =>
                  Some(SemanticEntity(
                    (t \ "_type").as[String],
                    (t \ "relevance").as[Double],
                    (t \ "instances").as[JsArray].value.length,
                    (t \ "name").as[String]
                  ))
                case _ => None
              }
            }
            val array = entities.flatten.toArray
            if (array.isEmpty) None
            else Some(array)
          }
          case _ => None
        }
      })
  }
}
