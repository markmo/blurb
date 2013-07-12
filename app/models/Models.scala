package models

import com.github.cleverage.elasticsearch.ScalaHelpers._
import com.github.cleverage.elasticsearch.annotations.IndexMapping
import org.bson.types.ObjectId
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.format.Formats._
import play.api.data.validation.Constraints._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import reactivemongo.bson._
import securesocial.core._

/**
 * Created by markmo on 5/07/13.
 */
//@IndexMapping(value =
//  "{columns: {" +
//    "properties: {" +
//      "question: {type: \"string\"}," +
//      "answer: {type: \"string\"}," +
//      "tags: {type: \"string\", index_name: \"tag\", index: \"not_analyzed\"}," +
//      "author: {type: \"string\", index: \"not_analyzed\"}," +
//      "createdDate: {type: \"date\"}," +
//      "createdYearMonth: {type: \"string\", index: \"not_analyzed\"}," +
//      "lastEditedBy: {type: \"string\", index: \"not_analyzed\"}," +
//      "lastEditedDate: {type: \"date\"}," +
//      "lastModifiedYearMonth: {type: \"string\", index: \"not_analyzed\"}" +
//    "}}")
case class Blurb(key: Option[ObjectId],
                 //id: Option[BSONObjectID],
                 question: String,
                 answer: String,
                 tags: Array[String],
                 createdBy: Option[Identity],
                 createdDate: Option[DateTime],
                 lastModifiedBy: Option[Identity],
                 lastModifiedDate: Option[DateTime],
                 version: Int = 1) extends Indexable {

  def id = key.map(_.toString).getOrElse("")

}

object Blurb extends IndexableManager[Blurb] {
/**
  implicit object UserIdBSONReader extends BSONDocumentReader[UserId] {
    def read(doc: BSONDocument): UserId =
      UserId(
        doc.getAs[String]("id").get,
        doc.getAs[String]("providerId").get
      )
  }

  implicit object UserIdBSONWriter extends BSONDocumentWriter[UserId] {
    def write(userId: UserId): BSONDocument =
      BSONDocument(
        "id" -> userId.id,
        "providerId" -> userId.providerId
      )
  }

  implicit object IdentityBSONReader extends BSONDocumentReader[Identity] {
    def read(doc: BSONDocument): Identity =
      SocialUser(
        id = doc.getAs[UserId]("id").get,
        firstName = doc.getAs[String]("firstName").get,
        lastName = doc.getAs[String]("lastName").get,
        fullName = doc.getAs[String]("fullName").get,
        email = doc.getAs[String]("email"),
        avatarUrl = doc.getAs[String]("avatarUrl"),
        authMethod = AuthenticationMethod(doc.getAs[String]("authMethod").get)
      )
  }

  implicit object IdentityBSONWriter extends BSONDocumentWriter[Identity] {
    def write(identity: Identity): BSONDocument =
      BSONDocument(
        "id" -> identity.id,
        "firstName" -> identity.firstName,
        "lastName" -> identity.lastName,
        "fullName" -> identity.fullName,
        "email" -> identity.email,
        "avatarUrl" -> identity.avatarUrl,
        "authMethod" -> identity.authMethod.method
      )
  }

  implicit object BlurbBSONReader extends BSONDocumentReader[Blurb] {
    def read(doc: BSONDocument): Blurb =
      Blurb(
        doc.getAs[BSONObjectID]("_id"),
        doc.getAs[String]("question").get,
        doc.getAs[String]("answer").get,
        doc.getAs[Array[String]]("tags").get,
        doc.getAs[Identity]("createdBy"),
        doc.getAs[BSONDateTime]("createdDate").map(dt => new DateTime(dt.value)),
        doc.getAs[Identity]("lastModifiedBy"),
        doc.getAs[BSONDateTime]("lastModifiedDate").map(dt => new DateTime(dt.value))
      )
  }

  implicit object BlurbBSONWriter extends BSONDocumentWriter[Blurb] {
    def write(blurb: Blurb): BSONDocument =
      BSONDocument(
        "_id" -> blurb.key.getOrElse(BSONObjectID.generate),
        "question" -> blurb.question,
        "answer" -> blurb.answer,
        "tags" -> blurb.tags,
        "createdBy" -> blurb.createdBy,
        "createdDate" -> blurb.createdDate.map(date => BSONDateTime(date.getMillis)),
        "lastModifiedBy" -> blurb.lastModifiedBy,
        "lastModifiedDate" -> blurb.lastModifiedDate.map(date => BSONDateTime(date.getMillis))
      )
  }

  implicit object userIdReads extends Reads[UserId] {
    def reads(json: JsValue) = json match {
      case jsObject: JsObject =>
        JsSuccess(
          UserId(
            id = (jsObject \ "id").as[String],
            providerId = (jsObject \ "providerId").as[String]
          )
        )
      case other =>
        JsError("Can't parse JSON path as a UserId. JSON content = " +
          other.toString())
    }
  }

  implicit object userIdWrites extends Writes[UserId] {
    def writes(userId: UserId) =
      Json.obj(
        "id" -> userId.id,
        "providerId" -> userId.providerId
      )
  }

  implicit val userIdReads: Reads[UserId] = (
    (__ \ "id").read[String] ~
    (__ \ "providerId").read[String]
    )(UserId)

  implicit object identityReads extends Reads[Identity] {
    def reads(json: JsValue) = json match {
      case jsObject: JsObject =>
        JsSuccess(
          SocialUser(
            id = (jsObject \ "id").as[UserId],
            firstName = (jsObject \ "firstName").as[String],
            lastName = (jsObject \ "lastName").as[String],
            fullName = (jsObject \ "fullName").as[String],
            email = (jsObject \ "email").asOpt[String],
            avatarUrl = (jsObject \ "avatarUrl").asOpt[String],
            authMethod = AuthenticationMethod((jsObject \ "authMethod").as[String])
          )
        )
      case other =>
        JsError("Can't parse JSON path as an Identity. JSON content = " +
          other.toString())
    }
  }
*/

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

  implicit val blurbReads: Reads[Blurb] = (

    // https://groups.google.com/forum/#!topic/play-framework/njps4vDRZNo
    (__ \ "_id").readNullable[ObjectId] ~

    (__ \ "question").read[String] ~
    (__ \ "answer").read[String] ~
    (__ \ "tags").read[Array[String]] ~
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
    (__ \ "tags").read[Array[String]]
    )((id, question, answer, tags) =>
      Blurb(
        Some(new ObjectId(id)),
        question,
        answer,
        tags,
        None, None, None, None
      )
    )

  val writes = new Writes[Blurb] {
    def writes(blurb: Blurb) =
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
      )
  }

//  val historyWrites = new Writes[Blurb] {
//    def writes(blurb: Blurb) =
//  }

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
      "createdBy" -> optional(text),
      "createdDate" -> optional(of[Long]),
      "lastModifiedBy" -> optional(text),
      "lastModifiedDate" -> optional(of[Long])
    ) { (id, question, answer, tags,
         createdBy, createdDate,
         lastModifiedBy, lastModifiedDate) =>
      Blurb(
        id.map(new ObjectId(_)),
        //id.map(new BSONObjectID(_)),
        question,
        answer,
        tags,
        createdBy.map(json => Json.parse(json).as[Identity]),
        createdDate.map(new DateTime(_)),
        lastModifiedBy.map(json => Json.parse(json).as[Identity]),
        lastModifiedDate.map(new DateTime(_))
      )
    } { blurb =>
      Some(
        (
          blurb.key.map(_.toString),
          //blurb.id.map(_.stringify),
          blurb.question,
          blurb.answer,
          blurb.tags,
          blurb.createdBy.map(user => Json.stringify(Json.toJson(user))),
          blurb.createdDate.map(_.getMillis),
          blurb.lastModifiedBy.map(user => Json.stringify(Json.toJson(user))),
          blurb.lastModifiedDate.map(_.getMillis)
        )
      )
    }
  )

  val indexType = "blurb"

//  def list(page: Int = 0, pageSize: Int = 10,
//           orderBy: String = "lastModifiedDate", orderDirection: Int = -1,
//           filter: String = "*") =
//    Repository.pageBlurbs(page, pageSize, orderBy, orderDirection, filter)

//  def tagOptions = Repository.getTags.map(tag => (tag, tag)).toMap

}

case class Page[A](items: Seq[A], page: Int, offset: Long, total: Long) {

  lazy val prev = Option(page - 1).filter(_ >= 0)

  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)

}
