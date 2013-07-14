package models

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import org.ocpsoft.prettytime.PrettyTime

/**
 * User: markmo
 * Date: 13/07/13
 * Time: 5:59 PM
 */
class HumanTime(dt: DateTime) {

  def humanize = {
    val p = new PrettyTime()
    p.format(dt.toDate)
  }

  def short = {
    val fmt: DateTimeFormatter = DateTimeFormat.forPattern("dd/MM/yyyy")
    fmt.print(dt)
  }

  def time = {
    val fmt: DateTimeFormatter = DateTimeFormat.forPattern("HH:mm")
    fmt.print(dt)
  }

}

trait HumanTimeImplicits {

  implicit def dateTime2HumanTime(dt: DateTime) = new HumanTime(dt)

}

object HumanTimeImplicits extends HumanTimeImplicits
