package utils

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

object DateUtils extends Serializable {
  private val oneDayInMs: Long = 1000 * 60 * 60 * 24

  def getDate(base: String, delta: Int = 0): String = {
    val format = if (base.exists(_ == '-')) {
      new SimpleDateFormat("yyyy-MM-dd")
    } else {
      new SimpleDateFormat("yyyyMMdd")
    }
    val baseDay = format.parse(base)
    val ts = baseDay.getTime + delta * oneDayInMs
    tsToString(ts)
  }

  def getDays(base: String, delta: Int, ignoreWeekend: Boolean = false): Array[String] = {
    if (!ignoreWeekend) {
      0.until(delta).map(i => getDate(base, i)).toArray
    } else {
      0.until(delta * 2).map(i => getDate(base, i)).filter(s => {
        val weekday = getWeekday(s)
        weekday != 1 && weekday != 7
      }).slice(0, delta).toArray
    }
  }

  def getWeekday(s: String): Int = {
    val calendar = Calendar.getInstance()
    val format = new SimpleDateFormat("yyyy-MM-dd")
    val day = format.parse(s)
    calendar.setTime(day)
    calendar.get(Calendar.DAY_OF_WEEK)
  }

  def getPreviousDays(base: String, delta: Int): Array[String] = {
    0.until(delta).map(i => getDate(base, -i)).toArray
  }

  def dateToString(d: Date): String = {
    val format = new SimpleDateFormat("yyyy-MM-dd")
    format.format(d)
  }

  def generateDays(start: String, end: String): Iterator[String] = {
    val format = new SimpleDateFormat("yyyy-MM-dd")
    val pt0 = format.parse(start).getTime
    val pt1 = format.parse(end).getTime
    pt0
      .to(pt1, oneDayInMs)
      .map(
        ts => tsToString(ts)
      ).iterator
  }

  def tsToString(ts: Long): String = {
    val format = new SimpleDateFormat("yyyy-MM-dd")
    format.format(new Date(ts))
  }

  def today(): String = {
    val format = new SimpleDateFormat("yyyy-MM-dd")
    format.format(new Date())
  }
}
