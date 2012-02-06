package models;

import scala.xml._
import scala.collection.mutable._
import java.text.SimpleDateFormat
import org.joda.time._
import org.joda.time.format._
import utils._

class Episode(val episodeid: Int,
	val title: String,
	val season: Int,
	val epNum: Int,
	val airDate: DateTime,
	val showid: Int) {

	override def toString = season + "x" + epNum + " - " + title

	override def equals(other: Any): Boolean = {
		other match {
			case that: Episode => (that canEqual this) && episodeid == that.episodeid
			case _ => false
		}
	}

	def canEqual(other: Any): Boolean = other.isInstanceOf[Episode]

	override def hashCode: Int = episodeid.hashCode
}

object Episode {
	val listUrl = "http://services.tvrage.com/feeds/episode_list.php?sid="
	val infoUrl = "	http://services.tvrage.com/feeds/episodeinfo.php?sid="
	val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

	def apply(episodeid: Int, title: String, season: Int, epNum: Int, airDate: DateTime, showid: Int): Episode =
		new Episode(episodeid, title, season, epNum, airDate, showid)

	def apply(tuple: (Int, String, Int, Int, java.sql.Date, Int)): Episode = {
		new Episode(tuple._1, tuple._2, tuple._3, tuple._4, new DateTime(tuple._5), tuple._6)
	}

	// Builds episodes from tvrage API (services.tvrage.com/feeds/episode_list.php?sid=...)
	def fromApi(showid: Int): List[Episode] = {
		val xml = Http.get(listUrl + showid)
		val seasons = xml \ "Episodelist" \ "Season"

		var list: List[Episode] = Nil

		for (season <- seasons) {
			val s = (season \ "@no").text.toInt
			val episodes = season \ "episode"
			for (episode <- episodes) {
				val num = (episode \ "seasonnum").text.toInt
				val title = (episode \ "title").text
				val date = (episode \ "airdate").text
				val airDate = formatter.parseDateTime(date)

				val e = Episode(0, title, s, num, airDate, showid)
				create(e)
				list ::= e
			}
		}
		list
	}

	def create(episode: Episode): Unit = {
		Queries.addEpisode(episode.title, 
				episode.season, 
				episode.epNum, 
				new java.sql.Date(episode.airDate.toDate().getTime()), 
				episode.showid)
	}

	def getByShowId(showid: Int): List[Episode] = {
		Queries.getEpisodesByShowId(showid) map {Episode(_)}
	}

	def isCurrentEpisode(ep: Episode): Boolean = {
		val before = DateTime.now().minusWeeks(4)
		val after = DateTime.now().plusWeeks(4)

		ep.airDate.isAfter(before) && ep.airDate.isBefore(after)
	}
}