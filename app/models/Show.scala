package models

import scala.xml._
import utils._
import java.net._

class Show(val id: Int, val name: String) {
	override def toString = name + " " + id
}

object Show {
	def apply(id: Int, name: String): Show = new Show(id, name)
	def apply(tuple: (Int, String)): Show = (apply _).tupled(tuple)

	val searchUrl = "http://services.tvrage.com/feeds/search.php?show="
	val showUrl = "	http://services.tvrage.com/feeds/showinfo.php?sid="

	def fromApi(search: String): List[Show] = {
		val xml = Http.get(searchUrl + URLEncoder.encode(search))
		val shows = xml \ "show"
		var list: List[Show] = Nil

		for (show <- shows) {
			val name = (show \ "name").text
			val id = (show \ "showid").text.toInt
			list ::= new Show(id, name)
		}

		list.sortBy(_.name)
	}

	def fromApi(showid: Int): Option[Show] = {
		val xml = Http.get(showUrl + showid)
		val name = xml \ "showname"

		name.text match {
			case "" => None
			case n => {
				val s = Show(showid, n)
				create(s)
				Some(s)
			}
		}
	}

	def create(show: Show): Unit = {
		Queries.getShowById(show.id) match {
			case Some(_) =>
			case None => { 
				Queries.addShow(show.id, show.name)
				Episode.fromApi(show.id)
			}
		}
	}

	def allShows(): List[Show] = {
		Queries.getShows map { Show(_) }
	}

	def getById(showid: Int): Option[Show] = {
		Queries.getShowById(showid) map { Show(_) } orElse fromApi(showid)
	}
}