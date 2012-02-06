package models

import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.extended.PostgresDriver.Implicit._
import org.scalaquery.ql.extended.{ExtendedTable => Table}
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql._

object Users extends Table[(Int, String, String)]("users") {
	def id = column[Int]("id")
	def name = column[String]("name")
	def password = column[String]("password")
	def * = id ~ name ~ password
}

object Shows extends Table[(Int, String)]("shows") {
	def id = column[Int]("id")
	def name = column[String]("name")
	def * = id ~ name
}

object Episodes extends Table[(Int, String, Int, Int, java.sql.Date, Int)]("episodes") {
	def id = column[Int]("id")
	def title = column[String]("title")
	def season = column[Int]("season")
	def epNum = column[Int]("epnum")
	def airDate = column[java.sql.Date]("airdate")
	def showid = column[Int]("showid")
	def * = id ~ title ~ season ~ epNum ~ airDate ~ showid
}

object User_Show extends Table[(Int, Int)]("user_show") {
	def userid = column[Int]("userid")
	def showid = column[Int]("showid")
	def * = userid ~ showid
}

object User_Episode extends Table[(Int, Int)]("user_episode") {
	def userid = column[Int]("userid")
	def episodeid = column[Int]("episodeid")
	def * = userid ~ episodeid
}

object Queries {
	val db = Database.forDataSource(play.db.DB.datasource)
	
	def addUser(name: String, password: String): Unit = {
		db withSession {
			Users.name ~ Users.password insert (name, password)
		}
	}
	
	def getUserByName(name: String): Option[(Int, String, String)] = {
		db withSession {
			Users where {_.name is name} firstOption
		}
	}
	
	def addShow(id: Int, name: String): Unit = {
		db withSession {
			Shows insert (id, name)
		}
	}
	
	def getShowById(id: Int): Option[(Int, String)] = {
		db withSession {
			Shows where {_.id is id} firstOption
		}
	}
	
	def getShows: List[(Int, String)] = {
		db withSession {
			(for { s <- Shows
				   _ <- Query orderBy s.name } yield s) list
		}
	}
	
	def getShowByUserId(userid: Int): List[(Int, String)] = {
		db withSession {
			(for {
				Join(s, us) <- Shows innerJoin User_Show on (_.id is _.showid)
				if us.userid is userid
			} yield s.*) list
		}
	}
	
	def isShowFollowedByUserId(userid: Int, showid: Int): Boolean = {
		db withSession {
			User_Show where {us => us.userid === userid && us.showid === showid} firstOption match {
				case Some(_) => true
				case None => false
			}
		}
	}
	
	def addEpisode(title: String, season: Int, epNum: Int, airDate: java.sql.Date, showid: Int): Unit = {
		db withSession {
			(Episodes.title ~ Episodes.season ~ Episodes.epNum ~ Episodes.airDate ~ Episodes.showid) insert
				(title, season, epNum, airDate, showid)
		}
	}
	
	def getEpisodeById(id: Int): Option[(Int, String, Int, Int, java.sql.Date, Int)] = {
		db withSession {
			Episodes where {_.id is id} firstOption
		}
	}
	
	def getEpisodesByShowId(showid: Int): List[(Int, String, Int, Int, java.sql.Date, Int)] = {
		db withSession {
			(for {
				e <- Episodes where {_.showid is showid}
				_ <- Query orderBy e.airDate.asc
			} yield e) list
		}
	}
	
	def getEpisodesByUserId(userid: Int): List[(Int, String, Int, Int, java.sql.Date, Int)] = {
		val userepisode = for(ue <- User_Episode if ue.userid === userid) yield ue.episodeid
		val usershow = for(us <- User_Show if us.userid === userid) yield us.showid
		
		db withSession {
			(for {
				e <- Episodes where {ep => (ep.showid in usershow) && (ep.id notIn userepisode)}
				_ <- Query orderBy e.airDate.asc
			} yield e) list
		}
	}
	
	def getEpisodesByUserIdAndShowId(userid: Int, showid: Int): List[(Int, String, Int, Int, java.sql.Date, Int)] = {
		val userepisode = for(ue <- User_Episode if ue.userid === userid) yield ue.episodeid
		
		db withSession {
			(for {
				e <- Episodes where {ep => (ep.showid is showid) && (ep.id notIn (userepisode))}
				_ <- Query orderBy e.airDate.asc
			} yield e) list
		}
	}
	
	def addShowToUser(userid: Int, showid: Int) = {
		db withSession {
			User_Show insert (userid, showid)
		}
	}
	
	def removeShowFromUser(userid: Int, showid: Int) = {
		db withSession {
			User_Show where {us => us.userid === userid && us.showid === showid} delete
		}
	}
	
	def addEpisodeToUser(userid: Int, episodeid: Int) = {
		db withSession {
			User_Episode where {ue => ue.userid === userid && ue.episodeid === episodeid} delete
		}
	}
	
	def removeEpisodeFromUser(userid: Int, episodeid: Int) = {
		db withSession {
			User_Episode insert (userid, episodeid)
		}
	}
}