package models

import org.joda.time._
import org.joda.time.format._

class User(val id: Int, val username: String) {
	
	def followShow(showid: Int): Unit = {		
		Queries.addShowToUser(id, showid)
	}

	def unfollowShow(showid: Int): Unit = {
		Queries.removeShowFromUser(id, showid)
	}

	def isShowFollowed(showid: Int): Boolean = {
		Queries.isShowFollowedByUserId(id, showid)
	}

	def getShows: List[Show] = {
		Queries.getShowByUserId(id) map { Show(_) }
	}

	def getNotWatchedEpisodes: List[Episode] = {
		Queries.getEpisodesByUserId(id) map { Episode(_) }
	}

	def episodeNotWatched(episodeId: Int): Unit = {
		Queries.addEpisodeToUser(id, episodeId)
	}

	def episodeWatched(episodeId: Int): Unit = {
		Queries.removeEpisodeFromUser(id, episodeId)
	}

}

object User {
	import play.libs.Crypto

	def apply(id: Int, name: String) = new User(id, name)
	
	val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")

	def create(username: String, password: String): Option[User] = {
		val hash = Crypto.passwordHash(password, Crypto.HashType.SHA256)
		Queries.getUserByName(username) match {
			case Some(_) => None
			case None => Queries.addUser(username, hash)
						 Queries.getUserByName(username) map { user => User(user._1, user._2)}
		}
	}

	def authenticate(username: String, password: String): Option[User] = {
		val hash = Crypto.passwordHash(password, Crypto.HashType.SHA256)
		
		Queries.getUserByName(username) match {
			case Some((id,_,h)) if h == hash => Some(User(id, username))
			case _ => None
		}
	}
}