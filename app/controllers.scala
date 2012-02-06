package controllers

import play._
import play.mvc._
import play.data.validation._
import play.libs._
import play.cache._
import models._

import org.joda.time._
import org.joda.time.format._

object Application extends Controller {

	import views.Application._

	@Before
	def checkAuthenticated = {
		session("username") match {
			case Some(u) =>
				renderArgs += "logged" -> true
				Continue
			case None => Continue
		}
	}

	def currentUser: Option[User] = {
		session("username") match {
			case Some(u) => {
				Queries.getUserByName(u) map { q => User(q._1, q._2)}
			}
			case None => None
		}
	}

	def removeEpisode = {
		currentUser match {
			case Some(user) => {
				user.episodeWatched(params.get("episodeid").toInt)
				Ok
			}
			case None => Forbidden("You must be logged in")
		}
	}

	def addEpisode = {
		currentUser match {
			case Some(user) => {
				user.episodeNotWatched(params.get("episodeid").toInt)
				Ok
			}
			case None => Forbidden("You must be logged in")
		}
	}

	def myShows = {
		currentUser match {
			case Some(user) => html.myshows(user.getShows)
			case None => Forbidden("You must be logged in")
		}
	}

	def myEpisodes = {
		currentUser match {
			case Some(user) => {
				val episodes = user.getNotWatchedEpisodes.filter(Episode.isCurrentEpisode)
				val showName = (showid: Int) => Show.getById(showid).get.name
				
				val formatter = DateTimeFormat.forPattern("EEEE dd MMM yyyy").withLocale(java.util.Locale.US)

				val list = episodes.map(ep => (ep, aired(ep.airDate), formatter.print(ep.airDate), showName(ep.showid)))

				html.myepisodes(list)
			}
			case None => Forbidden("You must be logged in")
		}
	}

	def createAccount = {
		Validation.clear()
		val username = params.get("username")
		val password = params.get("password")
		val password2 = params.get("password2")
		val code = params.get("code")
		val randomID = params.get("randomID")

		Validation.required("username", username).message("E-mail is required")
		Validation.required("password", password).message("Password is required")
		Validation.equals("password2", password2, "password", password).message("Both passwords must be the same")
		Validation.minSize("password", password, 6).message("Password must be at least 6 characters")
		Validation.equals("code", code, "code", Cache.get(randomID).orNull).message("Invalid captcha")

		Cache.delete(randomID)

		if (Validation.hasErrors()) {
			Validation.keep()
			Action(loginPage)
		} else {
			User.create(username, password) match {
				case Some(user) => {
					session.put("username", username)
					Action(allShows)
				}
				case None => {
					Validation.addError("signup-failed", "Username already taken")
					Validation.keep()
					Action(loginPage)
				}
			}
		}
	}

	def login = {
		Validation.clear()
		val username = params.get("username")
		val password = params.get("password")
		val user = User.authenticate(username, password)

		user match {
			case None => {
				Validation.addError("login-failed", "Login failed...")
				Validation.keep()
				Action(loginPage)
			}
			case _ => {
				session += "username" -> username
				Action(myEpisodes)
			}
		}
	}

	def logout = {
		session.remove("username")
		Action(index)
	}

	def addShow = {
		val showid = params.get("showid").toInt
		currentUser match {
			case Some(user) => {
				user.followShow(showid)
				Ok
			}
			case None => Forbidden("You must be logged in")
		}
	}

	def removeShow = {
		val showid = params.get("showid").toInt
		currentUser match {
			case Some(user) => {
				user.unfollowShow(showid)
				Ok
			}
			case None => Forbidden("You must be logged in")
		}
	}

	def captcha(id: String) = {
		val captcha = Images.captcha
		val code = captcha.getText("#3C3C3C")
		Cache.set(id, code, "10mn")
		captcha
	}

	def index = {
		html.index()
	}

	def loginPage = {
		html.login(Codec.UUID)
	}

	def search = {
		val search = params.get("searchstring")

		val shows =
			if (search == null || search.trim() == "")
				Nil
			else
				Show.fromApi(search)

		html.search(shows)
	}

	def allShows = {
		html.allshows(Show.allShows())
	}

	def show(showid: Int) = {
		Show.getById(showid) match {
			case Some(s) => {
				val watched = currentUser.map(_.getNotWatchedEpisodes).getOrElse(Nil)
				val formattedDate = (date: DateTime) => DateTimeFormat.forPattern("dd MMM yyyy").withLocale(java.util.Locale.US).print(date)
				val episodes = Episode.getByShowId(showid)
				val list = episodes.map(ep => (ep, aired(ep.airDate), formattedDate(ep.airDate), watched.contains(ep)))
				val followed = (showid: Int) => currentUser.map(_.isShowFollowed(showid)).getOrElse(false)
				html.show(s, list, followed(s.id))
			}

			case None => NotFound("This show doesn't exist")
		}
	}

	def aired(date: DateTime): Boolean = date.isBeforeNow()
}
