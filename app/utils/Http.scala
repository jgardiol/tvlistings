package utils

object Http {
	import java.net.{ URL, URLConnection }
	import xml.{ Elem, XML }

	def get(urlString: String): Elem = {
		val url = new URL(urlString)
		val conn = url.openConnection()
		XML.load(conn.getInputStream())
	}
}