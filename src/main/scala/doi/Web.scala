package doi

import org.jboss.netty.handler.codec.http.{ HttpRequest, HttpResponse }
import com.twitter.finagle.builder.ServerBuilder
import com.twitter.finagle.http.{ Http, Response }
import com.twitter.finagle.Service
import com.twitter.util.Future
import java.net.InetSocketAddress
import util.Properties
import java.net.URI

object Web {
  def main(args: Array[String]) {
    val port = Properties.envOrElse("PORT", "8080").toInt
    println("Starting on port:" + port)
    ServerBuilder()
      .codec(Http())
      .name("doi-direct")
      .bindTo(new InetSocketAddress(port))
      .build(new ResolverService)
    println("Started.")
  }
}

class ResolverService extends Service[HttpRequest, HttpResponse] {
  def apply(req: HttpRequest): Future[HttpResponse] = {
    val response = Response()

    val query = new URI(req.getUri()).getPath().stripPrefix("/")
    if (query.startsWith("10.")) {
      val resolution = Resolver(query)

      val json = req.getHeader("Accept").split(", ").contains("application/json")

      if (json) {
        println(" ... sending JSON response")
        response.setStatusCode(200)
        resolution match {
          case Some(redirect) => response.contentString = """{ "redirect": """" + redirect + """" }"""
          case None => response.contentString = "{  }"
        }
        response.setContentType("application/json")
      } else {
        resolution match {
          case Some(redirect) => {
            println(" ... sending 303 redirect")
            response.setStatusCode(303)
            response.addHeader("Location", redirect)
          }
          case None => {
            println(" ... sending 303 redirect to http://dx.doi.org")
            response.addHeader("Location", "http://dx.doi.org/" + query)
          }
        }
      }
    } else {
      response.setStatusCode(404)
    }
    Future(response)
  }
}