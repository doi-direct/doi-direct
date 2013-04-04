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
    Resolver(new URI(req.getUri()).getPath().stripPrefix("/")) match {
      case Some(redirect) => {
        response.setStatusCode(303)
        response.addHeader("Location", redirect)
      }
      case None => {
        response.setStatusCode(404)
      }
    }
    Future(response)
  }
}