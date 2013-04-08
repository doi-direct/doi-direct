package doi

import org.apache.http.client.HttpClient
import org.apache.http.params.HttpProtocolParams
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.DecompressingHttpClient
import org.apache.commons.io.IOUtils
import org.apache.http.impl.client.DefaultHttpClient

object Slurp {
  val client: HttpClient = new DecompressingHttpClient(new DefaultHttpClient)
  client.getParams().setBooleanParameter("http.protocol.handle-redirects", true)

  def useragent = "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)"
  HttpProtocolParams.setUserAgent(client.getParams(), useragent);

  def apply(url: String): String = {

    val get = new HttpGet(url)
    get.setHeader("Accept", "text/html,application/xhtml+xml,application/xml");

    val response = client.execute(get);
    IOUtils.toString(response.getEntity.getContent())
  }
}
