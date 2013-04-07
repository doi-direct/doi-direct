package doi

import scala.io.Source
import java.net.URL
import java.net.HttpURLConnection

object Resolver extends App {

  val cache = scala.collection.mutable.Map[String, Option[String]]()

  def fallback(doi: String): Option[String] = {
    cache.getOrElseUpdate(doi, {
      println(" ... falling back to dx.doi.org")
      val connection = new URL("http://dx.doi.org/" + doi).openConnection().asInstanceOf[HttpURLConnection]
      connection.setInstanceFollowRedirects(false);
      if (connection.getResponseCode == 303) {
        Some(connection.getHeaderField("Location"))
      } else {
        None
      }
    })
  }

  def apply(doi: String): Option[String] = {
    println("resolving " + doi)

    val result = doi match {
      case doi if doi.startsWith("10.1002/") => Some("http://onlinelibrary.wiley.com/doi/" + doi + "/pdf")

      // 10.1006, Elsevier, has evil URLs, e.g. 10.1006/jabr.1996.0306 leads to http://pdn.sciencedirect.com/science?_ob=MiamiImageURL&_cid=272332&_user=10&_pii=S0021869396903063&_check=y&_origin=article&_zone=toolbar&_coverDate=1996--15&view=c&originContentFamily=serial&wchp=dGLbVlt-zSkWz&md5=fb951ad4ff13953e97dc2afd6fd16d4a&pid=1-s2.0-S0021869396903063-main.pdf

      case doi if doi.startsWith("10.1007/") => Some("http://link.springer.com/content/pdf/" + doi)

      // 10.1016 is also Elsevier

      // 10.1017, Cambridge University Press
      // 10.1017/S0022112010001734 --> http://journals.cambridge.org/action/displayFulltext?type=1&fid=7829676&jid=FLM&volumeId=655&issueId=-1&aid=7829674&bodyId=&membershipNumber=&societyETOCSession=
      //						   --> http://journals.cambridge.org/download.php?file=%2FFLM%2FFLM655%2FS0022112010001734a.pdf&code=ac265aacb742b93fa69d566e33aeaf5e

      // 10.1051 is also CUP

      // 10.1070/IM2010v074n04ABEH002503 --> http://iopscience.iop.org/1064-5632/74/4/A03/pdf/1064-5632_74_4_A03.pdf

      // 10.1073/pnas.1001947107         --> http://www.pnas.org/content/107/32/14030.full.pdf

      case doi if doi.startsWith("10.1080/") => Some("http://www.tandfonline.com/doi/pdf/" + doi)

      // 10.1088/0951-7715/23/12/012 --> http://iopscience.iop.org/0951-7715/23/12/012/pdf/0951-7715_23_12_012.pdf
      case doi if doi.startsWith("10.1088/") => Some("http://iopscience.iop.org/" + doi.stripPrefix("10.1088/") + "/pdf/" + doi.stripPrefix("10.1088/").replace('/', '_') + ".pdf")

      // 10.1089/cmb.2008.0023 --> http://online.liebertpub.com/doi/pdf/10.1089/cmb.2008.0023
      case doi if doi.startsWith("10.1089/") => Some("http://online.liebertpub.com/doi/pdf/" + doi)

      // 10.1090, the AMS
      // 10.1090/S0002-9904-1897-00411-6 --> http://www.ams.org/journals/bull/1897-03-07/S0002-9904-1897-00411-6/S0002-9904-1897-00411-6.pdf
      // these can be constructed from the metadata

      // 10.1090/conm/517/10488 --> http://www.ams.org/books/conm/517/conm517.pdf
      case doi if doi.startsWith("10.1090/conm/") || doi.startsWith("10.1090/pspum/") => {
        val List(_, series, volume, _) = doi.split('/').toList
        Some("http://www.ams.org/books/" + series + "/" + volume + "/" + series + volume + ".pdf")
      }

      // ...

      // World Scientific 
      // 10.1142/S0218216502001779 --> http://www.worldscientific.com/doi/abs/10.1142/S0218216502001779 --> http://www.worldscientific.com/doi/pdf/10.1142/S0218216502001779
      case doi if doi.startsWith("10.1142/") => Some("http://www.worldscientific.com/doi/pdf/" + doi)

      // ...
      
      // 10.2307/2586590 --> http://www.jstor.org/stable/pdfplus/2586590.pdf
      case doi if doi.startsWith("10.2307/") => Some("http://www.jstor.org/stable/pdfplus/" + doi.stripPrefix("10.2307/"))
      
      // Quantum Topology
      // 10.4171/QT/16 --> http://www.ems-ph.org/journals/show_pdf.php?issn=1663-487X&vol=2&iss=2&rank=1
      
      // 10.1093/imrn/rnp169 --> http://imrn.oxfordjournals.org/content/2010/6/1062.full.pdf
      
      case _ => fallback(doi)
    }

    result match {
      case Some(url) => println(" ... resolved to " + url)
      case None => println(" ... resolution failed")
    }

    result
  }
}