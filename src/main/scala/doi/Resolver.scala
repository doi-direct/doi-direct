package doi

import scala.io.Source
import java.net.URL
import java.net.HttpURLConnection

trait Resolution
case class jQuery(query: String) extends Resolution

object Resolver extends App {

  // FIXME persist this
  val fallbackCache = scala.collection.mutable.Map[String, Option[String]]()

  def resolveViaDX(doi: String): Option[String] = {
    fallbackCache.getOrElseUpdate(doi, {
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

  def resolveLocally(doi: String): Option[String] = {
    doi match {
      case doi if doi.startsWith("10.1002/") => Some("http://onlinelibrary.wiley.com/doi/" + doi + "/pdf")
      case doi if doi.startsWith("10.1007/") => Some("http://link.springer.com/content/pdf/" + doi)
      case doi if doi.startsWith("10.1080/") => Some("http://www.tandfonline.com/doi/pdf/" + doi)
      // 10.1088/0951-7715/23/12/012 --> http://iopscience.iop.org/0951-7715/23/12/012/pdf/0951-7715_23_12_012.pdf
      case doi if doi.startsWith("10.1088/") => Some("http://iopscience.iop.org/" + doi.stripPrefix("10.1088/") + "/pdf/" + doi.stripPrefix("10.1088/").replace('/', '_') + ".pdf")
      // 10.1089/cmb.2008.0023 --> http://online.liebertpub.com/doi/pdf/10.1089/cmb.2008.0023
      case doi if doi.startsWith("10.1089/") => Some("http://online.liebertpub.com/doi/pdf/" + doi)
      // World Scientific 
      // 10.1142/S0218216502001779 ---resolves to---> http://www.worldscientific.com/doi/abs/10.1142/S0218216502001779
      //						   ---follow "PDF ("---> http://www.worldscientific.com/doi/pdf/10.1142/S0218216502001779
      case doi if doi.startsWith("10.1142/") => Some("http://www.worldscientific.com/doi/pdf/" + doi)
      // 10.2307/2586590 --> http://www.jstor.org/stable/pdfplus/2586590.pdf?acceptTC=true
      case doi if doi.startsWith("10.2307/") => Some("http://www.jstor.org/stable/pdfplus/" + doi.stripPrefix("10.2307/") + "?acceptTC=true")
      // 10.1090/conm/517/10488 --> http://www.ams.org/books/conm/517/conm517.pdf
      case doi if doi.startsWith("10.1090/conm/") || doi.startsWith("10.1090/pspum/") => {
        val List(_, series, volume, _) = doi.split('/').toList
        Some("http://www.ams.org/books/" + series + "/" + volume + "/" + series + volume + ".pdf")
      }

      case _ => None
    }
  }

  def resolveUsingMetadata(doi: String): Option[String] = {
    None
  }

  // FIXME persist this
  val resolveByScrapingCache = scala.collection.mutable.Map[String, String]()

  def resolveByScraping(doi: String): Option[String] = {
    resolveByScrapingCache.get(doi) match {
      case Some(result) => Some(result)
      case None => {
        val attempt = doi match {
          // 10.1006 10.1016, Elsevier, has complicated URLs, e.g.
          // 10.1006/jabr.1996.0306 ---resolves to---> http://www.sciencedirect.com/science/article/pii/S0021869396903063
          //						---follow "PDF" (or jQuery "#pdfLink")---> http://pdn.sciencedirect.com/science?_ob=MiamiImageURL&_cid=272332&_user=10&_pii=S0021869396903063&_check=y&_origin=article&_zone=toolbar&_coverDate=1996--15&view=c&originContentFamily=serial&wchp=dGLbVlt-zSkWz&md5=fb951ad4ff13953e97dc2afd6fd16d4a&pid=1-s2.0-S0021869396903063-main.pdf
          //                        ---resolves to---> http://ac.els-cdn.com/S0021869396903063/1-s2.0-S0021869396903063-main.pdf?_tid=756d984e-a048-11e2-8b82-00000aab0f02&acdnat=1365424565_666b1bf7394bbc91c15fac27d45952a0
          case doi if doi.startsWith("10.1006") || doi.startsWith("10.1016") => {
            val regex = """pdfurl="([^"]*)"""".r
            regex.findFirstMatchIn(Slurp("http://dx.doi.org/" + doi)) map { m => m.group(1) }
          }

          // 10.1017 10.1051, Cambridge University Press also has complicated URLs:
          // 10.1017/S0022112010001734 ---resolves to---> http://journals.cambridge.org/action/displayAbstract?fromPage=online&aid=7829674
          //						   ---follow "View PDF (" (or jQuery for "a.article-pdf")---> http://journals.cambridge.org/action/displayFulltext?type=1&fid=7829676&jid=FLM&volumeId=655&issueId=-1&aid=7829674&bodyId=&membershipNumber=&societyETOCSession=
          //						   ---resolves to---> http://journals.cambridge.org/download.php?file=%2FFLM%2FFLM655%2FS0022112010001734a.pdf&code=ac265aacb742b93fa69d566e33aeaf5e
          //      case doi if doi.startsWith("10.1017") || doi.startsWith("10.1051") => {
          //     
          //      }

          // 10.1070/IM2010v074n04ABEH002503 ---resolves to---> http://mr.crossref.org/iPage/?doi=10.1070%2FIM2010v074n04ABEH002503
          //								 ---follow "IOP Publishing"---> http://iopscience.iop.org/1064-5632/74/4/A03/
          // 								 ---follow "Full text PDF"--> http://iopscience.iop.org/1064-5632/74/4/A03/pdf/1064-5632_74_4_A03.pdf

          // 10.1073/pnas.1001947107         ---resolves to---> http://www.pnas.org/content/107/32/14030
          //								 ---follow "Full Text (PDF)"---> http://www.pnas.org/content/107/32/14030.full.pdf
          // This can also be done directly using the metadata

          // 10.1090, the AMS
          // 10.1090/S0002-9904-1897-00411-6 --> http://www.ams.org/journals/bull/1897-03-07/S0002-9904-1897-00411-6/S0002-9904-1897-00411-6.pdf
          // these can be constructed from the metadata

          // ...

          // Quantum Topology
          // 10.4171/QT/16 --> http://www.ems-ph.org/journals/show_pdf.php?issn=1663-487X&vol=2&iss=2&rank=1

          // 10.1093/imrn/rnp169 --> http://imrn.oxfordjournals.org/content/2010/6/1062.full.pdf
          case _ => None
        }
        attempt.map(result => resolveByScrapingCache.put(doi, result))
        attempt
      }
    }
  }

  def apply(doi: String): Option[String] = {
    println("resolving " + doi)

    val result = resolveLocally(doi).orElse(resolveUsingMetadata(doi)).orElse(resolveByScraping(doi)).orElse(resolveViaDX(doi))

    result match {
      case Some(url) => println(" ... resolved to " + url)
      case None => println(" ... resolution failed")
    }

    result
  }
}