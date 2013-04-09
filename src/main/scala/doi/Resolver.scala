package doi

import scala.io.Source
import java.net.URL
import java.net.HttpURLConnection
import net.tqft.mathscinet.Article
import net.tqft.util.Html
import net.tqft.util.FirefoxSlurp
import com.gargoylesoftware.htmlunit.html.HtmlPage
import be.roam.hue.doj.Doj
import net.tqft.toolkit.amazon.AnonymousS3
import org.apache.http.HttpException

trait Resolution
case class jQuery(query: String) extends Resolution

object Resolver {

  type =>?[-A, +B] = PartialFunction[A, B]

  def resolveLocally(doi: String): Option[String] = {
    val rules: String =>? String = {
      // Wiley
      case doi if doi.startsWith("10.1002/") => "http://onlinelibrary.wiley.com/doi/" + doi + "/pdf"
      // Springer
      // 10.1023/A:1015622607840 ---resolves to---> http://link.springer.com/article/10.1023%2FA%3A1015622607840
      // 						   ---links to---> http://link.springer.com/content/pdf/10.1023%2FA%3A1015622607840
      case doi if doi.startsWith("10.1007/") || doi.startsWith("10.1023") => "http://link.springer.com/content/pdf/" + doi
      case doi if doi.startsWith("10.1080/") => "http://www.tandfonline.com/doi/pdf/" + doi
      // 10.1088/0951-7715/23/12/012 --> http://iopscience.iop.org/0951-7715/23/12/012/pdf/0951-7715_23_12_012.pdf
      case doi if doi.startsWith("10.1088/") => "http://iopscience.iop.org/" + doi.stripPrefix("10.1088/") + "/pdf/" + doi.stripPrefix("10.1088/").replace('/', '_') + ".pdf"
      // 10.1089/cmb.2008.0023 --> http://online.liebertpub.com/doi/pdf/10.1089/cmb.2008.0023
      case doi if doi.startsWith("10.1089/") => "http://online.liebertpub.com/doi/pdf/" + doi
      // World Scientific 
      // 10.1142/S0218216502001779 ---resolves to---> http://www.worldscientific.com/doi/abs/10.1142/S0218216502001779
      //						   ---links to---> http://www.worldscientific.com/doi/pdf/10.1142/S0218216502001779
      case doi if doi.startsWith("10.1142/") => "http://www.worldscientific.com/doi/pdf/" + doi
      // JSTOR
      // 10.2307/2586590 --> http://www.jstor.org/stable/pdfplus/2586590.pdf?acceptTC=true
      case doi if doi.startsWith("10.2307/") => "http://www.jstor.org/stable/pdfplus/" + doi.stripPrefix("10.2307/") + "?acceptTC=true"
      // 10.1090/conm/517/10488 --> http://www.ams.org/books/conm/517/conm517.pdf
      case doi if doi.startsWith("10.1090/conm/") || doi.startsWith("10.1090/pspum/") => {
        val List(_, series, volume, _) = doi.split('/').toList
        "http://www.ams.org/books/" + series + "/" + volume + "/" + series + volume + ".pdf"
      }
      // 10.4007/annals.2011.174.3.5 --> http://annals.math.princeton.edu/wp-content/uploads/annals-v174-n3-p05-s.pdf
      case doi if doi.startsWith("10.4007") => {
        val List("annals", year, volume, number, page) = doi.stripPrefix("10.4007/").split('.').toList
        "http://annals.math.princeton.edu/wp-content/uploads/annals-v" + volume + "-n" + number + "-p" + padLeft(page.toString, '0', 2) + "-s.pdf"
      }
      // 10.3842/SIGMA.2008.059  ---resolves to---> http://www.emis.de/journals/SIGMA/2008/059/
      // 						   ---links to---> http://www.emis.de/journals/SIGMA/2008/059/sigma08-059.pdf
      case doi if doi.startsWith("10.3842") => {
        val List("SIGMA", year, paper) = doi.stripPrefix("10.3842/").split('.').toList
        "http://www.emis.de/journals/SIGMA/" + year + "/" + paper + "/sigma" + year.takeRight(2) + "-" + paper + ".pdf"
      }
    }
    rules.lift(doi)
  }

  def padLeft(string: String, padding: Char, length: Int) = {
    if (length > string.size) {
      padding.toString * (length - string.size) + string
    } else {
      string
    }
  }

  def resolveUsingMetadataRule: String => Option[String] = {
    // 10.1073/pnas.1001947107         ---resolves to---> http://www.pnas.org/content/107/32/14030
    //								 ---follow "Full Text (PDF)"---> http://www.pnas.org/content/107/32/14030.full.pdf
    case doi if doi.startsWith("10.1073") => {
      Article.fromDOI(doi) flatMap { article =>
        article.pageStart map { start =>
          "http://www.pnas.org/content/" + article.volume + "/" + article.number + "/" + start + ".full.pdf"
        }
      }
    }

    // 10.1112/jtopol/jtq033 --> http://jtopol.oxfordjournals.org/content/4/1/190.full.pdf
    case doi if doi.startsWith("10.1112/jtopol") => {
      Article.fromDOI(doi) flatMap { article =>
        article.pageStart map { start =>
          "http://jtopol.oxfordjournals.org/content/" + article.volume + "/" + article.number + "/" + start + ".full.pdf"
        }
      }
    }

    // 10.1093/imrn/rnp169 --> http://imrn.oxfordjournals.org/content/2010/6/1062.full.pdf
    // 10.1155/S1073792891000041 --> http://imrn.oxfordjournals.org/content/2000/1/23.full.pdf
    case doi if doi.startsWith("10.1093/imrn") || doi.startsWith("10.1155/S10737928") => {
      Article.fromDOI(doi) flatMap { article =>
        article.pageStart map { start =>
          "http://imrn.oxfordjournals.org/content/" + article.year + "/" + article.number + "/" + start + ".full.pdf"
        }
      }
    }

    // 10.1090, the AMS
    // 10.1090/S0002-9904-1897-00411-6 --> http://www.ams.org/journals/bull/1897-03-07/S0002-9904-1897-00411-6/S0002-9904-1897-00411-6.pdf
    case doi if doi.startsWith("10.1090") => {
      Article.fromDOI(doi) flatMap { article =>
        val identifier = doi.stripPrefix("10.1090/")
        val journalCode = article.bibtex.get("JOURNAL").get match {
          case "Bull. Amer. Math. Soc." => "bull"
        }
        Some("http://www.ams.org/journals/" + journalCode + "/" + article.year + "-" + padLeft(article.volume.toString, '0', 2) + "-" + padLeft(article.number.toString, '0', 2) + "/" + identifier + "/" + identifier + ".pdf")
      }
    }

    case _ => None
  }

  def selectLink(doj: Doj) = {
    if (doj.size == 1) {
      Some(doj.first.attribute("href"))
    } else {
      None
    }
  }

  val resolveByScrapingRule: String => Option[String] = {
    // 10.1006 10.1016, Elsevier, has complicated URLs, e.g.
    // 10.1006/jabr.1996.0306 ---resolves to---> http://www.sciencedirect.com/science/article/pii/S0021869396903063
    //						---follow "PDF" (or jQuery "#pdfLink")---> http://pdn.sciencedirect.com/science?_ob=MiamiImageURL&_cid=272332&_user=10&_pii=S0021869396903063&_check=y&_origin=article&_zone=toolbar&_coverDate=1996--15&view=c&originContentFamily=serial&wchp=dGLbVlt-zSkWz&md5=fb951ad4ff13953e97dc2afd6fd16d4a&pid=1-s2.0-S0021869396903063-main.pdf
    //                        ---resolves to---> http://ac.els-cdn.com/S0021869396903063/1-s2.0-S0021869396903063-main.pdf?_tid=756d984e-a048-11e2-8b82-00000aab0f02&acdnat=1365424565_666b1bf7394bbc91c15fac27d45952a0
    case doi if doi.startsWith("10.1006") || doi.startsWith("10.1016") => {
      val scrape = Html.jQuery("http://dx.doi.org/" + doi).get("#pdfLink")
      selectLink(scrape)
    }

    // 10.1017 10.1051, Cambridge University Press also has complicated URLs:
    // 10.1017/S0022112010001734 ---resolves to---> http://journals.cambridge.org/action/displayAbstract?fromPage=online&aid=7829674
    //						   ---follow "View PDF (" (or jQuery for "a.article-pdf")---> http://journals.cambridge.org/action/displayFulltext?type=1&fid=7829676&jid=FLM&volumeId=655&issueId=-1&aid=7829674&bodyId=&membershipNumber=&societyETOCSession=
    //						   ---resolves to---> http://journals.cambridge.org/download.php?file=%2FFLM%2FFLM655%2FS0022112010001734a.pdf&code=ac265aacb742b93fa69d566e33aeaf5e
    case doi if doi.startsWith("10.1017") || doi.startsWith("10.1051") => {
      val scrape = Html.jQuery("http://dx.doi.org/" + doi).get("a.article-pdf")
      selectLink(scrape).map(h => "http://journals.cambridge.org/action/" + h.replaceAll("\n", "").replaceAll("\t", "").replaceAll(" ", ""))
    }

    // 10.1070/IM2010v074n04ABEH002503 ---resolves to---> http://mr.crossref.org/iPage/?doi=10.1070%2FIM2010v074n04ABEH002503
    //								 ---follow "IOP Publishing"---> http://iopscience.iop.org/1064-5632/74/4/A03/
    // 								 ---follow "Full text PDF"--> http://iopscience.iop.org/1064-5632/74/4/A03/pdf/1064-5632_74_4_A03.pdf
    case doi if doi.startsWith("10.1070") => {
      val publisherPage = Html("http://mr.crossref.org/iPage/?doi=" + doi).getAnchorByText("IOP Publishing").click[HtmlPage]
      val links = Html.jQuery(publisherPage).get("a.pdf")
      selectLink(links).map(h => "http://iopscience.iop.org/" + h)
    }

    // Quantum Topology
    // 10.4171/QT/16 --> http://www.ems-ph.org/journals/show_pdf.php?issn=1663-487X&vol=2&iss=2&rank=1
    case doi if doi.startsWith("10.4171") => {
      selectLink(Html.jQuery("http://dx.doi.org/" + doi).get("#content a").first).map(h => "http://www.ems-ph.org" + h)
    }

    // ...

    // Mathematical Sciences Publishers
    // 10.2140/pjm.2010.247.323 ---resolves to---> http://msp.org/pjm/2010/247-2/p04.xhtml
    // 							---links to---> http://msp.org/pjm/2010/247-2/pjm-v247-n2-p04-s.pdf
    // unfortunately that "04" doesn't come from the metadata
    case doi if doi.startsWith("10.2140") => {
      selectLink(Html.jQuery("http://dx.doi.org/" + doi).get("table.action a.download-caption").first).map(h => "http://msp.org/" + h)
    }

    case _ => None
  }

  val resolveRemotelyCache = AnonymousS3("DOI2pdf")

  def resolveRemotely(doi: String): Option[String] = {
    resolveRemotelyCache.get(doi) match {
      case Some(result) => Some(result)
      case None => {
        val attempt = resolveUsingMetadataRule(doi).orElse(resolveViaDXRule(doi)).orElse(resolveByScrapingRule(doi))
        attempt.map(result => resolveRemotelyCache.put(doi, result))
        attempt
      }
    }
  }

  val dxCache = AnonymousS3("dx.doi.org")

  def resolveViaDX(doi: String): Option[String] = {
    try {
      Some(dxCache.getOrElseUpdate(doi, {
        println(" ... looking up dx.doi.org")
        val connection = new URL("http://dx.doi.org/" + doi).openConnection().asInstanceOf[HttpURLConnection]
        connection.setInstanceFollowRedirects(false);
        if (connection.getResponseCode == 303) {
          connection.getHeaderField("Location")
        } else {
          throw new HttpException(connection.getResponseMessage())
        }
      }))
    } catch {
      case e: HttpException => None
    }
  }

  def resolveViaDXRule: String => Option[String] = {
    // 10.1215/00127094-1548371 ---resolves to---> http://projecteuclid.org/euclid.dmj/1330610810
    // 											   http://projecteuclid.org/DPubS/Repository/1.0/Disseminate?view=body&id=pdfview_1&handle=euclid.dmj/1330610810
    case doi if doi.startsWith("10.1215") => {
      resolveViaDX(doi).map({ url =>
        require(url.startsWith("http://projecteuclid.org/"))
        "http://projecteuclid.org/DPubS/Repository/1.0/Disseminate?view=body&id=pdfview_1&handle=" + url.stripPrefix("http://projecteuclid.org/")
      })
    }

    // 10.1515/crll.2000.019 ---resolves to---> http://www.degruyter.com/view/j/crll.2000.2000.issue-519/crll.2000.019/crll.2000.019.xml
    //						 ---links to---> http://www.degruyter.com/dg/viewarticle.fullcontentlink:pdfeventlink/$002fj$002fcrll.2000.2000.issue-519$002fcrll.2000.019$002fcrll.2000.019.xml?t:ac=j$002fcrll.2000.2000.issue-519$002fcrll.2000.019$002fcrll.2000.019.xml
    // N.B degruyter send a header: "Content-Disposition: attachment; filename=crll.2000.019.pdf" which means Chrome won't show the PDF inline.
    case doi if doi.startsWith("10.1515") => {
      resolveViaDX(doi).map({ url =>
        require(url.startsWith("http://www.degruyter.com/view/"))
        val identifier = url.stripPrefix("http://www.degruyter.com/view/").replaceAllLiterally("/", "$002f")
        "http://www.degruyter.com/dg/viewarticle.fullcontentlink:pdfeventlink/$002f" + identifier + "?t:ac=" + identifier
      })
    }

    case _ => None
  }

  def apply(doi: String): Option[String] = {
    println("resolving " + doi)

    val result = resolveLocally(doi).orElse(resolveRemotely(doi))

    result match {
      case Some(url) => println(" ... resolved to " + url)
      case None => println(" ... resolution failed")
    }

    result
  }
}