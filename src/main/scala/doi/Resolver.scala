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
import com.github.theon.uri.Uri
import net.tqft.toolkit.Logging

trait Resolution
case class jQuery(query: String) extends Resolution

object Resolver extends Logging {

  type =>?[-A, +B] = PartialFunction[A, B]

  def resolveLocally(doi: String): Option[String] = {
    val rules: String =>? String = {
      // Springer
      // 10.1023/A:1015622607840 ---resolves to---> http://link.springer.com/article/10.1023%2FA%3A1015622607840
      // 						   ---links to---> http://link.springer.com/content/pdf/10.1023%2FA%3A1015622607840
      case doi if doi.startsWith("10.1007/") || doi.startsWith("10.1023/") => "http://link.springer.com/content/pdf/" + doi
      // 10.1073/pnas.95.1.98 ---resolves to---> http://www.pnas.org/content/95/1/98
      //	                    ---links to---> http://www.pnas.org/content/95/1/98.full.pdf
      case doi if doi.startsWith("10.1073/pnas") && doi.count(_ == '.') == 4 => {
        "http://www.pnas.org/content/" + doi.stripPrefix("10.1073/pnas.").replaceAllLiterally(".", "/") + ".full.pdf"
      }
      case doi if doi.startsWith("10.1080/") || doi.startsWith("10.1081/") => "http://www.tandfonline.com/doi/pdf/" + doi
      // 10.1088/0951-7715/23/12/012 --> http://iopscience.iop.org/0951-7715/23/12/012/pdf/0951-7715_23_12_012.pdf
      case doi if doi.startsWith("10.1088/") => "http://iopscience.iop.org/" + doi.stripPrefix("10.1088/") + "/pdf/" + doi.stripPrefix("10.1088/").replace('/', '_') + ".pdf"
      // 10.1089/cmb.2008.0023 --> http://online.liebertpub.com/doi/pdf/10.1089/cmb.2008.0023
      case doi if doi.startsWith("10.1089/") => "http://online.liebertpub.com/doi/pdf/" + doi
      // 10.1090/conm/517/10488 --> http://www.ams.org/books/conm/517/conm517.pdf
      case doi if doi.startsWith("10.1090/conm/") || doi.startsWith("10.1090/pspum/") => {
        val List(_, series, volume, _) = doi.split('/').toList
        "http://www.ams.org/books/" + series + "/" + volume + "/" + series + volume + ".pdf"
      }
      // Oxford University Press
      // We handle these DOIs below, by looking up metadata
      // 10.1112/jtopol/jtq033 --> http://jtopol.oxfordjournals.org/content/4/1/190.full.pdf
      // 10.1112/plms/pdq011 ---resolves to---> http://plms.oxfordjournals.org/content/102/1/25
      //					   ---links to--->    http://plms.oxfordjournals.org/content/102/1/25.full.pdf+html
      //					   ---links to--->    http://plms.oxfordjournals.org/content/102/1/25.full.pdf
      // And these ones here
      // 10.1112/plms/s3-65.2.423 ---resolves to---> http://plms.oxfordjournals.org/content/s3-65/2/423
      //							---links to--->    http://plms.oxfordjournals.org/content/s3-65/2/423.full.pdf
      // 10.1112/blms/14.5.385 ---resolves to---> http://blms.oxfordjournals.org/content/14/5/385

      case doi if doi.startsWith("10.1112/") && (doi.contains("-") || doi.count(_ == '.') == 3) => {
        val List("10.1112", journal, fragment) = doi.split('/').toList
        "http://" + journal + ".oxfordjournals.org/content/" + fragment.replaceAllLiterally(".", "/") + ".full.pdf"
      }
      // SIAM
      // http://dx.doi.org/10.1137/S1064827599357024 ---resolves to---> http://epubs.siam.org/doi/abs/10.1137/S1064827599357024
      //											 ---links to--->    http://epubs.siam.org/doi/pdf/10.1137/S1064827599357024
      case doi if doi.startsWith("10.1137/") => "http://epubs.siam.org/doi/pdf/" + doi
      // World Scientific 
      // 10.1142/S0218216502001779 ---resolves to---> http://www.worldscientific.com/doi/abs/10.1142/S0218216502001779
      //						   ---links to---> http://www.worldscientific.com/doi/pdf/10.1142/S0218216502001779
      case doi if doi.startsWith("10.1142/") => "http://www.worldscientific.com/doi/pdf/" + doi

      // Some IMRN DOIs can be done locally:
      // 10.1155/IMRN/2006/87604 --> http://imrn.oxfordjournals.org/content/2006/87604.full.pdf
      // 10.1155/IMRN/2006/36856 --> http://imrn.oxfordjournals.org/content/2006/36856.full.pdf
      case doi if doi.startsWith("10.1155/IMRN") && doi.count(_ == '/') == 3 => {
        "http://imrn.oxfordjournals.org/content/" + doi.stripPrefix("10.1155/IMRN/") + ".full.pdf"
      }

      // JSTOR
      // 10.2307/2586590 --> http://www.jstor.org/stable/pdfplus/2586590.pdf?acceptTC=true
      case doi if doi.startsWith("10.2307/") || doi.startsWith("10.4169/") => "http://www.jstor.org/stable/pdfplus/" + doi.stripPrefix("10.2307/") + "?acceptTC=true"
      // 10.3842/SIGMA.2008.059  ---resolves to---> http://www.emis.de/journals/SIGMA/2008/059/
      // 						   ---links to---> http://www.emis.de/journals/SIGMA/2008/059/sigma08-059.pdf
      // 10.3842/SIGMA.2011.062 ---resolves to---> http://www.emis.de/journals/SIGMA/2011/062/
      //                        ---links to---> http://www.emis.de/journals/SIGMA/2011/062/sigma11-062.pdf
      case doi if doi.startsWith("10.3842/") => {
        val List("SIGMA", year, paper) = doi.stripPrefix("10.3842/").split('.').toList
        "http://www.emis.de/journals/SIGMA/" + year + "/" + paper + "/sigma" + year.takeRight(2) + "-" + paper + ".pdf"
      }
      // 10.4007/annals.2011.174.3.5 --> http://annals.math.princeton.edu/wp-content/uploads/annals-v174-n3-p05-s.pdf
      // There is also an older syntax which requires resolving the DOI:
      // 10.4007/annals.2009.170.995 ---resolve to-> http://annals.math.princeton.edu/2009/170-2/p18
      // 							 ---links to---> http://annals.math.princeton.edu/wp-content/uploads/annals-v170-n2-p18-p.pdf
      case doi if doi.startsWith("10.4007/") && doi.count(_ == '.') == 5 => {
        val List("annals", year, volume, number, page) = doi.stripPrefix("10.4007/").split('.').toList
        val suffix = if (volume.toInt >= 170) "-s" else ""
        "http://annals.math.princeton.edu/wp-content/uploads/annals-v" + volume + "-n" + number + "-p" + padLeft(page.toString, '0', 2) + suffix + ".pdf"
      }
      // Homology, Homotopy and Applications
      // 10.4310/HHA.2012.v14.n1.a5 ---> http://www.intlpress.com/HHA/v14/n1/a5/pdf
      case doi if doi.startsWith("10.4310/") => {
        val List(journal, year, volume, number, a) = doi.stripPrefix("10.4310/").split('.').toList
        "http://www.intlpress.com/" + List(journal, volume, number, a, "pdf").mkString("/")
      }
      // 10.1512/iumj.2009.58.3518 ---resolves to---> http://www.iumj.indiana.edu/IUMJ/fulltext.php?artid=3518&year=2009&volume=58
      //						   ---links to--->    http://www.iumj.indiana.edu/IUMJ/FTDLOAD/2009/58/3518/pdf
      case doi if doi.startsWith("10.1512/iumj") => {
        val List("iumj", year, volume, articleId) = doi.stripPrefix("10.1512/").split('.').toList
        "http://www.iumj.indiana.edu/IUMJ/FTDLOAD/" + year + "/" + volume + "/" + articleId + "/pdf"
      }

      // 10.4064/fm209-1-5 --resolves to---> http://journals.impan.pl/cgi-bin/doi?fm209-1-5
      //					--links to---> http://journals.impan.pl/cgi-bin/fm/pdf?fm209-1-05
      case doi if doi.startsWith("10.4064/") => {
        val journalAbbreviation = doi.stripPrefix("10.4064/").take(2)
        val List(volume, number, id) = doi.stripPrefix("10.4064/").stripPrefix(journalAbbreviation).split('-').toList
        "http://journals.impan.pl/cgi-bin/" + journalAbbreviation + "/pdf?" + journalAbbreviation + volume + "-" + number + "-" + padLeft(id, '0', 2)
      }

      // https://dl.acm.org/purchase.cfm?id=1255446&CFID=315412686&CFTOKEN=28137627
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
    // PNAS can sometimes be resolves locally, but otherwise:
    // 10.1073/pnas.1001947107         ---resolves to---> http://www.pnas.org/content/107/32/14030
    //								 ---follow "Full Text (PDF)"---> http://www.pnas.org/content/107/32/14030.full.pdf
    case doi if doi.startsWith("10.1073/") => {
      Article.fromDOI(doi) flatMap { article =>
        article.pageStart map { start =>
          "http://www.pnas.org/content/" + article.volume + "/" + article.number + "/" + start + ".full.pdf"
        }
      }
    }

    // 10.1090, the AMS
    // 10.1090/S0002-9904-1897-00411-6 --> http://www.ams.org/journals/bull/1897-03-07/S0002-9904-1897-00411-6/S0002-9904-1897-00411-6.pdf
    // 10.1090/S0002-9947-2010-05210-9 --> http://www.ams.org/journals/tran/2011-363-05/S0002-9947-2010-05210-9/S0002-9947-2010-05210-9.pdf
    case doi if doi.startsWith("10.1090/") => {
      Article.fromDOI(doi) match {
        case Some(article) => {
          val identifier = doi.stripPrefix("10.1090/")
          val journalCode = article.bibtex.get("JOURNAL").get match {
            case "Bull. Amer. Math. Soc." => "bull" // ISSN 0002-9904
            case "Bull. Amer. Math. Soc. (N.S.)" => "bull" // ISSN 0273-0979
            case "Trans. Amer. Math. Soc." => "tran" // ISSN 0002-9947
            case "Represent. Theory" => "ert" // ISSN 1088-4165
            case "Conform. Geom. Dyn." => "ecgd" // ISSN
            case "J. Amer. Math. Soc." => "jams" // ISSN
            case "Proc. Amer. Math. Soc." => "proc" // ISSN
            case "Math. Comp." => "mcom" // ISSN
            case "Mem. Amer. Math. Soc." => "memo" // ISSN
            case "Electron. Res. Announc. Amer. Math. Soc." => "era" // ISSN
            case "Algebra i Analiz" => "spmj" // Now the St. Petersburg Mathematical Journal
            case "J. Algebraic Geom." => "jag" // ISSN
            case "Int. Math. Res. Not. IMRN" => ??? // Shouldn't actually appear? Mistakes in the BIBTEX?
            case "J. London Math. Soc. (2)" => ??? // Also a mistake?
            case "J. Reine Angew. Math." => ??? // also mistakes...
          }
          Some("http://www.ams.org/journals/" + journalCode + "/" + article.year + "-" + padLeft(article.volume.toString, '0', 2) + "-" + padLeft(article.number.toString, '0', 2) + "/" + identifier + "/" + identifier + ".pdf")
        }
        case None => {
          warn("No metadata available for " + doi)
          None
        }
      }
    }

    // 10.1093/imrn/rnp169 --> http://imrn.oxfordjournals.org/content/2010/6/1062.full.pdf
    // 10.1155/S1073792891000041 --> http://imrn.oxfordjournals.org/content/2000/1/23.full.pdf
    // 10.1155/IMRN.2005.2941 --> http://imrn.oxfordjournals.org/content/2005/48/2941 --> http://imrn.oxfordjournals.org/content/2005/48/2941.full.pdf
    // Some can be done locally:
    // 10.1155/IMRN/2006/87604 --> http://imrn.oxfordjournals.org/content/2006/87604.full.pdf
    // 10.1155/IMRN/2006/36856 --> http://imrn.oxfordjournals.org/content/2006/36856.full.pdf
    case doi if doi.startsWith("10.1093/imrn") || doi.startsWith("10.1155/IMRN") || doi.startsWith("10.1155/S10737928") => {
      Article.fromDOI(doi) flatMap { article =>
        article.pageStart map { start =>
          "http://imrn.oxfordjournals.org/content/" + article.year + "/" + article.number + "/" + start + ".full.pdf"
        }
      }
    }

    // Oxford University Press
    // We handle these DOIs here:
    // 10.1112/jtopol/jtq033 --> http://jtopol.oxfordjournals.org/content/4/1/190.full.pdf
    // 10.1112/plms/pdq011 ---resolves to---> http://plms.oxfordjournals.org/content/102/1/25
    //					   ---links to--->    http://plms.oxfordjournals.org/content/102/1/25.full.pdf+html
    //					   ---links to--->    http://plms.oxfordjournals.org/content/102/1/25.full.pdf
    // 10.1112/S0010437X07003260 looks like it is at OUP, but it's really at CUP
    // 10.1112/S0024610701002733 ---resolves to---> http://jlms.oxfordjournals.org/content/65/1/223
    // And these ones above, by rewriting the DOI
    // 10.1112/blms/14.5.385 ---resolves to---> http://blms.oxfordjournals.org/content/14/5/385
    // 10.1112/plms/s3-65.2.423 ---resolves to---> http://plms.oxfordjournals.org/content/s3-65/2/423
    //							---links to--->    http://plms.oxfordjournals.org/content/s3-65/2/423.full.pdf
    case doi if doi.startsWith("10.1112/") && !doi.contains("-") => {
      val journal = doi.split('/').toList match {
        case List("10.1112", journal, _) => journal
        case List("10.1112", fragment) if fragment.startsWith("S00246107") => "jlms"
        case List("10.1112", fragment) if fragment.startsWith("S00246115") => "plms"
      }
      Article.fromDOI(doi) flatMap { article =>
        article.pageStart map { start =>
          "http://" + journal + ".oxfordjournals.org/content/" + article.volume + "/" + article.number + "/" + start + ".full.pdf"
        }
      }
    }

    // 10.1103/RevModPhys.80.1083  --- resolves to --> http://rmp.aps.org/abstract/RMP/v80/i3/p1083_1
    //							   --- links to --> http://rmp.aps.org/pdf/RMP/v80/i3/p1083_1
    // 10.1103/PhysRevLett.101.010501 -- resolves to --> http://prl.aps.org/abstract/PRL/v101/i1/e010501
    // 								  ---links to---> http://prl.aps.org/pdf/PRL/v101/i1/e010501
    // 10.1103/PhysRevA.75.032322 --> resolves to --> http://pra.aps.org/abstract/PRA/v75/i3/e032322
    // 							  ---links to --->    http://pra.aps.org/pdf/PRA/v75/i3/e032322
    case doi if doi.startsWith("10.1103/") => {
      val List(journal, volume, page) = doi.stripPrefix("10.1103/").split('.').toList
      val shortJournal = journal match {
        case "RevModPhys" => "rmp"
        case "PhysRevLett" => "prl"
        case "PhysRevA" => "pra"
        case "PhysRevB" => "prb"
        case "PhysRevC" => "prc"
        case "PhysRevD" => "prd"
        case "PhysRevE" => "pre"
        case "PhysRevX" => "prx"
      }
      def pageFragment(page: String) = {
        shortJournal match {
          case "rmp" => "p" + page + "_1"
          case sj if sj.startsWith("pr") => "e" + page
        }
      }
      Article.fromDOI(doi).map({ article =>
        "http://" + shortJournal + ".aps.org/pdf/" + shortJournal.toUpperCase + "/v" + volume + "/i" + article.number + "/" + pageFragment(page)
      })
    }

    // http://dx.doi.org/10.1353/ajm.2012.0046
    // Amer. J. Math. 134 (2012), no. 6, 1679–1704
    // http://muse.jhu.edu/journals/american_journal_of_mathematics/v134/134.6.moravec.pdf
    case doi if doi.startsWith("10.1353/ajm") => {
      Article.fromDOI(doi).map({ article =>
        val name = java.text.Normalizer.normalize(article.authors.head.lastName, java.text.Normalizer.Form.NFD).replaceAll("[^\\p{ASCII}]", "").toLowerCase
      	"http://muse.jhu.edu/journals/american_journal_of_mathematics/v" + article.volume + "/" + article.volume + "." + article.number + "." + name + ".pdf"
      })
    }
    
    case _ => None
  }

  def selectLink(doj: Doj, attr: String = "href") = {
    if (doj.size == 1) {
      Some(doj.first.attribute(attr))
    } else {
      None
    }
  }

  def jQuery(doi: String) = Html.jQuery("http://dx.doi.org/" + doi)

  val resolveByScrapingRule: String => Option[String] = {
    // 10.1070/SM1995v186n10ABEH000079 // TODO not working?    
    // 10.1070/IM2010v074n04ABEH002503 ---resolves to---> http://mr.crossref.org/iPage/?doi=10.1070%2FIM2010v074n04ABEH002503
    //								 ---follow "IOP Publishing"---> http://iopscience.iop.org/1064-5632/74/4/A03/
    // 								 ---follow "Full text PDF"--> http://iopscience.iop.org/1064-5632/74/4/A03/pdf/1064-5632_74_4_A03.pdf
    case doi if doi.startsWith("10.1070/") => {
      val publisherPage = Html("http://mr.crossref.org/iPage/?doi=" + doi).getAnchorByText("IOP Publishing").click[HtmlPage]
      val links = Html.jQuery(publisherPage).get("a.pdf")
      selectLink(links).map(h => "http://iopscience.iop.org/" + h)
    }

    // Mathematical Sciences Publishers
    // 10.2140/pjm.2010.247.323 ---resolves to---> http://msp.org/pjm/2010/247-2/p04.xhtml
    // 							---links to---> http://msp.org/pjm/2010/247-2/pjm-v247-n2-p04-s.pdf
    // unfortunately that "04" doesn't come from the metadata
    case doi if doi.startsWith("10.2140/") => {
      selectLink(jQuery(doi).get("table.action a.download-caption").first).map(h => "http://msp.org" + h)
    }

    // Quantum Topology
    // 10.4171/QT/16 --> http://www.ems-ph.org/journals/show_pdf.php?issn=1663-487X&vol=2&iss=2&rank=1
    case doi if doi.startsWith("10.4171/") => {
      selectLink(jQuery(doi).get("#content a").first).map(h => "http://www.ems-ph.org" + h)
    }

    // Annals, fallback if we couldn't do it locally or via dx.doi.org
    // 10.4007/annals.2004.160.493 ---> http://annals.math.princeton.edu/wp-content/uploads/annals-v160-n2-p04.pdf
    case doi if doi.startsWith("10.4007/") && doi.count(_ == '.') == 4 => {
      selectLink(jQuery(doi).get("div#pdf-link a"))
    }

    // Canad. Math. Bull.
    // 10.4153/CMB-2010-014-8 --resolves to--> http://cms.math.ca/10.4153/CMB-2010-014-8
    //                        ---links to---> http://cms.math.ca/cmb/v53/etingofB9125.pdf
    case doi if doi.startsWith("10.4153/") => {
      selectLink(jQuery(doi).get("div#readlink a")).map(h => "http://cms.math.ca" + h)
    }

    // AAAS
    // 10.1126/
    // FIXME not working
    case doi if doi.startsWith("10.1126/") => {
      selectLink(jQuery(doi).get("meta[name=citation_pdf_url]"), "content")
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
    // 10.1215/00127094-1548371 ---resolves to---> http://projecteuclid.org/Dienst/getRecord?id=euclid.dmj/1330610810/
    //							---resolves to---> http://projecteuclid.org/DPubS?service=UI&version=1.0&verb=Display&handle=euclid.dmj/1330610810
    //							---links to--->    http://projecteuclid.org/DPubS/Repository/1.0/Disseminate?view=body&id=pdf_1&handle=euclid.dmj/1330610810
    // 10.1215/S0012-7094-92-06702-0 ---resolves to---> http://projecteuclid.org/DPubS?service=UI&version=1.0&verb=Display&handle=euclid.dmj/1077294270
    //								 ---links to--->    http://projecteuclid.org/DPubS/Repository/1.0/Disseminate?view=body&id=pdf_1&handle=euclid.dmj/1077294270
    // 10.1215/00127094-1593344 ---resolves to---> http://projecteuclid.org/euclid.dmj/1338987165
    case doi if doi.startsWith("10.1215/") || doi.startsWith("10.1307/mmj") || doi.startsWith("10.2969") => {
      resolveViaDX(doi).flatMap({ uriString =>
        val uri: Uri = uriString
        val JournalTag = """euclid\.([a-z]*)""".r
        val handle = uri.path.stripPrefix("/").split('/').toList match {
          case JournalTag(tag) :: number :: _ => Some("euclid." + tag + "/" + number)
          case "Dienst" :: _ => Some(uri.query.params("id").head.stripSuffix("/"))
          case "DPubS" :: _ => Some(uri.query.params("handle").head)
          case _ => {
            warn("Unfamiliar DOI resolution for project euclid: " + uri + ", please check " + doi)
            None
          }
        }
        handle map { h => "http://projecteuclid.org/DPubS/Repository/1.0/Disseminate?view=body&id=pdf_1&handle=" + h }
      })
    }

    // 10.1515/crll.2000.019 ---resolves to---> http://www.degruyter.com/view/j/crll.2000.2000.issue-519/crll.2000.019/crll.2000.019.xml
    //						 ---links to---> http://www.degruyter.com/dg/viewarticle.fullcontentlink:pdfeventlink/$002fj$002fcrll.2000.2000.issue-519$002fcrll.2000.019$002fcrll.2000.019.xml?t:ac=j$002fcrll.2000.2000.issue-519$002fcrll.2000.019$002fcrll.2000.019.xml
    // N.B degruyter send a header: "Content-Disposition: attachment; filename=crll.2000.019.pdf" which means browsers won't show the PDF inline.
    // Using XHR to obtain a blob works nicely.
    case doi if doi.startsWith("10.1515/") => {
      resolveViaDX(doi).map({ url =>
        require(url.startsWith("http://www.degruyter.com/view/"))
        val identifier = url.stripPrefix("http://www.degruyter.com/view/").replaceAllLiterally("/", "$002f")
        "http://www.degruyter.com/dg/viewarticle.fullcontentlink:pdfeventlink/$002f" + identifier + "?t:ac=" + identifier
      })
    }

    // American Institute of Physics
    // http://dx.doi.org/10.1063/1.864184 ---resolves to---> http://link.aip.org/link/PFLDAS/v26/i3/p684/s1&Agg=doi
    // 									  ---links to---> http://scitation.aip.org/getpdf/servlet/GetPDFServlet?filetype=pdf&id=PFLDAS000026000003000684000001&idtype=cvips&doi=10.1063/1.864184&prog=normal
    case doi if doi.startsWith("10.1063/") => {
      // This only works while logged in:
      //      selectLink(jQuery(doi).get("li.fulltextdesc a").first)
      resolveViaDX(doi).map({ url =>
        println(url)
        require(url.startsWith("http://link.aip.org/link/"))
        val fragments = url.stripPrefix("http://link.aip.org/link/").stripSuffix("&Agg=doi").split('/')
        "http://scitation.aip.org/getpdf/servlet/GetPDFServlet?filetype=pdf&id=" +
          fragments(0).toUpperCase() +
          padLeft(fragments(1).stripPrefix("v"), '0', 6) +
          padLeft(fragments(2).stripPrefix("i"), '0', 6) +
          padLeft(fragments(3).stripPrefix("p"), '0', 6) +
          padLeft(fragments(4).stripPrefix("s"), '0', 6) +
          "&idtype=cvips&doi=" + doi + "&prog=normal"
      })
    }

    // 10.4007/annals.2009.170.995 ---resolve to-> http://annals.math.princeton.edu/2009/170-2/p18
    // 							 ---links to---> http://annals.math.princeton.edu/wp-content/uploads/annals-v170-n2-p18-p.pdf
    case doi if doi.startsWith("10.4007/") && doi.count(_ == '.') == 4 => {
      resolveViaDX(doi).map({ url =>
        val List(year, volume, number, id) = url.stripPrefix("http://annals.math.princeton.edu/").split("[/-]").toList
        val suffix = if (volume.toInt >= 170) "-s" else ""
        "http://annals.math.princeton.edu/wp-content/uploads/annals-v" + volume + "-n" + number + "-" + id + suffix + ".pdf"
      })
    }

    // 10.1103/RevModPhys.80.1083  --- resolves to (off campus) --> http://link.aps.org/doi/10.1103/RevModPhys.80.1083
    // 10.1103/RevModPhys.80.1083  --- resolves to (on campus) --> http://rmp.aps.org/abstract/RMP/v80/i3/p1083_1
    //							   --- links to --> http://rmp.aps.org/pdf/RMP/v80/i3/p1083_1
    // 10.1103/PhysRevLett.101.010501 -- resolves to --> http://prl.aps.org/abstract/PRL/v101/i1/e010501
    // 								  ---links to---> http://prl.aps.org/pdf/PRL/v101/i1/e010501
    // 10.1103/PhysRevA.75.032322 --> resolves to --> http://pra.aps.org/abstract/PRA/v75/i3/e032322
    // 							  ---links to --->    http://pra.aps.org/pdf/PRA/v75/i3/e032322
    case doi if doi.startsWith("10.1103/") => {
      resolveViaDX(doi).flatMap({
        case url if url.contains("abstract") => Some(url.replace("abstract", "pdf"))
        case url if url.startsWith("http://link.aps.org/doi/") => None // not enough information available
      })
    }

    // Hindawi
    case doi if doi.startsWith("10.1155/") => {
      resolveViaDX(doi).map({ url =>
        "http://downloads." + url.stripPrefix("http://www.").stripSuffix("/") + ".pdf"
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