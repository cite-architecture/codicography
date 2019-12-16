package edu.furman.classics.codicography

import better.files._
import java.io.{File => JFile}
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.citerelation._
import edu.holycross.shot.dse._
import edu.holycross.shot.ohco2._
import edu.furman.classics.citewriter._
import scala.annotation.tailrec
import scala.io.Source

/** An object that writes a facsimile view of a CEX file that implements the DSE datamodel */
object writer {

  case class FacsimilePageMetadata(
    urn: Cite2Urn,
    prevFile: Option[String],
    prevLabel: Option[String],
    nextFile: Option[String],
    nextLabel: Option[String],
    howMany: Int,
    index: Int,
    fileName: String,
    title: String 
  )

  case class FacsimilePage( metadata: FacsimilePageMetadata, content: String )



  /** Our main function where the action happens */
  def main(args: Array[String]) {
  		// Find out of the user typed the right command to launch this!

  		// Get the parameters that came with 'run'
  		val params: Vector[String] = args.toVector
  		val libraryPath: Option[String] = {
  			if ( params.size > 0 ) Some(params.head)
  			else {
  				println(s"""\n------\nRunning with default config: configs/config.txt.\n\nTo specify a different config file, type 'run path/to/config.txt'.\n------\n""")
  				Some("configs/config.txt")
  			}
  		}
  		libraryPath match {
  			case Some(lp) => {
  				 if (lp.endsWith(".txt")) makePages( lp )
  				 else {
  				 	println(s"""\n------\nParameter `${lp}` does not seem to be a Unix path to a .txt config file.\n------\n""")
  				 }
  			}
	  		case None => // do nothing
  		}
  		
  }

  def getConfig( f: String): Option[Map[String, String]] = {
    try {
      val configString: Vector[String] = utilities.loadFile(f).filter(_.size > 0).filter( _.contains("#")).filter ( _.take(2) != "//")
      val cm: Map[String, String] = configString.map( s => {
        val k = s.split("#").toVector.head
        val v = s.split("#").toVector.last
        (k -> v)
      }).toMap

      println(s"""\n\nCurrent Config Values:\n\n""")
      utilities.showMe(cm)
      println("\n")

      Some(cm)
    } catch {
      case e: Exception => {
         println(s"Error getting config file: ${e}")
         None 
      }
    }
  }

  /** the `main` function kicks it off, but the real work happens here… */
  def makePages( configPath: String ): Unit = {

  	try {

				/* Steps */
				// 1. Get our Data…
      val config: Option[Map[String, String]] = getConfig(configPath)
      val cexFile: String = config.get("cexPath")
      val htmlPath: String = config.get("htmlPath")
      lazy val lib: CiteLibrary = utilities.loadLibrary(cexFile)
      lazy val tr: TextRepository = lib.textRepository.get
      lazy val corp: Corpus = tr.corpus
      lazy val cat: Catalog = tr.catalog
      lazy val colls: CiteCollectionRepository = lib.collectionRepository.get
      lazy val rels: CiteRelationSet = lib.relationSet.get
      val dseVec: DseVector = DseVector.fromCiteLibrary(lib)

      /* -------------------------------------- */
      /* Get Surface URNs */
      val surfaceUrns: Vector[Cite2Urn] = {
        val limit: Option[Int] = {
          val fromConf: Int = config.get("pageLimit").toInt
          if (fromConf == 0) None
          else Some(fromConf)
        }
        content.selectionSwitcher( dseVec, lib, limit ) 
      }

      /* Prepare the directory */
      val htmlDirPath: String = config.get("htmlPath")
      val htmlDir: File = htmlDirPath.toFile.createIfNotExists(true)
      htmlDir.clear()

      
      /* Iterate through surfaceUrns, making pages */
      for ( s <- surfaceUrns.zipWithIndex ) {
        // make a facs-object 
        val u: Cite2Urn = s._1
        val index: Int = s._2
        // Get metadata for page (everything we need except the content-html)
        val pm: FacsimilePageMetadata = makeFacsMetadata(u, index, surfaceUrns, colls, corp, dseVec, config.get)
        // Get the content-html
        val contentHtml: String = content.contentSwitcher(u, lib, config.get)
        // Make a FacsimilePage object
        val facsPage = FacsimilePage( pm, contentHtml )
        // Assemble the whole page html
        val html: String = assembleHtml(facsPage, config.get)
        // Save it
        utilities.saveString( html, htmlDirPath, facsPage.metadata.fileName)
      }





      /* -------------------------------------- */
    } catch {
			case e: Exception => {
  			println(s"""\n-----\nBuilding the site failed with the following error: \n\n${e}\n\n-----\n)""")
			}
    }
  }

  

  /** Given a Cite2Urn and a Vector[Cite2Urn], make 
   *  a FacsimilePageMetadata object
   */
  def makeFacsMetadata(
    thisUrn: Cite2Urn,
    thisIndex: Int,
    surfaceUrns: Vector[Cite2Urn], 
    colls: CiteCollectionRepository,
    corp: Corpus,
    dseVec: DseVector,
    config: Map[String, String]
  ): FacsimilePageMetadata = {

    val howMany: Int = surfaceUrns.size

    val urn: Cite2Urn = thisUrn
    val fileName: String = urnToFileName(urn)
    val surfaceObj: CiteObject = colls.citableObject(urn)
    val title: String = surfaceObj.label
    val index: Int = thisIndex
    val prevFile: Option[String] = {
      if (index > 0) {
        val prevUrn = surfaceUrns(index - 1)
        Some(urnToFileName(prevUrn))
      } else {
        None
      }
    }
    val prevLabel: Option[String] = {
      if (index > 0) {
        val prevUrn = surfaceUrns(index - 1)
        val prevObj: CiteObject = colls.citableObject(prevUrn)
        Some(prevObj.label)
      } else {
        None
      }
    }
    val nextFile: Option[String] = {
      if (index < (surfaceUrns.size - 1)) {
        val nextUrn = surfaceUrns(index + 1)
        Some(urnToFileName(nextUrn))
      } else {
        None
      }
    }
    val nextLabel: Option[String] = {
      if (index < (surfaceUrns.size - 1)) {
        val nextUrn = surfaceUrns(index + 1)
        val nextObj: CiteObject = colls.citableObject(nextUrn)
        Some(nextObj.label)
      } else {
        None
      }
    }
    FacsimilePageMetadata(
      urn = urn,
      prevFile = prevFile,
      prevLabel = prevLabel,
      nextFile = nextFile,
      nextLabel = nextLabel,
      howMany = howMany,
      index = index, 
      fileName = fileName,
      title = title
      )
  }

  /** A function: Given a URN and a number, generate a file-name. */
  def urnToFileName( urn: Urn, index: Option[Int] = None ): String = {
    urn match {
      case CtsUrn(_) => {
          urnToFileName( urn.asInstanceOf[CtsUrn], index )
      }
      case _ => {
          urnToFileName( urn.asInstanceOf[Cite2Urn], index )
      }
    }
  }
  def urnToFileName( urn: CtsUrn, index: Option[Int] ): String = {
    val baseName: String = urn.workParts.mkString("_")
    index match {
      case Some(i) => {
        s"${baseName}_${i}.html"
      }
      case None => {
        s"${baseName}.html"
      }
    }
  }

  def urnToFileName( urn: Cite2Urn, index: Option[Int] ): String = {
    val baseName: String = urn.toString.replaceAll(":","_").replaceAll("\\.","-")
    index match {
      case Some(i) => {
        s"${baseName}_${i}.html"
      }
      case None => {
        s"${baseName}.html"
      }
    }
  }

/** Working from the inside out, use templates to assemble
   *  an HTML page
   */
  def assembleHtml( facsObject: FacsimilePage, config: Map[String, String]): String = {
    // Get the templates
    val surfTempFile: String = config("surfaceTemplate")
    val surfTemp: String = utilities.loadFile(surfTempFile).mkString("\n")
    val htmlTempFile: String = config("htmlTemplate")
    val htmlTemp: String = utilities.loadFile(htmlTempFile).mkString("\n")
    val cssTempFile: String = config("cssTemplate")
    val cssTemp: String = utilities.loadFile(cssTempFile).mkString("\n")
    val jsTempFile: String = config("jsTemplate")
    val jsTemp: String = utilities.loadFile(jsTempFile).mkString("\n")

    // Navigation
    val navHtml: String = {
      val prevHtml: String = facsObject.metadata.prevFile match {
        case Some(f) => {
          s"""<span class="cts_prev"><a href="${f}"> ⇽ ${facsObject.metadata.prevLabel.getOrElse("")} </a></span>"""
        }
        case None => ""
      } 
      val nextHtml: String = facsObject.metadata.nextFile match {
        case Some(f) => {
          s"""<span class="cts_next"><a href="${f}"> ${facsObject.metadata.nextLabel.getOrElse("")} ⇾ </a></span>"""
        }
        case None => ""
      } 
      val middleStr: String = {
          if ( (facsObject.metadata.prevFile != None) && (facsObject.metadata.nextFile != None) ) " | " else ""
        }
      s"""<div class="cite_facs_nav">${prevHtml}${middleStr}${nextHtml}</div>"""
    }

    // Progress bar, 'cause it's fun!
    val progressBarHtml = {
      s"""<div class="cts_progress"><progress class="cts_progress" max="${facsObject.metadata.howMany}" value="${facsObject.metadata.index + 1}"/></div>"""
    }

    // Some stats
    val statsHtml = {
      s"""<div class="cite_stats">Generated from file <code>${config("cexPath")}</code>. This is page ${facsObject.metadata.index + 1} of ${facsObject.metadata.howMany} pages.</div>"""
    }

    // Work inside to outside
    val surfaceHtml: String = surfTemp.replaceAll("CONTENT_HERE", facsObject.content)
    val htmlPage = htmlTemp.replaceAll("LABEL_HERE", facsObject.metadata.title)
                           .replaceAll("CSS_HERE", cssTemp)
                           .replaceAll("NAV_HERE", navHtml)
                           .replaceAll("CONTENT_HERE", surfaceHtml)
                           .replaceAll("JS_HERE", jsTemp)
                           .replaceAll("PROGRESSBAR_HERE", progressBarHtml)
                           .replaceAll("STATS_HERE", statsHtml)
                           .replaceAll("URN_HERE", HtmlWriter.writeUrn(facsObject.metadata.urn))
    htmlPage

  }

}
