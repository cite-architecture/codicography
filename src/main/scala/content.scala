package edu.furman.classics.codicography

import better.files._
import java.io.{File => JFile}
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.citerelation._
import edu.holycross.shot.dse._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citebinaryimage._
import edu.furman.classics.citewriter._
import scala.annotation.tailrec
import scala.io.Source

object content {


  /** For selecting a function to select surfaces */
  def selectionSwitcher( dseVec: DseVector, lib: CiteLibrary, limit: Option[Int] = Some(5), startAt: Option[Cite2Urn] = None ): Vector[Cite2Urn] = {
      getSurfaceUrns(dseVec, lib, limit, startAt)
  }




	/** For selecting a function to attach content to each surface
   *  Options
   *  htmlPageBasicTexts : Just texts for surface
   *  htmlPageTextAndOverviewImage : Texts and a single overview image
   *  htmlPageOverviewTextsRois : Texts, rois, and overview
   */
	def contentSwitcher( surfaceUrn: Cite2Urn, lib: CiteLibrary, config: Map[String, String], rels: CiteRelationSet ): String = {
			//htmlPageOverviewTextsRois(surfaceUrn, lib, config)
      hmt.hmtFacsimile(surfaceUrn, lib, config, rels)
	}


  /** Get all surfaces, in order, for a DSE Vector */
  def getSurfaceUrns( 
    dseVec: DseVector, 
    lib: CiteLibrary, 
    limit: Option[Int], 
    startAt: Option[Cite2Urn] = None
  ): Vector[Cite2Urn] = {

    val colls: Vector[Cite2Urn] = {
      dseVec.passages.map(_.surface.dropSelector).distinct
    } 
    val surfaceUrns: Vector[Cite2Urn] = dseVec.passages.map(_.surface).distinct
    val cr: CiteCollectionRepository = lib.collectionRepository.get
    val collVec: Vector[Cite2Urn] = {
      colls.map( c => {
        (cr ~~ c).map(_.urn)
      }).flatten
    }
    // Some collection-urns may not be in DSE
    val urnsInDse: Vector[Cite2Urn] = {
      collVec.filter( u => {
        surfaceUrns.contains(u)
      })
    }

    // If we are asked to start at one of them…
    val choppedList: Vector[Cite2Urn] = {
      startAt match {
        case None => {
          urnsInDse
        }
        case Some(u) => {
            val idx: Int = urnsInDse.indexOf(u)
            val count: Int = urnsInDse.size
            urnsInDse.takeRight( count - idx )
        }
      }
    }

    // Limited
    val limitedUrns = limit match {
      case Some(i) => choppedList.take(i)
      case None => choppedList
    }

    println("\n--------------\nSurfaces to be Rendered")
    utilities.showMe(limitedUrns)


    limitedUrns
  }



	/** Returns HTML for just the text-passages on a
	 * 	surface, grouped by text. */
	def htmlPageBasicTexts(
    surfaceUrn: Cite2Urn, 
    lib: CiteLibrary,
    config: Map[String, String]
  ): String = {

    // Get things where we can reach them
    val tr: TextRepository = lib.textRepository.get
    val corp: Corpus = tr.corpus
    val cat: Catalog = tr.catalog
    val colls: CiteCollectionRepository = lib.collectionRepository.get
    val rels: CiteRelationSet = lib.relationSet.get
    val dseVec: DseVector = DseVector.fromCiteLibrary(lib)

	  val surfaceTexts: Vector[Corpus] = getTextCorporaForSurface(surfaceUrn, lib, config)

    // Write HTML
    val corporaHtml: String = {
      surfaceTexts.map( t => {
     		val catHtml: String = {
     			val catEntry: CatalogEntry = {
     				val textU: CtsUrn = t.urns.head.dropPassage
     				cat.texts.filter(_.urn == textU).head
     			}
     			HtmlWriter.writeCtsCatalogEntry(catEntry)	
     		} 	
        val corpHtml: String = HtmlWriter.writeCorpus(t)
        Vector(catHtml, corpHtml).mkString("\n")
      }).mkString("\n\n")
    }
    print("\t…done.")
    corporaHtml
  }



  /** Returns HTML for the text-passages on a TBS, 
   *  sorted by Text, working from a Vector[Corpus],
   *  with each passage accompanied by an ImageROI.
   */
  def htmlTextAndROIs(
    surfaceUrn: Cite2Urn, 
    lib: CiteLibrary,
    config: Map[String, String]
  ): String = {

    // Get things where we can reach them
    val tr: TextRepository = lib.textRepository.get
    val corp: Corpus = tr.corpus
    val cat: Catalog = tr.catalog
    val colls: CiteCollectionRepository = lib.collectionRepository.get
    val rels: CiteRelationSet = lib.relationSet.get
    val dseVec: DseVector = DseVector.fromCiteLibrary(lib)

    // Get texts
    val surfaceTexts: Vector[Corpus] = getTextCorporaForSurface(surfaceUrn, lib, config)


    // Get images for each text-node
    val nodesAndImages: Vector[ (CatalogEntry, Vector[ (CitableNode, Option[Cite2Urn]) ] )] = {
      surfaceTexts.map( t => {

        println(s"\t\tGetting image-ROIs for ${t.urns.head.dropPassage}")

        val catEntry: CatalogEntry = {
          val textU: CtsUrn = t.urns.head.dropPassage
          cat.texts.filter(_.urn == textU).head
        }

        val nodes: Vector[CitableNode] = t.nodes
        println(s"\t\t${t.urns.head.dropPassage} = ${nodes.size} passages.")

        val nodeImage: Vector[ (CitableNode, Option[Cite2Urn]) ] = {
          nodes.map( n => {
            val imageForText: Option[Cite2Urn] = {
              try {
                val thisImage: Option[Cite2Urn] = dseVec.imageWRoiForText(n.urn)
                thisImage
              } catch {
                  case e: Exception => {
                    try {
                      val thisImage: Option[Cite2Urn] = dseVec.imageWRoiForText(n.urn.collapsePassageBy(1))
                      thisImage
                    } catch {
                      case e: Exception => None
                    }
                  }
              }
            }
            println(s"\t\t\tGot: ${imageForText}")
            (n, imageForText)
          })
        }
        (catEntry, nodeImage)
      })
    }

     //nodesAndImages: Vector[ (CatalogEntry, Vector[ (CitableNode, Option[Cite2Urn]) ] )]
    val returnHtml: String = nodesAndImages.map( ni => {
      // write catalog entry
      val catHtml = HtmlWriter.writeCtsCatalogEntry(ni._1)

      // Open the Corpus
      val corpHtmlOpen = """<div class="ohco2_versionCorpus">"""
      val corpHtmlClose = """</div>"""

      // for each Node+Image pair…      
      val nodesHtml:String = ni._2.map( cn => {
        // Set up showHide
        val openHtml = """<div class="cite_showhide_header cite_showhide_closed">"""
        // Get the node HTML
        val nodeHtml = HtmlWriter.writeCitableNode(cn._1)
        val closeHtml = """</div>"""
        // Wrap up the node
        val nodeContainerHtml = openHtml + "\n" + nodeHtml + "\n" + closeHtml
        //Get the image HTML
        val imageOpenHtml = {
          cn._2 match {
            case Some(u) => {
              getDSEImage(u, lib, config)
            }
            case None => {
              s"""<p class="cite_no_image_found">No image for ${HtmlWriter.writeUrn(cn._1.urn)}</p>"""
            }
          }
        }
        nodeContainerHtml + "\n" + imageOpenHtml
      }).mkString("\n")


      val htmlVec = Vector(catHtml, corpHtmlOpen, nodesHtml, corpHtmlClose)


      htmlVec.mkString("\n")
    }).mkString("\n")
    returnHtml
  }




  /** For a TBS, return HTML showing an overview image of the whole 
   *  surface, and all texts on that surface. */
  def htmlPageTextAndOverviewImage(
    surfaceUrn: Cite2Urn, 
    lib: CiteLibrary,
    config: Map[String, String]
  ): String = {

      // Get things where we can reach them
      val tr: TextRepository = lib.textRepository.get
      val corp: Corpus = tr.corpus
      val cat: Catalog = tr.catalog
      val colls: CiteCollectionRepository = lib.collectionRepository.get
      val rels: CiteRelationSet = lib.relationSet.get
      val dseVec: DseVector = DseVector.fromCiteLibrary(lib)

      // Get texts
      val corporaHtml: String = htmlPageBasicTexts(surfaceUrn, lib, config)

      // Get one image URN for this page 
      val imageUrn: Option[Cite2Urn] = dseVec.imageForTbs(surfaceUrn)

      // Get image
      val imageHtml = getOverviewImageHtml(surfaceUrn, lib, config)

      val between: String = s"""\n\n<h2>Texts on Surface ${surfaceUrn.objectComponent}</h2>\n\n"""

      // Return string 
      imageHtml + between + corporaHtml
  }




  /** Delivers a thumbnail overview image of the surface,
   *  and for each passage of text, the transcription
    * and the image-roi.
   */
  def htmlPageOverviewTextsRois(
    surfaceUrn: Cite2Urn, 
    lib: CiteLibrary,
    config: Map[String, String]
  ): String = {

      // Get things where we can reach them
      val tr: TextRepository = lib.textRepository.get
      val corp: Corpus = tr.corpus
      val cat: Catalog = tr.catalog
      val colls: CiteCollectionRepository = lib.collectionRepository.get
      val rels: CiteRelationSet = lib.relationSet.get
      val dseVec: DseVector = DseVector.fromCiteLibrary(lib)

      // Get one image URN for this page 
      val imageUrn: Option[Cite2Urn] = dseVec.imageForTbs(surfaceUrn)

      // Get image
      val imageHtml = getOverviewImageHtml(surfaceUrn, lib, config)

      // dividing overview image from text-passages
      val between: String = s"""\n\n<h2>Texts on Surface ${surfaceUrn.objectComponent}</h2>\n\n"""

      // text and image-rois
      val corporaHtml: String = htmlTextAndROIs(surfaceUrn, lib, config)

      // Return string 
      imageHtml + between + corporaHtml
  }



  /** Get HTML for a DSE image, with its caption */
  def getDSEImage(
      imageUrn: Cite2Urn, 
      lib: CiteLibrary,
      config: Map[String, String]
  ): String = {
    // Get things where we can reach them
    val cr: CiteCollectionRepository = lib.collectionRepository.get
    val colls: CiteCollectionRepository = lib.collectionRepository.get
    val rels: CiteRelationSet = lib.relationSet.get
    val dseVec: DseVector = DseVector.fromCiteLibrary(lib)


    // Get image
        // binary image model urn
    val bim = Cite2Urn("urn:cite2:hmt:binaryimg.v1:") 
        // get objects
    val bimObjs: Vector[CiteObject] = colls ~~ bim
    val bimCollProperty: Cite2Urn = bim.addProperty("collection")
    val bimProtocolProperty: Cite2Urn = bim.addProperty("protocol")
    val matchingBimObject: CiteObject = {
      bimObjs.filter( c => { 
        c.propertyValue(bimCollProperty) == imageUrn.dropSelector 
      }).filter( c => {
        c.propertyValue(bimProtocolProperty) == "iiifApi"
      }).head
    }

    val pathPropUrn: Cite2Urn  = matchingBimObject.urn.addProperty("path")
    val path: String = matchingBimObject.propertyValue(pathPropUrn).asInstanceOf[String]
    val urlPropUrn: Cite2Urn = matchingBimObject.urn.addProperty("url")
    val url: String = matchingBimObject.propertyValue(urlPropUrn).asInstanceOf[String]
    val imgService =  IIIFApi(url, path)
    val imageViewerUrl:String = config("imageViewerUrl")
    // how high the image resolution should be
    val overviewWidth: Int = config("imageResolution").toInt
    val imageThumbHtml: String = imgService.linkedHtmlImage(u = imageUrn, maxWidth = Some(overviewWidth), viewerUrl = imageViewerUrl)

    // Get image metadata
    //val imgObj = (colls ~~ imageUrn).head
    val imgObj = cr.objects.objectMap(imageUrn.dropExtensions)
    val imgCat = (colls.catalog ~~ imageUrn).collections.head
    val imageMetadataHtml = HtmlWriter.writeCiteObject(imgObj, imgCat)
    val imageHtml = s"""
    <div class="cite_image_dseimage">
        <div class="cite_image_thumb">${imageThumbHtml}</div>
        <div class="cite_image_data">${imageMetadataHtml}</div>
    </div>
    """
    imageHtml
  }



  /** Get HTML for an overview image thumbnail for a TBS, with its caption */
  def getOverviewImageHtml(
      surfaceUrn: Cite2Urn, 
      lib: CiteLibrary,
      config: Map[String, String]
    ): String = {

    // Get things where we can reach them
    val tr: TextRepository = lib.textRepository.get
    val corp: Corpus = tr.corpus
    val cat: Catalog = tr.catalog
    val colls: CiteCollectionRepository = lib.collectionRepository.get
    val rels: CiteRelationSet = lib.relationSet.get
    val dseVec: DseVector = DseVector.fromCiteLibrary(lib)

    // Get one image URN for this page 
    val imageUrnOption: Option[Cite2Urn] = dseVec.imageForTbs(surfaceUrn)

    val imageThumbHtml: String = {
      imageUrnOption match {
        case Some(imageUrn) => {
          // Get image
              // binary image model urn
          val bim = Cite2Urn("urn:cite2:hmt:binaryimg.v1:") 
              // get objects
          val bimObjs: Vector[CiteObject] = colls ~~ bim
          val bimCollProperty: Cite2Urn = bim.addProperty("collection")
          val bimProtocolProperty: Cite2Urn = bim.addProperty("protocol")
          val matchingBimObject: CiteObject = {
            bimObjs.filter( c => { 
              c.propertyValue(bimCollProperty) == imageUrn.dropSelector 
            }).filter( c => {
              c.propertyValue(bimProtocolProperty) == "iiifApi"
            }).head
          }

          val pathPropUrn: Cite2Urn  = matchingBimObject.urn.addProperty("path")
          val path: String = matchingBimObject.propertyValue(pathPropUrn).asInstanceOf[String]
          val urlPropUrn: Cite2Urn = matchingBimObject.urn.addProperty("url")
          val url: String = matchingBimObject.propertyValue(urlPropUrn).asInstanceOf[String]
          val imgService =  IIIFApi(url, path)
          val imageViewerUrl:String = config("imageViewerUrl")
          // how high the image resolution should be
          val overviewWidth: Int = config("imageResolution").toInt
          imgService.linkedHtmlImage(u = imageUrn, maxWidth = Some(overviewWidth), viewerUrl = imageViewerUrl)
        }
        case None => {
          s"""<span class="cite_no_image_found">No Image Found for ${surfaceUrn}.</span>"""
        }
      }
    } // end imageThumbHtml

   

    // Get image metadata
    val imageMetadataHtml: String = {
      imageUrnOption match {
        case Some(imageUrn) => {
          val imgObj = (colls ~~ imageUrn).head
          val imgCat = (colls.catalog ~~ imageUrn).collections.head
          HtmlWriter.writeCiteObject(imgObj, imgCat)
        }
        case None => {
          ""
        }
      }
    }
    val imageHtml = s"""
    <div class="cite_image_overview">
        <div class="cite_image_thumb">${imageThumbHtml}</div>
        <div class="cite_image_/data">${imageMetadataHtml}</div>
    </div>
    """
    imageHtml
  }



  /** Return a Vector[Corpus] containing the text-passages
   *  appearing on a Text-Bearing-Surface, grouped by Text.
   *  This will be the basis for subsequent processing.
   */
  def getTextCorporaForSurface(
    surfaceUrn: Cite2Urn, 
    lib: CiteLibrary,
    config: Map[String, String]
  ): Vector[Corpus] = {
      // Get things where we can reach them
      val tr: TextRepository = lib.textRepository.get
      val corp: Corpus = tr.corpus
      val cat: Catalog = tr.catalog
      val colls: CiteCollectionRepository = lib.collectionRepository.get
      val rels: CiteRelationSet = lib.relationSet.get
      val dseVec: DseVector = DseVector.fromCiteLibrary(lib)

      // Get the specific surface as a CITE Object
      val surfaceObj: CiteObject = colls.citableObject(surfaceUrn)
      val surfaceLabel: String = surfaceObj.label
      println("\tGetting text urns…")

      // What passages are on it?
      val textUrns: Vector[CtsUrn] = dseVec.textsForTbs(surfaceUrn)

      // Get the text passages
      println("\ttwiddling text urns…")
      //val textCorpus: Corpus = corp ~~ sortedUrns
      val textCorpus: Corpus = corp ~~ textUrns

      // Group by text
      println("\tgrouping by text…")
      val byText: Vector[Corpus] = HtmlWriter.groupCorpusByText(textCorpus)
      byText
  }


}

