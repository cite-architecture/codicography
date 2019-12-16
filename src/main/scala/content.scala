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

  /** 
   *  For selecting a function to select surfaces
  */
  def selectionSwitcher( dseVec: DseVector, lib: CiteLibrary ): Vector[Cite2Urn] = {

      getSurfaceUrns(dseVec)

  }

	/** 
	 *  For selecting a function to attach content to each surface
	*/
	def contentSwitcher( surfaceUrn: Cite2Urn, lib: CiteLibrary, config: Map[String, String] ): String = {

			textAndThumb(surfaceUrn, lib, config)

	}



  /** Stupid selection, for testing: grab any 3 */
  def getSurfaceUrns( dseVec: DseVector ): Vector[Cite2Urn] = {
    dseVec.passages.map(_.surface).distinct.take(3)
  }

	/** Returns HTML for just the text-passages on a
	 * 	surface, grouped by text.
	 */
	def basicTexts(
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

       				

      val surfaceObj: CiteObject = colls.citableObject(surfaceUrn)
      val surfaceLabel: String = surfaceObj.label
      println(s"\n------\nWorking on ${surfaceUrn}\n------")
      println("\tGetting text urns…")
      val textUrns: Vector[CtsUrn] = dseVec.textsForTbs(surfaceUrn)
      println("\tsorting text urns…")
      val sortedUrns: Vector[CtsUrn] = corp.sortPassages(textUrns)
      println("\ttwiddling text urns…")
      val textCorpus: Corpus = corp ~~ sortedUrns
      println("\tgrouping by text…")
      val byText: Vector[Corpus] = HtmlWriter.groupCorpusByText(textCorpus)
      val corporaHtml: String = {
        byText.map( t => {
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

  def textAndThumb(
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
      val corporaHtml: String = basicTexts(surfaceUrn, lib, config)

      // Get one image URN for this page 
      val imageUrn: Cite2Urn = dseVec.imageForTbs(surfaceUrn)

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
      val overviewWidth: Int = config("imageOverviewWidth").toInt
      val imageThumbHtml: String = imgService.linkedHtmlImage(u = imageUrn, maxWidth = Some(overviewWidth), viewerUrl = imageViewerUrl)

      // Get image metadata
      val imgObj = (colls ~~ imageUrn).head
      val imgCat = (colls.catalog ~~ imageUrn).collections.head
      val imageMetadataHtml = HtmlWriter.writeCiteObject(imgObj, imgCat)
      val imageHtml = s"""
      <div class="cite_image_overview">
          <div class="cite_image_thumb">${imageThumbHtml}</div>
          <div class="cite_image_metadata">${imageMetadataHtml}</div>
      </div>
      """


      // Return string 
      imageHtml + "\n\n" + corporaHtml

  }

}

