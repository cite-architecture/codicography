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

object hmt {



	/** Delivers a thumbnail overview image of the surface,
   *  and for each passage of Iliadic text: 
   *       the transcription
   *       and the image-roi.
   *       and categorized scholia, with image-roi
   */
  def hmtFacsimile(
    surfaceUrn: Cite2Urn, 
    lib: CiteLibrary,
    config: Map[String, String],
    rels: CiteRelationSet
  ): String = {

      // Get things where we can reach them
      val tr: TextRepository = lib.textRepository.get
      val corp: Corpus = tr.corpus
      val cat: Catalog = tr.catalog
      val colls: CiteCollectionRepository = lib.collectionRepository.get
      val dseVec: DseVector = DseVector.fromCiteLibrary(lib)

      // Get one image URN for this page 
      val imageUrn: Option[Cite2Urn] = dseVec.imageForTbs(surfaceUrn)

      // Get image

      val imageHtml = content.getOverviewImageHtml(surfaceUrn, lib, config)

      // dividing overview image from text-passages
      val button: String = showAllButton
      val between: String = s"""\n\n<h2>Texts on Folio ${surfaceUrn.objectComponent}</h2>\n${showAllButton}\n"""
      val scholiaKey: String = {
        val stringVec = Vector(
          s"""<div id="hmt_scholia_key">Key to scholia:""",
          s"""<span class="hmt_scholia_6">Iliadic text</span>""",
          s"""<span class="hmt_scholia_1">${urnToScholiaLabel("msA")}</span>""",
          s"""<span class="hmt_scholia_2">${urnToScholiaLabel("msAext")}</span>""",
          s"""<span class="hmt_scholia_3">${urnToScholiaLabel("msAil")}</span>""",
          s"""<span class="hmt_scholia_4">${urnToScholiaLabel("msAim")}</span>""",
          s"""<span class="hmt_scholia_5">${urnToScholiaLabel("msAint")}</span>""",
          s"""<span class="hmt_scholia_0">Uncategorized</span></div>""")
        stringVec.mkString(" ")
      }

      //s""" <a id="hmt_citeAppLink" target=_blank href="http://www.homermultitext.org/hmt-digital/index.html?urn=${imageUrn.getOrElse("")}">Interactive CITE-App for this folio</a></div>"""

      // The meat of the content is here
      val corporaHtml: String = hmtIliadText(surfaceUrn, lib, config, rels)

      // Javascript necessary for building an image-map
      val allTexts: Vector[DsePassage] = {
        dseVec.passages.filter(_.surface == surfaceUrn)
      }
      val jsData: Vector[(String, Vector[(String, String, String, String)])] = {
        val textAndImageTuples: Vector[(CtsUrn, Cite2Urn)] = {
          allTexts.map( at => {
            val textU = at.passage
            val imageU = at.imageroi 
            (textU, imageU)
          })
        }
        val groupedByText: Vector[(CtsUrn, Vector[(CtsUrn, Cite2Urn)])] = {
          textAndImageTuples.groupBy(_._1).toVector
        }
        val textAndImages: Vector[(CtsUrn, Vector[Cite2Urn])] = {
          groupedByText.map( gt => {
            val textU: CtsUrn = gt._1
            val imageVec: Vector[Cite2Urn] = gt._2.map(_._2)
            (textU, imageVec)
          })
        }
        val classAndRois: Vector[(String, Vector[(String, String, String, String)])] = {
          textAndImages.map( ti => {
            val textU: CtsUrn = ti._1
            val roiVec: Vector[Cite2Urn] = ti._2
            val textString: String = urnToScholiaClass(textU) 
            val roiStrings: Vector[String] = {
              roiVec.map( _.objectExtensionOption.getOrElse(""))
            }
            val filteredString: Vector[String] = {
              roiStrings.filter(_.split(",").toVector.size == 4)
            }
            val splittedStrings: Vector[(String, String, String, String)] = {
              val mappedStrings: Vector[Vector[String]] = {
                filteredString.map( _.split(",").toVector)
              }
              val fourPlVec: Vector[(String, String, String, String)] = {
                mappedStrings.map( ms => {
                  ( ms(0), ms(1), ms(2), ms(3) )
                })
              }
              fourPlVec
            }
            (textString, splittedStrings)
          })
        }
        classAndRois
      }
      val mapJs = imageMapJS(jsData)

      // Return string 
      imageHtml + scholiaKey +  between + corporaHtml + mapJs
  }

  def hmtIliadText(
    surfaceUrn: Cite2Urn, 
    lib: CiteLibrary,
    config: Map[String, String],
    rels: CiteRelationSet
  ): String = {

    // Get things where we can reach them
    val tr: TextRepository = lib.textRepository.get
    val corp: Corpus = tr.corpus
    val cat: Catalog = tr.catalog
    val colls: CiteCollectionRepository = lib.collectionRepository.get
    val dseVec: DseVector = DseVector.fromCiteLibrary(lib)

    val iliadUrn: CtsUrn = CtsUrn("urn:cts:greekLit:tlg0012.tlg001:")
    val scholiaUrn: CtsUrn = CtsUrn("urn:cts:greekLit:tlg5026:")
    val commentaryVerb:Cite2Urn = Cite2Urn("urn:cite2:cite:verbs.v1:commentsOn")

    val allTexts: Vector[CtsUrn] = {
      dseVec.textsForTbs(surfaceUrn)
    }

    val iliadTexts: Vector[CtsUrn] = allTexts.filter( u => u ~~ iliadUrn )

    val scholiaTexts: Vector[CtsUrn] = allTexts.filter( u => u ~~ scholiaUrn )

    val iliadScholiaMap: Map[CtsUrn, Vector[CtsUrn]] = {
      val commentsForPage: CiteRelationSet = {
        val commentRelations: Set[CiteTriple] = rels.verb(commentaryVerb).relations.filter( r => {
          iliadTexts.contains(r.urn2.asInstanceOf[CtsUrn])
        })
        CiteRelationSet(commentRelations)
      } 
      iliadTexts.map( u => {
        val scholia: Vector[CtsUrn] = commentsForPage.relations.filter( r => {
          r.urn2 == u
        }).map( _.urn1.asInstanceOf[CtsUrn]).toVector
        ( u -> scholia)
      }).toMap
    }

    val leftoverScholia: Vector[CtsUrn] = {
      val usedScholia: Vector[CtsUrn] = {
        iliadScholiaMap.toVector.map(_._2).flatten
      }
      scholiaTexts.diff(usedScholia)
    } 

    utilities.showMe(leftoverScholia)

    // Sort IliadLines
    val sortedIliadMap: Vector[CtsUrn] = {
      corp.sortPassages(iliadTexts)
    }

    utilities.showMe(sortedIliadMap)

    /* for each Iliad line…
       … turn the scholia into a Vector[Corpus]
    */
    val fleshedOutScholia: Vector[(CtsUrn, Vector[Corpus])] = {
      sortedIliadMap.map( il => {
        val sortedScholiaUrns: Vector[CtsUrn] = corp.sortPassages(iliadScholiaMap(il))
        val allScholiaCorp = Corpus(
          corp.nodes.filter( n =>  sortedScholiaUrns.contains(n.urn))
        )
        val dividedCorps: Vector[Corpus] = allScholiaCorp.chunkByText.sortBy( c => {
            c.nodes.head.urn.workOption.getOrElse("_")
        })
        (il, dividedCorps)
      })
    }

    // dividing overview image from text-passages
    val between: String = s"""\n\n<h2>Texts on Folio ${surfaceUrn.objectComponent}</h2>\n\n"""

    // Write IliadLines
    val iliadHtml: String = {
      if (fleshedOutScholia.size > 0) {
        getIliadHtml(fleshedOutScholia, surfaceUrn, lib, config)
      } else {
        s"<h2>No <i>Iliad</i> text found for ${surfaceUrn}."
      }
    }

    // Write leftoverScholia
    val leftoverScholaHtml: String = {
      val opener = s"""<h2>Unaligned Texts</h2>
      <p>These texts are mapped to folio ${surfaceUrn}, but are neither Iliadic lines, nor indexed as commenting on any particular Iliadic line. </p>
      """
      if (leftoverScholia.size == 0) ""
      else {
        val leftoverNodes = leftoverScholia.map( los => {
          val littleCorp: Corpus = corp ~~ los
          littleCorp.nodes.map( lcn => {
            nodeAndImage(lcn, lib, config)
          }).mkString("\n")
        }).mkString("\n")
        opener + leftoverNodes
      }
    }

    iliadHtml + leftoverScholaHtml

  }

  def getIliadHtml(
    textMap: Vector[(CtsUrn, Vector[Corpus])],
    surfaceUrn: Cite2Urn,
    lib: CiteLibrary,
    config: Map[String, String]
  ): String = {

    val tr: TextRepository = lib.textRepository.get
    val corp: Corpus = tr.corpus
    val cat: Catalog = tr.catalog
    val colls: CiteCollectionRepository = lib.collectionRepository.get
    val dseVec: DseVector = DseVector.fromCiteLibrary(lib)

    val iliadCatalogHtml: String = {
      val catEntry: CatalogEntry = {
        val u: CtsUrn = textMap.head._1.dropPassage
        tr.catalog.entriesForUrn(u).head
      }
      HtmlWriter.writeCtsCatalogEntry(catEntry)
    }

    val iliadLines: String = {
      textMap.map( tm => {
        val n: Option[CitableNode] = corp.nodes.find(_.urn == tm._1)
        n match {
          case Some(node) => {
            val nodeHtml: String = nodeAndImage(node, lib, config)
            val commentaryOpen: String = """<div class="ohco2_commentedPassage">"""
            val commentaryClose: String = "</div>"

            // Get HTML for scholia
            val scholiaHtml: String = groupedScholia( tm._2, surfaceUrn, lib, config)

            commentaryOpen + nodeHtml + scholiaHtml + commentaryClose + "\n"

          }
          case None => {
            println(s"No text found in corpus for ${tm._1}")
            ""
          }
        }
      }).mkString("\n")
    }

    iliadCatalogHtml + iliadLines

  }

  def nodeAndImage(node: CitableNode, lib: CiteLibrary, config: Map[String, String]): String = {

    val tr: TextRepository = lib.textRepository.get
    val dseVec: DseVector = DseVector.fromCiteLibrary(lib)

    val iliadUrn = CtsUrn("urn:cts:greekLit:tlg0012.tlg001:")

    val imageForText: Option[Cite2Urn] = {
      try {
        val thisImage: Option[Cite2Urn] = dseVec.imageWRoiForText(node.urn)
        thisImage
      } catch {
          case e: Exception => {
            try {
              val thisImage: Option[Cite2Urn] = dseVec.imageWRoiForText(node.urn.collapsePassageBy(1))
              thisImage
            } catch {
              case e: Exception => None
            }
          }
      }
    }

    // Set up showHide
    val openHtml = """<div class="cite_showhide_header cite_showhide_closed">"""
    // Get the node HTML
    val genericHtml = HtmlWriter.writeCitableNode(node)
    val nodeHtml = {
      if (node.urn ~~ iliadUrn) {
        genericHtml.replaceAll("ohco2_citableNodeContainer","ohco2_citableNodeContainer hmt_iliad_node")
      } else genericHtml
    }
    val closeHtml = """</div>"""
    // Wrap up the node
    val nodeContainerHtml = openHtml + "\n" + nodeHtml + "\n" + closeHtml
    //Get the image HTML
    val imageOpenHtml = {
      imageForText match {
        case Some(u) => {
          content.getDSEImage(u, lib, config)
        }
        case None => {
          s"""<p class="cite_no_image_found">No image for ${HtmlWriter.writeUrn(node.urn)}</p>"""
        }
      }
    }
    nodeContainerHtml + "\n" + imageOpenHtml
  }

  def groupedScholia(
    corpVec: Vector[Corpus],
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
    val surfaceTexts: Vector[Corpus] = corpVec


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
      val commentRevealOpen = s"""<div class="ohco2_commentBlock hmt_scholia_block ${urnToScholiaClass(ni._1.urn)}" data-commentBlockLabel="${ni._2.size}">"""
      val corpHtmlOpen = s"""<h2>Commentary Texts</h2>\n<div class="ohco2_versionCorpus">"""
      val corpHtmlClose = """</div></div>"""

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
              content.getDSEImage(u, lib, config)
            }
            case None => {
              s"""<p class="cite_no_image_found">No image for ${HtmlWriter.writeUrn(cn._1.urn)}</p>"""
            }
          }
        }
        nodeContainerHtml + "\n" + imageOpenHtml
      }).mkString("\n")


      val htmlVec = Vector(commentRevealOpen, catHtml, corpHtmlOpen, nodesHtml, corpHtmlClose)


      htmlVec.mkString("\n")
    }).mkString("\n")
    returnHtml
  }

  def urnToScholiaLabel(u: CtsUrn): String =  {
    urnToScholiaLabel(u.workOption.getOrElse(""))
  }

  def urnToScholiaLabel(s: String): String =  {
    s match {
      case "msA" => "Main Scholia"
      case "msAext" => "Exterior Scholia"
      case "msAil" => "Interlinear Scholia"
      case "msAim" => "Intra-marginal Scholia"
      case "msAint" => "Interior Scholia"
      case _ => "Scholia: no version specified"
    }
  }

  def urnToScholiaClass(u: CtsUrn): String =  {
    u.workOption match {
      case None => " hmt_scholia_0 "
      case Some(v) => {
        v match {
          case "msA" => " hmt_scholia_1 "
          case "msAext" => " hmt_scholia_2 "
          case "msAil" => " hmt_scholia_3 "
          case "msAim" => " hmt_scholia_4 "
          case "msAint" => " hmt_scholia_5 "
          case "tlg001" => "hmt_scholia_6"
          case _ => " hmt_scholia_0 "
        }
      }
    }
  }

  def  showAllButton: String = {
    """
<span class="hmtButton" id="hmt_showAllButton">Show Everything</span>
<input type="checkbox" name="hmtZoomImage" id="hmtZoomImage" ><label for="hmtZoomImage">Zoom Images on Hover</label></input>
"""
  }

  def imageMapJS(imgMap: Vector[(String, Vector[(String, String, String, String)])]):String = {
    val jsOpen: String = """
<script type="text/javascript">
var imgOverlayROIs = new Array();
var temp = new Object();
"""
    val jsClose: String= """</script>"""

    val jsContent: String = {
      imgMap.map( im => {
        val thisClass = im._1
        val thisVec = im._2
        val writeObjects: String = thisVec.map( tv => {
          val l: String = tv._1.toString
          val t: String = tv._2.toString
          val w: String = tv._3.toString
          val h: String = tv._4.toString
          val str: String = {
            s"""
            temp = new Object();
            temp.class = "${thisClass}";
            temp.left = "${l}";
            temp.top = "${t}";
            temp.width = "${w}";
            temp.height = "${h}";
            imgOverlayROIs.push(temp);
            """
          }
          str
        }).mkString("\n")
        writeObjects
      }).mkString("\n")
    }
    jsOpen + jsContent + jsClose
  }

}