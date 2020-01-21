package edu.furman.classics.codicography

import better.files._
import better.files.Dsl._
import better.files.File._  
import java.io.{File => JFile}
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import scala.annotation.tailrec
import edu.holycross.shot.citerelation._
import java.nio.charset.Charset

object utilities {

 	def deluxeRelationSet(lib: CiteLibrary): Option[CiteRelationSet] = {
      println("Building deluxe relations set…")
      lib.relationSet match {
         case Some(rs) => {
						println(s"rs.relations.size = ${rs.relations.size}")
            val u2RangeRelations:Vector[CiteTriple] = {
              rs.relations.filter( ct => {
                  ct.urn2 match {
                    case CtsUrn(_) => {
                      val u2:CtsUrn = ct.urn2.asInstanceOf[CtsUrn]
                      if (u2.isRange) {
                        // let's exclude any mixed-ranges
                        val citationDepth:Vector[Int] = u2.citationDepth
                        if (citationDepth.size != 2){ false }
                        else {
                          if (citationDepth(0) != citationDepth(1)) { 
                          	println(s"irrational range in relations: ${u2}")
                          	false 
                          }
                          else true
                        }
                        //true
                      } else {
                        false
                      }
                    }
                    case _ => false
                  } 
              }).toVector
            }
            println(s"u2RangeRelations = ${u2RangeRelations.size}")
            val u2ExpandedUrns:Vector[CiteTriple] = {
              lib.textRepository match {
                case None => Vector()
                case Some(tr) => {
                  u2RangeRelations.map(rr => {
                    val u2:CtsUrn = rr.urn2.asInstanceOf[CtsUrn]
                    val expandedU2:Vector[CtsUrn] = tr.corpus.validReff(u2)
                    val eU2 = expandedU2.map( eu2 => {
                        val tempRelation:Cite2Urn = rr.relation
                        val u1:Urn = rr.urn1
                        val tempCiteTriple:CiteTriple = CiteTriple(u1, tempRelation, eu2) 
                        tempCiteTriple
                    }).toVector
                    eU2
                  }).flatten 
                }
              } 
            }
            println(s"u2RangeRelations = ${u2RangeRelations.size}")
            val u1RangeRelations:Vector[CiteTriple] = {
              rs.relations.filter( ct => {
                  ct.urn1 match {
                    case CtsUrn(_) => {
                      val u1:CtsUrn = ct.urn1.asInstanceOf[CtsUrn]
                      if (u1.isRange) {
                        // let's exclude any mixed-ranges
                        val citationDepth:Vector[Int] = u1.citationDepth
                        if (citationDepth.size != 2){ false }
                        else {
                          if (citationDepth(0) != citationDepth(1)) { 
                          	println(s"irrational range in relations: ${u1}")
                          	false 
                          }
                          else true
                        }
                        //true
                      } else {
                        false
                      }
                    }
                    case _ => false
                  } 
              }).toVector
            }
            println(s"u1RangeRelations = ${u1RangeRelations.size}")
            val u1ExpandedUrns:Vector[CiteTriple] = {
              lib.textRepository match {
                case None => Vector()
                case Some(tr) => {
                  u1RangeRelations.map(rr => {
                    val u1:CtsUrn = rr.urn1.asInstanceOf[CtsUrn]
                    val expandedU1:Vector[CtsUrn] = tr.corpus.validReff(u1)
                    val eU1 = expandedU1.map( eu1 => {
                        val tempRelation:Cite2Urn = rr.relation
                        val u2:Urn = rr.urn2
                        val tempCiteTriple:CiteTriple = CiteTriple(eu1, tempRelation, u1) 
                        tempCiteTriple
                    }).toVector
                    eU1
                  }).flatten 
                }
              } 
            }
            println(s"u1ExpandedUrns = ${u1ExpandedUrns.size}")
            val newTriplesVec:Vector[CiteTriple] = u2ExpandedUrns ++ u1ExpandedUrns
            println(s"newTriplesVec = ${newTriplesVec.size}")
            val newTriplesSet:Set[CiteTriple] = newTriplesVec.toSet
            val allTriplesSet:Set[CiteTriple] = rs.relations ++ newTriplesSet
            println(s"original triples: ${rs.relations.size}")
            println(s"new triples: ${newTriplesSet.size}")
            println(s"deluxe triples: ${allTriplesSet.size}")
            val newCiteRelationSet:CiteRelationSet = CiteRelationSet(allTriplesSet)
            Some(newCiteRelationSet)
         }
         case None => None
      }
  }

	def showMe(v:Any):Unit = {
	  v match {
	  	case _:Corpus => {
	  		for ( n <- v.asInstanceOf[Corpus].nodes) {
	  			println(s"${n.urn}\t\t${n.text}")
	  		}	
	  	}
		  case _:Map[Any, Any] => {
		  	for ( (k, v) <- v.asInstanceOf[Map[Any, Any]] ) println(s""" "${k}" -> ${v}""")
		  }
	    case _:Vector[Any] => println(s"""\n----\n${v.asInstanceOf[Vector[Any]].mkString("\n")}\n----\n""")
	    case _:Iterable[Any] => println(s"""\n----\n${v.asInstanceOf[Iterable[Any]].mkString("\n")}\n----\n""")
	    case _ => println(s"\n-----\n${v}\n----\n")
	  }
	}

	def loadLibrary(fp:String):CiteLibrary = {
		val file:  File = File(fp)
		val tempString: String = file.contentAsString(charset = Charset.forName("UTF-8"))
		val library = CiteLibrary(tempString)
		library
	}

	def loadFile( fp: String ): Vector[String] = {
		val file: File = File(fp)
		file.lines.toVector
	}

	def saveStringVec(sv:Vector[String], filePath:String = "texts/", fileName:String = "temp.txt"):Unit = {
		val fp = filePath.toFile.createIfNotExists(true)
		val file = (fp/fileName).createIfNotExists()
		for (s <- sv){
			file.appendLine().append(s)
		}
	}

	def saveString(s:String, filePath:String = "texts/", fileName:String = "temp.txt"):Unit = {
		val fp = filePath.toFile.createIfNotExists(true)
		val file = (fp/fileName).createIfNotExists()
		file.overwrite(s)
	}

	def splitWithSplitter(text: String, puncs: String =  """[()\[\]·⸁.,; "?·!–—⸂⸃]"""): Vector[String] = {
		val regexWithSplitter = s"((?<=${puncs})|(?=${puncs}))"
		text.split(regexWithSplitter).toVector.filter(_.size > 0)
	}

	val punctuation: String = """[“”“‘()‘’'\[\]·_…⸁.,:; "?·!⸂⸃–—-]"""
	val alphabet: String = """[A-Za-z]"""


	// source: https://gist.github.com/sebleier/554280
	val stopWords: Vector[String] = Vector("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "you're", "you've", "you'll", "you'd", "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "she's", "her", "hers", "herself", "it", "it's", "its", "itself", "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "that'll", "these", "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very", "s", "t", "can", "will", "just", "don", "don't", "should", "should've", "now", "d", "ll", "m", "o", "re", "ve", "y", "ain", "aren", "aren't", "couldn", "couldn't", "didn", "didn't", "doesn", "doesn't", "hadn", "hadn't", "hasn", "hasn't", "haven", "haven't", "isn", "isn't", "ma", "mightn", "mightn't", "mustn", "mustn't", "needn", "needn't", "shan", "shan't", "shouldn", "shouldn't", "wasn", "wasn't", "weren", "weren't", "won", "won't", "wouldn", "wouldn't")

}
