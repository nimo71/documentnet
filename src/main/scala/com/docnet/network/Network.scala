package com.docnet.network

import com.docnet.lexicology.Lexis
import scala.collection._
import grizzled.slf4j.Logger

class Network(val lexis: Lexis) {
	val log = Logger[this.type]
	val matrix = mutable.Map.empty[Int, mutable.Map[Document, Int]]
	
	def search(query: String): List[Document] = {
		
		def getDocumentsWithWeights(token: String): Option[Map[Document, Int]] = 
				lexis.find(token).map{ lexeme => matrix(lexeme.id) }
			
		val results = mutable.Map.empty[Document, Int]
		
		query.split(' ').foreach { 
			getDocumentsWithWeights(_).map { docsToWeights =>	
				docsToWeights.foreach { docToWeight => 
					val doc = docToWeight._1
					val weight = docToWeight._2
					results += (doc -> (results.getOrElse(doc, 0) + weight))
				}
			}	
		}
		results.toList sortBy { _._2 } map { _._1 } reverse
	}
	
	def add(doc: Document) = 
		doc.tokens.foreach { token =>
			val lexId = lexis.index(token).id
			
			val docWeight = matrix.getOrElse(lexId, mutable.Map(doc -> 0))
			val weight = docWeight.getOrElse(doc, 0)
			docWeight += (doc -> (weight + 1))
			matrix += (lexId -> docWeight)
		}
	
	def tokens(): Set[String] = matrix.keySet.map{ i => lexis.find(i).get.token }

}
