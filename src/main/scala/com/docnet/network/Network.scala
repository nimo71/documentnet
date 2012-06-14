package com.docnet.network

import com.docnet.lexicology.Lexis
import scala.collection._
import grizzled.slf4j.Logger

/**
 * Represent a set of Documents as a Network of the Lexemes that make up the 
 * Documents.
 */
class Network(val lexis: Lexis) {
	val log = Logger[this.type]
	
	val docCxn = mutable.Map.empty[Int, mutable.Map[Document, Int]]
	val lexCxn = mutable.Map.empty[Int, mutable.Map[Int, Int]]
	
	/**
	 * Search the Network for Documents matching the given query
	 */
	def search(query: String): List[Document] = {
		
		def getDocumentsWithWeights(token: String): Option[Map[Document, Int]] = 
				lexis.find(token).map{ lexeme => docCxn(lexeme.id) }
		
		val firing = mutable.Map.empty[Int, Int]
		val results = mutable.Map.empty[Document, Int]
		
		query.split(' ').foreach { token =>  
			
			lexis.find(token).map { lexeme => 
				
				docCxn.getOrElse(lexeme.id, Map.empty[Document, Int]).foreach { docToWeight =>
					val doc = docToWeight._1
					val weight = docToWeight._2 * (firing.getOrElse(lexeme.id, 0) + 1)
					results += (doc -> (results.getOrElse(doc, 0) + weight))
				}
				
				lexCxn.getOrElse(lexeme.id, Map.empty[Int, Int]).foreach { lexToWeight => 
					val lex = lexToWeight._1
					val weight = lexToWeight._2
					firing += (lex -> (firing.getOrElse(lex, 0) + weight))
				}
			} 
				
		}
		results.toList sortBy { _._2 } map { _._1 } reverse
	}
	
	/**
	 * Add a Document to this Network
	 */
	def add(doc: Document) = {
		var last = 0;
		doc.tokens.foreach { token =>
			val lexId = lexis.index(token).id
			
			val docWeight = docCxn.getOrElse(lexId, mutable.Map(doc -> 0))
			val docWeightValue = docWeight.getOrElse(doc, 0)
			docWeight += (doc -> (docWeightValue + 1))
			docCxn += (lexId -> docWeight)
			
			if (last > 0) {
				val lexWeight = lexCxn.getOrElse(last, mutable.Map(lexId -> 0))
				val lexWeightValue = lexWeight.getOrElse(lexId, 0)
				lexWeight += (lexId -> (lexWeightValue + 1))
				lexCxn += (last -> lexWeight)
				last = lexId
			}
		}
	}
	
	/**
	 * 
	 */
	def tokens(): Set[String] = docCxn.keySet.map{ i => lexis.find(i).get.token }

}
