package com.docnet.network

import com.docnet.lexicology.Lexis
import scala.collection._
import grizzled.slf4j.Logger

class Network(val lexis: Lexis) {
	val log = Logger[this.type]
	val matrix = mutable.Map.empty[Int, mutable.Map[Document, Int]]
	
	def search(query: String): List[Document] = {
		val docToWeightOption = lexis.find(query).map{ lexeme => matrix(lexeme.id) }
		docToWeightOption match {
			case None => List.empty[Document]
			case Some(docToWeight) => {
				docToWeight.toList sortBy { _._2 } map { _._1 } reverse
			}
		}
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
