package com.docnet.network

import com.docnet.lexicology.Lexis
import scala.collection._
import grizzled.slf4j.Logger

class Network(val lexis: Lexis) {
	val log = Logger[this.type]
	val lexIdToDocument: mutable.Map[Int, Set[Document]] = mutable.Map.empty[Int, Set[Document]]
	
	def search(query: String): Set[Document] = {
		lexis.find(query) match {
			case Some(q) => lexIdToDocument(q.id)
			case None => Set.empty[Document]
		}
	}
	
	def add(doc: Document) = {
		log.debug("Network.add("+ doc +")")
		
		doc.tokens.foreach { token =>
			log.debug("doc token="+ token)
			
			val lexId = lexis.index(token).id
			log.debug("lexId="+ lexId)
			
			val docs = lexIdToDocument.getOrElse(lexId, Set.empty[Document]) + doc
			lexIdToDocument(lexId) = docs 
		}
	}
	
	def tokens(): Set[String] = { 
		log.debug("tokens()")
		lexIdToDocument.keys.foreach { k => log.debug("lexemeId="+ k) }
		lexIdToDocument.keySet.map{ i => lexis.find(i).get.token }
	}

}
