package com.docnet

import com.docnet.lexicology.Lexis

class Network(val lexis: Lexis) {
	
	var documents: List[Document] = Nil
	
	def search(query: String): List[Document] = {
		documents filter { _.contains(query) }		
	}
	
	def add(doc: Document) = {
		documents = doc :: documents
	}
	
	def tokens(): List[String] = { 
		"token"::Nil 
	}
}
