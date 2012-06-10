package com.docnet

import com.docnet.lexicology.Lexis

class Document(val tokens: String) {
	
	def contains(query: String): Boolean = {
		tokens == query
	}
}