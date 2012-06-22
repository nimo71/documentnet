package com.docnet.network

import com.docnet.lexicology.Lexis

class Document(val tokens: List[String]) {
	
	override def equals(that: Any): Boolean = {
		that match {
			case d: Document => d.tokens == this.tokens
			case _ => false
		}
	}
	
	override def hashCode(): Int = tokens hashCode
	
}