package com.docnet

import java.io.Reader
import java.io.StringReader
import org.specs2.mutable._
import org.specs2.specification._
import com.docnet.lexicology.Lexis

class NetworkSpec extends Specification {

	"An empty document net" should {
		
		"return None when searching for documents containing a given token" in new emptyNetwork {
			emptyNetwork search "query" must empty
		}
		
		"contain only the lexemes of any single document added" in new emptyNetwork {
			emptyNetwork add new Document("token")
			emptyNetwork.tokens().length must be equalTo(1)
		}
	} 
	
	"A network with a single document" should {
		
		"return the document if a query matches a documents token" in new singleTokenSingleDocumentNetwork {
			val resultDoc = singleTokenNetwork search token 
			resultDoc must contain(singleTokenDocument)
		} 
		
		"return none if no match in document" in new singleTokenSingleDocumentNetwork {
			val resultDoc = singleTokenNetwork search "unknownToken"
			resultDoc must be empty
		}
		
		"use a lexis to represent the tokens in the document" in new singleTokenSingleDocumentNetwork {
			singleTokenNetwork.lexis.find(token) must beSome(token)
		}
	}
	
	// consider a search specification
	
	// a network with 2 documents should
	// - return both documents if a query is made that is contained in them both
	// - return None if a query is made using tokens that aren't in either
	// - return one document if a query is made using tokens that are only in one
	
	// when a search query is made 
	// - order the search results with the most relevant document first
	// - consider the order of the search terms
	
	trait emptyNetwork extends Scope {
		lazy val emptyNetwork = new Network(Lexis());
	}
	
	trait singleTokenSingleDocumentNetwork extends Scope {
		lazy val token = "token"
		lazy val lexis = Lexis()
		lazy val singleTokenDocument = new Document(token)
		lazy val singleTokenNetwork = new Network(lexis)
		singleTokenNetwork.add(singleTokenDocument)
	}
	
}