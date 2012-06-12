package com.docnet.network

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import com.docnet.lexicology.Lexis
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NetworkSpec extends Specification {

	"An empty document net" should {
		
		"return None when searching for documents containing a given token" in new emptyNetwork {
			emptyNetwork search "query" must empty
		}
		
		"contain only the lexemes of any single document added" in new emptyNetwork {
			val token = "token"
			emptyNetwork add new Document(token::Nil)
			
			emptyNetwork.tokens() must contain (token)
			emptyNetwork.tokens() must haveLength(1)
		}
		
		"contain the lexemes of all the documents added" in new emptyNetwork {
			val token1 = "one"
			val token2 = "two"
			emptyNetwork add new Document(token1::Nil)
			emptyNetwork add new Document(token2::Nil)
			
			lexis.lexemeCount() must beEqualTo(2)
			emptyNetwork.tokens() must contain(token1, token2).only
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
			singleTokenNetwork.lexis.find(token).get.token() must beEqualTo(token)
		}
	}
	
	"A network with 2 documents" should {
		
		"return no documents if the search query tokens are not contained in either document" in new twoDocumentNetwork {
			network.search("unknown") must be empty
		}
		
		"return one document if a search token is only contained in one document" in new twoDocumentNetwork {
			network.search(doc2Token) must contain (doc2).only
		}

		"return both documents if a search token is contained in both documents" in new twoDocumentNetwork {
			network.search(bothToken) must contain (doc1, doc2).only
		}
	
		"return documents in order of relevance as a result of a search query" in new moreRelevantDocumentNetwork {
			network.search(token1) must contain (doc1, doc2).inOrder.only
			network.search(token2) must contain (doc2, doc1).inOrder.only
		}
	}
	
	// when a search query is made 
	// - order the search results with the most relevant document first
	// - consider the order of the search terms
	
	trait emptyNetwork extends Scope {
		lazy val lexis = new Lexis()
		lazy val emptyNetwork = new Network(lexis)
	}
	
	trait singleTokenSingleDocumentNetwork extends Scope {
		lazy val token = "token"
		lazy val lexis = new Lexis()
		lazy val singleTokenDocument = new Document(token::Nil)
		lazy val singleTokenNetwork = new Network(lexis)
		singleTokenNetwork.add(singleTokenDocument)
	}
	
	trait twoDocumentNetwork extends Scope {
		lazy val lexis = new Lexis()
		lazy val bothToken = "one"
		lazy val doc2Token = "two"
		lazy val doc1 = new Document(bothToken::Nil)
		lazy val doc2 = new Document(bothToken::doc2Token::Nil)
		lazy val network = new Network(lexis)
		network.add(doc1)
		network.add(doc2)
	}
	
	trait moreRelevantDocumentNetwork extends Scope {
		lazy val token1 = "one"
		lazy val token2 = "two"
		lazy val doc1 = new Document(token1::token1::token2::Nil)
		lazy val doc2 = new Document(token1::token2::token2::Nil)
		lazy val network = new Network(new Lexis())
		network.add(doc1)
		network.add(doc2)
	}
	
}