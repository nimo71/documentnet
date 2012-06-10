package com.docnet.lexicology

import org.specs2.mutable._
import org.specs2.specification._

class LexisSpec extends Specification {

	"An empty lexis" should {
		
		"return None when finding an unknown token" in new emptyLexis {
			empty.find("test") must beNone
		}
		
		"return a Lexime after indexing a token" in new emptyLexis {
			val lexeme = empty index "a"
			lexeme must not be null
			lexeme must not be None
			lexeme must beAnInstanceOf[Lexeme]
		}
		
		"return none when finding the route of a token" in new emptyLexis {
			val route = "route"
			empty index route +"test"
			empty find route must beNone
		}
	} 
  
	"An all same root lexis" should {
		
		"contain a lexime for each token" in new allSameRootTokens {
			sameRootLexis.lexemeCount() must be equalTo(tokens.length)
		}
		
		"contain a node for each different character" in new allSameRootTokens {
			val lengthOfLongestToken = tokens.foldLeft(0)((i, s) => { i max s.length() } )
			sameRootLexis.nodeCount() must be equalTo(lengthOfLongestToken)
		}
		
		"return a lexeme when finding a matching token" in new allSameRootTokens {
			val token = "123"
			val found = sameRootLexis find token 
			found must beAnInstanceOf[Some[Lexeme]]
			found.get.token() must be equalTo token
		}
		
		"return None when finding a none matching token" in new allSameRootTokens {
			sameRootLexis find "nomatch" must beNone
		}
	}
	
	"An all different root lexis" should {
		
		"contain a lexime for each token" in new allDifferentRootTokens {
			differentRootLexis.lexemeCount() must be equalTo(tokens.length)
		}
		
		"have a node for each of the sum of the characters in each token" in new allDifferentRootTokens {
			differentRootLexis.nodeCount() must be equalTo(tokens.map { _.length() } sum)
		}
	}
	
	trait emptyLexis extends Scope {
		lazy val empty = Lexis()
	}
	
	trait allSameRootTokens extends Scope {
		lazy val tokens = "1"::"12"::"123"::"1234"::"12345"::Nil
		lazy val sameRootLexis = Lexis()
		tokens foreach(sameRootLexis index _)
	}
	
	trait allDifferentRootTokens extends Scope {
		lazy val tokens = "54321"::"4321"::"321"::"21"::"1"::Nil
		lazy val differentRootLexis = Lexis()
		tokens foreach(differentRootLexis index _)
	}
}