package com.docnet.network

import com.docnet.lexicology._
import scala.collection._
import grizzled.slf4j.Logger

/**
 * Represent a set of Documents as a Network of the Lexemes that make up the 
 * Documents.
 */
class Network(val lexis: Lexis) {
	val log = Logger[this.type]
	
	val root = new RootNode(lexis)
	val nodes = mutable.ListBuffer.empty[Node]
	
	def add(document: Document) {
		val addNodes = mutable.ListBuffer.empty[Node]
		val docNode = DocumentNode(document)
		addNodes += docNode
		var last: Node = root
		document.tokens.foreach { token =>	
			val lexeme = lexis.index(token)
			val lexNode = findNode(lexeme, addNodes) match { 
				case Some(ln @ LexemeNode(_, _)) => ln
				case _ =>
					val lexId = lexis.index(token).id
					val ln = new LexemeNode(lexId, docNode)
					addNodes += ln
					ln
			}
			lexNode.connect(docNode)
			last.connect(lexNode)
			last = lexNode
		}
		
		nodes ++= addNodes
	}
	
	private def findNode(lexeme: Lexeme, inNodes: mutable.ListBuffer[Node]): Option[Node] = {
		inNodes.find {
			_ match {
				case LexemeNode(lexId, _) => lexId == lexeme.id
				case _ => false
			}
		}
	}
	
	/**
	 * 
	 */
	def tokens(): Set[String] = { 
		nodes.foldLeft(Set.empty[String]) (
			(acc, node) => {
				node match {
					case LexemeNode(lexId, _) => acc + lexis.find(lexId).get.token
					case _ => acc
				}
			}  
		)
	}

	/**
	 * Search the Network for Documents matching the given query
	 */
	def search(query: String): List[Document] = {
		val firing = new Firing(this)
		firing fire(query.split(' '))
		firing results
	}
	
	def findNodes(token: String): List[Node] = {
		lexis.find(token) match {
			case Some(lexeme) => nodes.filter { 
				_ match {
						case LexemeNode(lexId, _) => lexeme.id == lexId
						case _ => false
					}
				}.toList
			case None => List.empty[Node]
		}
	}		
}
