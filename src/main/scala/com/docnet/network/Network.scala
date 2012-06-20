package com.docnet.network

import com.docnet.lexicology._
import scala.collection._
import grizzled.slf4j.Logger

private[network] sealed abstract case class Node() {
	var connections = mutable.ListBuffer.empty[Connection]
	
	def connect(to: Node) {
		getConnection(to) match {
			case Some(cxn) => {
				connections -= cxn
				connections += cxn.increment
			}
			case None => connections += new Connection(this, to, 1)
		}
	}
	
	def getConnection(to: Node): Option[Connection] = {
		connections.find { _.to == to } 
	}
	
	def fire(weight: Int): Map[Node, Int] = {
		val fired = mutable.Map.empty[Node, Int]
		connections.foreach { cxn => 
			fired += (cxn.to -> cxn.weight * weight)
		}
		fired.toMap
	}
} 

private case class DocumentNode(val document: Document) extends Node {
	override def equals(any: Any): Boolean = {
		any match {
			case DocumentNode(doc) => doc == document
			case _ => false
		}
	}
}

private case class LexemeNode(val lexId: Int) extends Node {
	override def equals(any: Any): Boolean = {
		any match {
			case LexemeNode(id) => lexId == id
			case _ => false
		}
	}
}

private[network] class RootNode(val lexis: Lexis) extends Node

private class Connection(val from: Node, val to: Node, val weight: Int) {
	def increment = { new Connection(from, to, weight + 1)}
}

/**
 * Represent a set of Documents as a Network of the Lexemes that make up the 
 * Documents.
 */
class Network(val lexis: Lexis) {
	val log = Logger[this.type]
	
	val root = new RootNode(lexis)
	val nodes = mutable.ListBuffer.empty[Node]
	
	/**
	 * Add a Document to this Network
	 */
	def add(document: Document) = {
		val docNode = DocumentNode(document)
		nodes += docNode
		var last: Node = root
		
		def connect(lexNode: LexemeNode) {
			lexNode.connect(docNode)
			last.connect(lexNode)
			last = lexNode
		}
		
		document.tokens.foreach { token =>		
			findNode(token) match { 
				case Some(lexNode @ LexemeNode(_))  => connect(lexNode)
				case _ =>
					val lexId = lexis.index(token).id
					val lexNode = new LexemeNode(lexId)
					connect(lexNode)
					nodes += lexNode
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
					case LexemeNode(lexId) => acc + lexis.find(lexId).get.token
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
	
	/**
	 * Optionally retrieve a node in the network matching the given token 
	 */
	def findNode(token: String): Option[Node] = {
		val optionalLexeme = lexis.find(token)
		optionalLexeme.flatMap { lexeme => findNode(lexeme) }
	}
	
	/**
	 * Optionally retrieve a node in the network matching the given lexeme
	 */
	def findNode(lexeme: Lexeme): Option[Node] = {
		nodes.find {
			_ match {
				case LexemeNode(lexId) => lexId == lexeme.id
				case _ => false
			}
		}
	}
}

private class Firing(val network: Network) {
	val fired = mutable.Map.empty[Node, Int] 
	
	def fire(tokens: Array[String]) {
		tokens.foreach { token =>  
			network.findNode(token).foreach { node =>
				val weightToFire = fired.getOrElse(node, 0) + 1
				node.fire(weightToFire).foreach { 
					case (activated, weight) => 
						fired(activated) = fired.getOrElse(activated, 0) + weight
				}
			}
		}
	} 
	
	/** 
	 * Return the weight fired into the given node
	 */
	def getWeight(node: Node): Int = {
		fired find { _._1 == node } match {
			case Some((n, w)) => w
			case None => 0
		}
	}
	
	def results: List[Document] = {
		val results = fired.foldLeft(mutable.Map.empty[Document, Int])(
				(acc, fire) => {
					fire match {
						case (DocumentNode(doc), weight) => {
							acc(doc) = weight
							acc
						}
						case _ => acc
					}
				})	
				
		results.toList sortBy { _._2 } map { _._1 } reverse
	}
}