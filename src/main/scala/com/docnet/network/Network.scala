package com.docnet.network

import com.docnet.lexicology._
import scala.collection._
import grizzled.slf4j.Logger

private[network] sealed abstract case class Node() 
{
	private val next = mutable.Map.empty[Node, Int] 
	
	def connect(node: Node) {
		val weight = next.getOrElse(node, 0) + 1
		next(node) = weight 
	}
	
	def fire(weight: Int): Map[Node, Int] = {
		next.map { case (nextNode, nextWeight) =>
			(nextNode -> nextWeight * weight)
		}
	}
}
private case class DocumentNode(val document: Document) extends Node {
	
	override def equals(a: Any): Boolean = {
		a match {
			case DocumentNode(doc) => doc == document
			case _ => false
		}
	}
}
private case class LexemeNode(val lexId: Int) extends Node {
	
//	override def equals(a: Any): Boolean = {
//		a match {
//			case LexemeNode(id) => id == lexId
//			case _ => false
//		}
//	}
	
}
private[network] class RootNode(val lexis: Lexis) extends Node

private class Firing {
	val nodes = mutable.ListBuffer.empty[(Node, Int)]
	
	/** 
	 * Return the weight fired into the given node
	 */
	def getWeight(node: Node): Int = {
		nodes find { _._1 == node } match {
			case Some((n, w)) => w
			case None => 0
		}
	}
	
	def updateWeight(node: Node, weight: Int) {
		val index = nodes findIndexOf { _._1 == node }
		
		if (index < 0) 
			nodes += ((node, weight))
		else {
			val (n, w) = nodes(index)
			nodes(index) = (n, w + weight)
		} 
	}
	
	def updateFired(fired: Map[Node, Int]) = {
		fired foreach { case (firedNode, firedWeight) => 
			this.updateWeight(firedNode, firedWeight)
		}
	}
	
	def results: List[Document] = {
		val results = nodes.foldLeft(mutable.Map.empty[Document, Int])(
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

/**
 * Represent a set of Documents as a Network of the Lexemes that make up the 
 * Documents.
 */
class Network(val lexis: Lexis) {
	val log = Logger[this.type]
	
	val root = new RootNode(lexis)
	val nodes = mutable.ListBuffer.empty[Node]
	
	/**
	 * Search the Network for Documents matching the given query
	 */
	def search(query: String): List[Document] = {
		
		val firing = new Firing()
		
		query.split(' ').foreach { token =>  
			lexis.find(token).foreach { lexeme =>
				findLexemeNodes(lexeme.id) foreach { node =>
					
					val firingWeight = firing.getWeight(node)	
					val firedNextNodes = node.fire(firingWeight + 1);
					firing.updateFired(firedNextNodes)
				} 
			} 
		}
		firing results
	}
	
	private def findLexemeNodes(lexId: Int): List[Node] = {
		nodes.filter { 
			_ match {
				case LexemeNode(id) => lexId == id
				case _ => false
			}
		}.toList
	} 
	
	/**
	 * Add a Document to this Network
	 */
	def add(document: Document) = {
		val docNode = DocumentNode(document)
		nodes += docNode
		
		var last: Node = root
		document.tokens.foreach { token =>
			val lexId = lexis.index(token).id
			val lexNode = new LexemeNode(lexId)
			
			lexNode.connect(docNode)
			last.connect(lexNode)
			nodes += lexNode
			last = lexNode
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

}
