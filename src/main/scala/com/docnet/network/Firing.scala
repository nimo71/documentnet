package com.docnet.network

import scala.collection._
import grizzled.slf4j.Logger

private class Firing(val network: Network) {
	val fired = new FiredNodes()
	
	def fire(tokens: Array[String]) {
		tokens.foreach { fire(_) }
	} 
	private def fire(token: String) {
		network.findNodes(token).foreach { fire(_) }
	}
	private def fire(node: Node) {
		val valueToFire = fired.firedValue(node)
		val firedNodes = Node.fire(node, valueToFire)
		firedNodes.foreach { 
			case (firedNode, value) => fired.update(firedNode, value)
		}
	}
	
	def results: List[Document] = {
		val results = fired.documents 
		results.toList sortBy { _._2 } map { _._1 } reverse
	}
}

private class FiredNodes {
	val fired = mutable.Map.empty[Node, Int]
	
	def firedValue(node: Node): Int = {
		fired.getOrElse(node, 0) + 1
	} 	
	
	def update(node: Node, value: Int) {
		fired(node) = fired.getOrElse(node, 0) + value
	}
	
	def documents(): Map[Document, Int] = {
		fired.foldLeft(mutable.Map.empty[Document, Int])(
			(acc, fire) => {
				fire match {
					case (DocumentNode(doc), weight) => {
						acc(doc) = weight
						acc
					}
					case _ => acc
				}
			})	
	}
}