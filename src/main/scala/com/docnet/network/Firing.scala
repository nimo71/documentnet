package com.docnet.network

import scala.collection._
import grizzled.slf4j.Logger

private class Firing(val network: Network) {
	val log = Logger[this.type]
	
	val fired = mutable.Map.empty[Node, Int] 
	
	def fire(tokens: Array[String]) {
		tokens.foreach { fire(_) }
	} 
	
	private def fire(token: String) {
		network.findNodes(token).foreach { fire(_) }
		log.debug { "fired after fire(%s)=%s".format(token, fired.toString) }
	}
	
	private def fire(node: Node) {
		val valueToFire = fired.getOrElse(node, 0) + 1
		val firedNodes = node.fire(valueToFire)
		firedNodes.foreach { 
			case (firedNode, value) => 
				fired(firedNode) = fired.getOrElse(firedNode, 0) + value
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