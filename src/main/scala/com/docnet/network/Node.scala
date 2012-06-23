package com.docnet.network

import com.docnet.lexicology._
import scala.collection._
import grizzled.slf4j.Logger

private[network] object Node {
	
	def connect(from: Node, to: Node) {
		from.getConnection(to) match {
			case Some(cxn) => {
				from.connections -= cxn
				from.connections += Connection.increment(cxn)
			}
			case None => from.connections += new Connection(from, to, 1)
		}
	}
	
	def fire(node: Node, value: Int): Map[Node, Int] = {
		val fired = mutable.Map.empty[Node, Int]
		node.connections.foreach { cxn => 
			fired(cxn.to) = fired.getOrElse(cxn.to, 0) + cxn.weight * value
		}
		fired.toMap
	}
	
}

private[network] sealed abstract case class Node(val parent: Node) {
	val connections = mutable.ListBuffer.empty[Connection]
	
	def getConnection(to: Node): Option[Connection] = {
		connections.find { _.to == to } 
	}
	
	def cxnString(): String
	
} 

private[network] case class DocumentNode(val document: Document) extends Node(null) {
	
	override def equals(any: Any): Boolean = {
		any match {
			case DocumentNode(d) => d == document
			case _ => false
		}
	}
	
	override def hashCode(): Int = 41 + document.hashCode
	
	override def toString(): String = "[Document]"
	
	def cxnString(): String = toString
}

private[network] case class LexemeNode(
		val lexId: Int, 
		override val parent: Node ) extends Node(parent) {
	
	override def equals(any: Any): Boolean = {
		any match {
			case LexemeNode(id, p) => lexId == id && parent == p
			case _ => false
		}
	}
	
	override def hashCode(): Int = { 
		41 * (41 + lexId.hashCode()) + parent.hashCode()
	}
	
	override def toString(): String = {
		"[LexemeNode lexId=%d, connections=%s]".format(lexId, connectionsString) 
	}

	private def connectionsString(): String = {
		connections.foldLeft("")(
				(acc, cxn) => {
					acc +"[w:%d->%s]".format(cxn.weight, cxn.to.cxnString)
				})
	}
				
	def cxnString(): String = { 
		"[lexId=%d]".format(lexId)
	}
}

private[network] class RootNode(val lexis: Lexis) extends Node(null) 
{
	override def toString(): String = "[RootNode]"

	def cxnString(): String = toString
}
