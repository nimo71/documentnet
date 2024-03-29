package com.docnet.network

class Connection(val from: Node, val to: Node, val weight: Int) {
	
	override def equals(that: Any): Boolean = {
		that match {
			case c: Connection => equals(c)
			case _ => false
		}
	}
	
	def equals(cxn: Connection): Boolean = {
		if (cxn.from != from) return false
		if (cxn.to != to) return false
		if (cxn.weight != weight) return false
		return true
	}
	
	override def toString(): String = "[Connection: -%d-> %s]".format(weight, to)
}

object Connection {
	def increment(cxn: Connection): Connection = {
		new Connection(cxn.from, cxn.to, cxn.weight + 1)
	}
}