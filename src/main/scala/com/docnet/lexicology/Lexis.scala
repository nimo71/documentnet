package com.docnet.lexicology

import grizzled.slf4j.Logger
import collection.mutable._
import javax.management.remote.rmi._RMIConnection_Stub


/**
 * A Lexis is the dictionary of a language or all the words in that language.
 * User: nick
 * Date: 12/09/2011
 * Time: 18:16
 */

class Lexis() {
	private val root = new LexisRoot(this);
	private var index: ListBuffer[Lexeme] = ListBuffer.empty[Lexeme]
	
	def index(token: String): Lexeme = root.index(token)
	
	def find(id: Int): Option[Lexeme] = {
		if (id <= 0) None
		else if (id > index.length) None
		else Some(index(id - 1))
	}
	
	def find(token: String): Option[Lexeme] = root.find(token)
	
	def lexemeCount(): Int = root.lexemeCount()
	
	private[lexicology] def nodeCount(): Int = root.nodeCount()
	
	private[lexicology] def indexLexeme(lexeme: Lexeme) {
		index+=lexeme
	}
}

/**
 * Building block for the Lexis class. A token is represented by a sequence of
 * LexisNodes terminated by a Lexeme.
 */
private[lexicology] sealed case class LexisNode(prev: LexisNode, next: ListBuffer[LexisNode], ch: Char, lexis: Lexis)
{
	val log = Logger[this.type]

	/**
	 * Index the given token and return the resulting lexeme.
	 *
	 * TODO: Order the next list for binary search to improve performance
	 */
	def index(token: String): Lexeme = {
		assert(token.length > 0)

		def newNextLexeme(ch: Char): Lexeme = {
			val lexeme = Lexeme(this, ListBuffer[LexisNode](), ch, lexis)
			next += lexeme
			log.debug("created Lexeme: " + lexeme + ", for token: " + lexeme.token())
			lexeme
		}
		
		def newNextNodeThenIndexTail(head: Char, tail: String): Lexeme = {
			val node = LexisNode(this, ListBuffer[LexisNode](),	head, lexis)
			next += node
			node index tail
		}
		
		def replaceNextNodeWithLexeme(nextNode: LexisNode): Lexeme = {
			val lexeme = Lexeme(this, nextNode.next, nextNode.ch, lexis)
			val index = next.indexOf(nextNode);
			next.remove(index)
			next.insert(index, lexeme)
			lexeme
		}
		
		val head = token(0)
		val tail = token.substring(1)
		
		return if (lastChar(token))
			getMatchingNextNode(head) match {
				case None => newNextLexeme(head)
				case Some(lexeme @ Lexeme(_, _, _, _)) => lexeme
				case Some(node @ LexisNode(_, _, _, _)) => replaceNextNodeWithLexeme(node)	
			}
		else 
			getMatchingNextNode(head) match {
				case None => newNextNodeThenIndexTail(head, tail) 
				case Some(lexeme @ Lexeme(_, _, _, _)) => lexeme index tail
				case Some(nextNode @ LexisNode(_, _, _, _)) => nextNode index tail
			}
	}

	private def lastChar(token: String): Boolean = token.length == 1
	
	private def getMatchingNextNode(ch: Char): Option[LexisNode] = next.find( _.ch == ch)
	
	/**
	 * Search forward through the "next" LexisNodes to find the given token.
	 */
	def find(token: String): Option[Lexeme] = {
		log.debug("find(token: '"+ token +"') on "+ this);
		assert(token.length > 0)
		
		val head = token(0)
		val tail = token.substring(1)

		getMatchingNextNode(head) match {
			case None => None
			case Some(lexeme @ Lexeme(_, _, _, _)) => {
				if (lastChar(token)) 
					Some(lexeme)
				else 
					lexeme.find(tail)
			}

			case Some(node @ LexisNode(_, _, _, _)) => {
				if (lastChar(token)) 
					None
				else 
					node.find(tail)
			}
		} 	
	}
	
	/**
	 * Return the token represented by this lexis node
	 */
	def token(): String = {
		log.debug("token()");
		
		if (prev.isInstanceOf[LexisRoot]) this.ch.toString
		else prev.token() + this.ch
	}

	/**
	 * Count the number of lexemes defined in this lexis node
	 */
	private[lexicology] def lexemeCount(): Int = {
		var count = 0
		next.foreach(n => {
			if (n.isInstanceOf[Lexeme]) count += 1
			count += n.lexemeCount()
		})
		count
	}

	/**
	 * Count the number of LexemeNodes
	 */
	private[lexicology] def nodeCount(): Int = {
		var count = 0
		next.foreach(n => {
			count += 1
			count += n.nodeCount()
		})
		count
	}
	
	/**
	 * Return the String representation of this LexisNode
	 */
	override def toString() = {
		"[" + this.getClass.getSimpleName + ": ch=" + this.ch + "]"
	}
}

/**
 *
 */
private[lexicology] case class LexisRoot(override val lexis: Lexis)
		extends LexisNode(null, ListBuffer[LexisNode](), '^', lexis) 

/**
 *
 */
case class Lexeme(
	override val prev: LexisNode,
	override val next: ListBuffer[LexisNode],
	override val ch: Char, 
	override val lexis: Lexis)
	extends LexisNode(prev, next, ch, lexis)
{
	val uid = lexis.lexemeCount() + 1
	lexis.indexLexeme(this)
	
	/**
	 * The unique id of this lexeme in the lexis
	 */
	def id() = { uid }
	
	/**
	 * Lemmatization of a Lexime will produce a Lemma
	 */
	def lemmatize(): Lemma = null
}

/**
 * A Lemma is the canonical or dictionary form of a word. All Leximes have a Lemma
 */
case class Lemma(
	override val prev: LexisNode,
	override val next: ListBuffer[LexisNode],
	override val ch: Char, 
	override val lexis: Lexis)
	extends Lexeme(prev, next, ch, lexis)

/**
 * The Stem of a word is the part of the word that never changes. For example,
 * from "produced", the lemma is "produce", but the stem is produc-
 *
 * The foa list will represent all the lexemes that start with this stem.
 */
case class Stem(
	override val prev: LexisNode,
	override val next: ListBuffer[LexisNode],
	override val ch: Char,
	override val lexis: Lexis)
	extends LexisNode(prev, next, ch, lexis)