package com.docnet.lexicology

import grizzled.slf4j.Logger
import collection.mutable.ListBuffer
import javax.management.remote.rmi._RMIConnection_Stub

/**
 * A Lexis is the dictionary of a language or all the words in that language.
 * User: nick
 * Date: 12/09/2011
 * Time: 18:16
 */

/**
 * Building block for the Lexis class. A token is represented by a sequence of
 * LexisNodes terminated by a Lexeme.
 */
sealed case class LexisNode(prev: LexisNode, next: ListBuffer[LexisNode], ch: Char)
{
	val log = Logger[this.type]

	/**
	 * Index the given token and return the resulting lexeme.
	 *
	 * TODO: Order the next list for binary search to improve performance
	 */
	def index(token: String): Lexeme = {
		log.debug("index(token=" + token + ")");
		assert(token.length > 0)

		val found = next.find( _.ch == token(0))
		val last = token.length == 1
		found match {
			case None => {
				if (last) {
					val lex = Lexeme(this, ListBuffer[LexisNode](),	token(0))
					next += lex
					log.debug("created Lexeme: " + lex + ", for token: " + lex.token())
					lex
				}
				else {
					val node = LexisNode(this, ListBuffer[LexisNode](),	token(0))
					next += node
					node index token.substring(1)
				}
			}
			case Some(Lexeme(_, _, c)) => {
				if (last) found.get.asInstanceOf[Lexeme]
				else found.get index token.substring(1)
			}
			case Some(LexisNode(_, _, c)) =>
				if (last) {
					val lex = Lexeme(this, found.get.next, found.get.ch)
					val index = next.indexOf(found.get);
					next.remove(index)
					next.insert(index, lex)
					lex
				}
				else found.get index token.substring(1)
		}
	}

	/**
	 * Search forward through the "next" LexisNodes to find the given token.
	 */
	def find(token: String): Option[Lexeme] = {
		log.debug("find(token: '" + token + "') on " + this);
		log.debug("next=" + next)
		assert(token.length > 0)
		val node = next.find(_.ch == token(0))

		node match {
			case None => None
			case Some(Lexeme(_, _, c)) => {
				log.debug("found " + node + ", token='" + token + "'")
				if (token.length == 1) node.asInstanceOf[Option[Lexeme]]
				else node.get.find(token.substring(1))
			}

			case Some(LexisNode(_, _, c)) => {
				if (token.length == 1) None
				else node.get.find(token.substring(1))
			}
		} 	
	}

	/**
	 * Return the token represented by this lexis node
	 */
	def token(): String = {
		log.debug("token()");
		
		if (prev.isInstanceOf[Lexis]) this.ch.toString
		else prev.token() + this.ch
	}

	/**
	 * Count the number of lexemes defined in this lexis node
	 */
	def lexemeCount(): Int = {
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
	def nodeCount(): Int = {
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
case class Lexis()
		extends LexisNode(null, ListBuffer[LexisNode](), '^')
/**
 *
 */
case class Lexeme(
	override val prev: LexisNode,
	override val next: ListBuffer[LexisNode],
	override val ch: Char)
	extends LexisNode(prev, next, ch)
{
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
	override val ch: Char)
	extends Lexeme(prev, next, ch)

/**
 * The Stem of a word is the part of the word that never changes. For example,
 * from "produced", the lemma is "produce", but the stem is produc-
 *
 * The foa list will represent all the lexemes that start with this stem.
 */
case class Stem(
	override val prev: LexisNode,
	override val next: ListBuffer[LexisNode],
	override val ch: Char)
	extends LexisNode(prev, next, ch)