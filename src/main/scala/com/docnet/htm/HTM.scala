package com.docnet.htm

import collection.immutable.HashSet


/**
 * User: nick
 * Date: 26/09/2011
 * Time: 20:47
 */

class HTM(regions: Set[Region])

class Region(inputs: Set[Bit], columns: Set[Column]) {
	// connect each column to a unique set of the input bits

	/**
	 * learning the connections to each column from a subset of the inputs
	 * determining the level of input to each column
	 * using inhibition to select a sparse set of active columns
	 */
	def spatialPooler() = {}

	
}

class Column(cells: Set[Cell]) {
	// activation level
	// inhibition
	// 
}

class Bit(state: BitState)

abstract class BitState(value: Int)
case class Active() extends BitState(1)
case class Inactive() extends BitState(0)

class Cell(val input: Bit)

abstract class CellState
case class FeedForwardActiveCellState() extends CellState // active from feed-forward input
case class LateralActiveCellState() extends CellState // active from lateral input, is this the predictive input??
case class InactiveCellState() extends CellState // inactive