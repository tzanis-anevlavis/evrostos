/*
 *  rLTL Tool Collection
 *  Copyright (C) 2017  Daniel Neider
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.mpi_sws.rltl.expressions;

import org.mpi_sws.rltl.visitors.Visitor;


/**
 * This class represents a generic LTL expression and serves as an abstract
 * base class from which concrete expressions are derived. Expression can be
 * nested, thereby forming an abstract syntax tree.
 *
 * @author Daniel Neider
 * 
 * @version 1.0
 */
public abstract class Expression
{

	/**
	 * Accepts a visitor (implementing the visitor pattern).
	 *
	 * @param visitor The visitor to accept
	 */
	public abstract void accept(Visitor visitor);


	/**
	 * Returns a deep copy of this expression.
	 *
	 * @return a deep copy of this expression
	 */
	public abstract Expression copy();

}
