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

package org.mpi_sws.rltl.visitors;

import org.mpi_sws.rltl.expressions.*;


/**
 * Base class of all visitors that generate a textual representation of an expression.
 * In particular, this class provides basic facilities to handle and manipulate
 * such textual representations.
 *
 * @author Daniel Neider
 *
 * @version 1.0
 */
public abstract class PrintVisitor implements Visitor
{

	/**
	 * The string builder used to construct the textual representation of the visited expression.
	 */
	protected StringBuilder builder;


	/**
	 * Creates a print visitor with an empty string builder.
	 */
	public PrintVisitor()
	{
		this(new StringBuilder());
	}


	/**
	 * Creates a new print visitor using the given string builder.
	 *
	 * @param builder The string builder to use
	 */
	public PrintVisitor(StringBuilder builder)
	{
		this.builder = builder;
	}


	/**
	 * Returns the string builder used by this visitor.
	 *
	 * @return the string builder used by this visitor
	 */
	public StringBuilder getBuilder()
	{
		return builder;
	}


	/**
	 * Returns the content of the string builder associated with this visitor.
	 *
	 * @return the content of the string builder associated with this visitor
	 */
	public String toString()
	{
		return builder.toString();
	}


	/**
	 * Resets this visitor by replacing the associated string builder with a new object.
	 */
	public void reset()
	{
		builder = new StringBuilder();
	}


	/**
	 * Generates a textual representation of the given expression.
	 *
	 * @param expr The expression to print
	 *
	 * @return a textual representation of the given expression
	 */
	public String expression2String(Expression expr)
	{
		
		// Check arguments
		if (expr == null)
		{
			throw new IllegalArgumentException("Expression must not be null");
		}

		reset();
		expr.accept(this);

		return toString();

	}

}