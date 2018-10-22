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
 * The negation of an expressions.
 *
 * @author Daniel Neider
 *
 * @version 1.0
 */
public class Negation extends Expression
{

	/**
	 * The subexpression of this negation.
	 */
	public Expression subExpr = null;


	/**
	 * Creates a negation of the given expression.
	 *
	 * @param subExpr The expression to negate
	 */
	public Negation(Expression subExpr)
	{
		this.subExpr = subExpr;
	}


	/**
	 * Creates a copy of the given negation.
	 *
	 * @param other The negation to copy
	 */
	public Negation(Negation other)
	{
		this(other.subExpr.copy());
	}


	@Override
	public void accept(Visitor visitor)
	{
		visitor.visit(this);
	}


	@Override
	public Expression copy()
	{
		return new Negation(this);
	}

}
