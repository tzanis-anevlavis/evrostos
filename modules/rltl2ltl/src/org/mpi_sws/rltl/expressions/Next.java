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
 * A next expressions.
 *
 * @author Daniel Neider
 *
 * @version 1.0
 */
public class Next extends Expression
{

	/**
	 * The subexpression of this next expression.
	 */
	public Expression subExpr = null;


	/**
	 * Creates a next expression with the given subexpression.
	 *
	 * @param subExpr The subexpression
	 */
	public Next(Expression subExpr)
	{
		this.subExpr = subExpr;
	}


	/**
	 * Creates a copy of the given next expression.
	 *
	 * @param other The next expression to copy
	 */
	public Next(Next other)
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
		return new Next(this);
	}

}
