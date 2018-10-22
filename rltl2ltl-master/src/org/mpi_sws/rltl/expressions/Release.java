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
 * A release expression.
 *
 * @author Daniel Neider
 *
 * @version 1.0
 */
public class Release extends Expression
{

	/**
	 * The expression on the left-hand-side of this release expression.
	 */
	public Expression subExpr1 = null;


	/**
	 * The expression on the right-hand-side of this release expression.
	 */
	public Expression subExpr2 = null;


	/**
	 * Creates a release expression.
	 *
	 * @param subExpr1 The expression on the left-hand-side of the release expression
	 * @param subExpr2 The expression on the right-hand-side of the release expression
	 */
	public Release(Expression subExpr1, Expression subExpr2)
	{
		this.subExpr1 = subExpr1;
		this.subExpr2 = subExpr2;
	}


	/**
	 * Creates a copy of the given release expression.
	 *
	 * @param other The release expression to copy
	 */
	public Release(Release other)
	{
		this(other.subExpr1.copy(), other.subExpr2.copy());
	}


	@Override
	public void accept(Visitor visitor)
	{
		visitor.visit(this);
	}


	@Override
	public Expression copy()
	{
		return new Release(this);
	}

}
