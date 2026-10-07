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
 * A disjunction of two LTL expressions.
 *
 * @author Daniel Neider
 *
 * @version 1.0
 */
public class Disjunction extends Expression
{

	/**
	 * The first disjunct.
	 */
	public Expression subExpr1 = null;


	/**
	 * The second disjunct.
	 */
	public Expression subExpr2 = null;


	/**
	 * Creates a disjunction of the given expressions.
	 *
	 * @param subExpr1 The first disjunct
	 * @param subExpr2 The second disjunct
	 */
	public Disjunction(Expression subExpr1, Expression subExpr2)
	{
		this.subExpr1 = subExpr1;
		this.subExpr2 = subExpr2;
	}


	/**
	 * Creates a copy of the given disjunction.
	 *
	 * @param other The disjunction to copy
	 */
	public Disjunction(Disjunction other)
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
		return new Disjunction(this);
	}


	/**
	 * Creates a disjunction of four expressions.
	 *
	 * @param expr1 The first expression
	 * @param expr2 The second expression
	 * @param expr3 The third expression
	 * @param expr4 The fourth expression
	 *
	 * @return the disjunction of the given four expressions
	 */
	public static Expression makeDisjunction(Expression expr1, Expression expr2, Expression expr3, Expression expr4)
	{
		return new Disjunction(expr1, new Disjunction(expr2, new Disjunction(expr3, expr4)));
	}

}
