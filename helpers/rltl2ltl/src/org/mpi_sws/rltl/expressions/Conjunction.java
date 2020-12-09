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
 * A conjunction of two LTL expressions.
 *
 * @author Daniel Neider
 *
 * @version 1.0
 */
public class Conjunction extends Expression
{

	/**
	 * The first conjunct.
	 */
	public Expression subExpr1 = null;


	/**
	 * The second conjunct.
	 */
	public Expression subExpr2 = null;


	/**
	 * Creates a conjunction of the given expressions.
	 *
	 * @param subExpr1 The first conjunct
	 * @param subExpr2 The second conjunct
	 */
	public Conjunction(Expression subExpr1, Expression subExpr2)
	{
		this.subExpr1 = subExpr1;
		this.subExpr2 = subExpr2;
	}


	/**
	 * Creates a copy of the given conjunction.
	 *
	 * @param other The conjunction to copy
	 */
	public Conjunction(Conjunction other)
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
		return new Conjunction(this);
	}


	/**
	 * Creates a conjunction of four expressions.
	 *
	 * @param expr1 The first expression
	 * @param expr2 The second expression
	 * @param expr3 The third expression
	 * @param expr4 The fourth expression
	 *
	 * @return the conjunction of the given four expressions
	 */
	public static Expression makeConjunction(Expression expr1, Expression expr2, Expression expr3, Expression expr4)
	{
		return new Conjunction(expr1, new Conjunction(expr2, new Conjunction(expr3, expr4)));
	}

	// J-edit: created to use in RLTL2LTLVisitor.java to translate Robust Implication
	public static Expression makeConjunction4(Implication expr1, Implication expr2, Implication expr3, Implication expr4)
	{
		return new Conjunction(expr1, new Conjunction(expr2, new Conjunction(expr3, expr4)));
	}

	public static Expression makeConjunction3(Implication expr1, Implication expr2, Implication expr3)
	{
		return new Conjunction(expr1, new Conjunction(expr2, expr3));
	}
	// End of J-edit.

}
