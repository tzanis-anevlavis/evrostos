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
 * A print visitor for creating a textual representation of the abstract syntax tree
 * of an expression.
 *
 * @author Daniel Neider
 *
 * @version 1.0
 */
public class ASTPrintVisitor extends PrintVisitor
{

	/**
	 * Number of white spaces used to indent the current line.
	 */
	private int _indent = 0;


	/**
 	 * Prints an atomic expression.
	 *
	 * @param atom The atomic expression to print
	 */
	@Override
	public void visit(Atom atom)
	{
		indent();
		builder.append(atom.identifier);
	}


	/**
 	 * Prints the abstract syntax tree rooted at a conjunction.
	 *
	 * @param conjunction The conjunction to print
	 */
	@Override
	public void visit(Conjunction conjunction)
	{
		visitBinaryOperator(conjunction.subExpr1, conjunction.subExpr2, "&");
	}


	/**
 	 * Prints the abstract syntax tree rooted at a disjunction.
	 *
	 * @param disjunction The disjunction to print
	 */
	@Override
	public void visit(Disjunction disjunction)
	{
		visitBinaryOperator(disjunction.subExpr1, disjunction.subExpr2, "|");
	}


	/**
 	 * Prints the abstract syntax tree rooted at an implication.
	 *
	 * @param implication The implication to print
	 */
	@Override
	public void visit(Implication implication)
	{
		visitBinaryOperator(implication.subExpr1, implication.subExpr2, "=>");
	}


	/**
 	 * Prints the abstract syntax tree rooted at a negation.
	 *
	 * @param negation The negation to print
	 */
	@Override
	public void visit(Negation negation)
	{
		visitUnaryOperator(negation.subExpr, "!");
	}


	/**
 	 * Prints the abstract syntax tree rooted at a finally expression.
	 *
	 * @param finallyExpr The finally expression to print
	 */
	@Override
	public void visit(Finally finallyExpr)
	{
		visitUnaryOperator(finallyExpr.subExpr, "F");
	}


	/**
 	 * Prints the abstract syntax tree rooted at a globally expression.
	 *
	 * @param globallyExpr The globally expression to print
	 */
	@Override
	public void visit(Globally globallyExpr)
	{
		visitUnaryOperator(globallyExpr.subExpr, "G");
	}


	/**
 	 * Prints the abstract syntax tree rooted at a next expression.
	 *
	 * @param nextExpr The next expression to print
	 */
	@Override
	public void visit(Next nextExpr)
	{
		visitUnaryOperator(nextExpr.subExpr, "X");
	}


	/**
 	 * Prints the abstract syntax tree rooted at an until expression.
	 *
	 * @param untilExpr The until expression to print
	 */
	@Override
	public void visit(Until untilExpr)
	{
		visitBinaryOperator(untilExpr.subExpr1, untilExpr.subExpr2, "U");
	}



	/**
 	 * Prints the abstract syntax tree rooted at a release expression.
	 *
	 * @param releaseExpr The release expression to print
	 */
	public void visit(Release releaseExpr)
	{
		visitBinaryOperator(releaseExpr.subExpr1, releaseExpr.subExpr2, "R");
	}



	/**
 	 * Prints white spaces to indent the current line.
	 */
	private void indent()
	{
		for (int i = 0; i < _indent; ++i)
		{
			builder.append(" ");
		}
	}



	/**
 	 * Prints the abstract syntax tree rooted at a unary operator.
	 *
	 * @param subExpr The subexpression of a unary operator
	 * @param operator A textual representation of the operator
	 */
	private void visitUnaryOperator(Expression subExpr, String operator)
	{

		indent();
		builder.append(operator);
		_indent += 2;

		if (subExpr == null)
		{
			indent();
			builder.append("null");
		}
		else
		{
			subExpr.accept(this);
		}

		_indent -= 2;
	}


	/**
 	 * Prints the abstract syntax tree rooted at a binary operator.
	 *
	 * @param subExpr1 The first subexpression of a binary operator
	 * @param subExpr2 The second subexpression of a binary operator
	 * @param operator A textual representation of the operator
	 */
	private void visitBinaryOperator(Expression subExpr1, Expression subExpr2, String operator)
	{

		indent();
		builder.append(operator);
		_indent += 2;

		if (subExpr1 == null)
		{
			indent();
			builder.append("null");
		}
		else
		{
			subExpr1.accept(this);
		}

		if (subExpr2 == null)
		{
			indent();
			builder.append("null");
		}
		else
		{
			subExpr2.accept(this);
		}

		_indent -= 2;

	}

}