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
 * A print visitor for creating a textual representation of an expression in standard,
 * fully bracketed LTL notation.
 *
 * @author Daniel Neider
 *
 * @version 1.0
 */
public class PrettyPrintVisitor extends PrintVisitor
{

	/**
 	 * Prints an atomic expression in standard LTL syntax.
	 *
	 * @param atom The atomic expression to print
	 */
	@Override
	public void visit(Atom atom)
	{
		builder.append(atom.identifier);
	}


	/**
 	 * Prints a conjunction in standard LTL syntax.
	 *
	 * @param conjunction The conjunction to print
	 */
	@Override
	public void visit(Conjunction conjunction)
	{

		builder.append("(");
		conjunction.subExpr1.accept(this);
		builder.append(" & ");
		conjunction.subExpr2.accept(this);
		builder.append(")");

	}


	/**
 	 * Prints a disjunction in standard LTL syntax.
	 *
	 * @param disjunction The disjunction to print
	 */
	@Override
	public void visit(Disjunction disjunction)
	{

		builder.append("(");
		disjunction.subExpr1.accept(this);
		builder.append(" | ");
		disjunction.subExpr2.accept(this);
		builder.append(")");

	}


	/**
 	 * Prints an implication in standard LTL syntax.
	 *
	 * @param implication The implication to print
	 */
	@Override
	public void visit(Implication implication)
	{

		builder.append("(");
		implication.subExpr1.accept(this);
		builder.append(" => ");
		implication.subExpr2.accept(this);
		builder.append(")");

	}


	/**
 	 * Prints a negation in standard LTL syntax.
	 *
	 * @param negation The negation to print
	 */
	@Override
	public void visit(Negation negation)
	{

		builder.append("(!");
		negation.subExpr.accept(this);
		builder.append(")");

	}


	/**
 	 * Prints a finally expression in standard LTL syntax.
	 *
	 * @param finallyExpr The finally expression to print
	 */
	@Override
	public void visit(Finally finallyExpr)
	{

		builder.append("(F ");
		finallyExpr.subExpr.accept(this);
		builder.append(")");

	}


	/**
 	 * Prints a globally expression in standard LTL syntax.
	 *
	 * @param globallyExpr The globally expression to print
	 */
	@Override
	public void visit(Globally globallyExpr)
	{

		builder.append("(G ");
		globallyExpr.subExpr.accept(this);
		builder.append(")");

	}


	/**
 	 * Prints a next expression in standard LTL syntax.
	 *
	 * @param nextExpr The next expression to print
	 */
	@Override
	public void visit(Next nextExpr)
	{

		builder.append("(X ");
		nextExpr.subExpr.accept(this);
		builder.append(")");

	}


	/**
 	 * Prints an until expression in standard LTL syntax.
	 *
	 * @param untilExpr The until expression to print
	 */
	@Override
	public void visit(Until untilExpr)
	{

		builder.append("(");
		untilExpr.subExpr1.accept(this);
		builder.append(" U ");
		untilExpr.subExpr2.accept(this);
		builder.append(")");

	}


	/**
 	 * Prints a release expression in standard LTL syntax.
	 *
	 * @param releaseExpr The release expression to print
	 */
	@Override
	public void visit(Release releaseExpr)
	{

		builder.append("(");
		releaseExpr.subExpr1.accept(this);
		builder.append(" R ");
		releaseExpr.subExpr2.accept(this);
		builder.append(")");

	}

}