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

import java.util.HashMap;
import org.mpi_sws.rltl.expressions.*;


/**
 * Visitor for translating rLTL expressions (which are represented as classical LTL expressions)
 * to LTL expressions.
 * <p>
 * This class implements the translation of rLTL to LTL as described in the paper. For each rLTL
 * subexpression. The result of the translation is an array of four LTL expressions, where each
 * entry of this array corresponds to one bit in the vector of the semantics of the rLTL expression.
 * <p>
 * All translations make sure that no two expressions share common subexpressions.
 *
 * @author Daniel Neider
 *
 * @version 1.0
 */
public class RLTL2LTLVisitor implements Visitor
{

	/**
	 * Array of expressions (of length four) storing the translation so far.
	 */
	private Expression[] ltlExpr = null;


	/**
	 * Converts the given expression rLTL expression to four LTL expressions.
	 *
	 * @param expr The rLTL expression to convert
	 *
	 * @return four LTL expressions obtained from translating the given rLTL expression
	 */
	public static Expression[] convert(Expression expr)
	{

		// Run converter
		RLTL2LTLVisitor converterVisitor = new RLTL2LTLVisitor();
		expr.accept(converterVisitor);
		
		return converterVisitor.ltlExpr;

	}


	/**
	 * Converts an atomic rLTL expression.
	 *
	 * @param atom The atomic expression to convert
	 */
	@Override
	public void visit(Atom atom)
	{

		// Construct LTL expressions for atomic expression
		ltlExpr = new Expression[]
		{		
			new Atom(atom),
			new Atom(atom),
			new Atom(atom),
			new Atom(atom)
		};

	}


	/**
	 * Converts a conjunction of two rLTL expressions.
	 *
	 * @param conjunction The conjunction to convert
	 */
	@Override
	public void visit(Conjunction conjunction)
	{

		// Convert first subexpression; the result will be stored in the ltlExpr member
		conjunction.subExpr1.accept(this);
		Expression[] ltlSubExpr1 = ltlExpr;

		// Convert second subexpression; the result will be stored in the ltlExpr member
		conjunction.subExpr2.accept(this);

		// Create LTL expressions for conjunction
		ltlExpr = new Expression[]
		{
			new Conjunction(ltlSubExpr1[0], ltlExpr[0]),
			new Conjunction(ltlSubExpr1[1], ltlExpr[1]),
			new Conjunction(ltlSubExpr1[2], ltlExpr[2]),
			new Conjunction(ltlSubExpr1[3], ltlExpr[3])
		};

	}


	/**
	 * Converts a disjunction of two rLTL expressions.
	 *
	 * @param disjunction The disjunction to convert
	 */
	@Override
	public void visit(Disjunction disjunction)
	{

		// Convert first subexpression; the result will be stored in the ltlExpr member
		disjunction.subExpr1.accept(this);
		Expression[] ltlSubExpr1 = ltlExpr;

		// Convert second subexpression; the result will be stored in the ltlExpr member
		disjunction.subExpr2.accept(this);

		// Construct LTL expressions for disjunction
		ltlExpr = new Expression[]
		{
			new Disjunction(ltlSubExpr1[0], ltlExpr[0]),
			new Disjunction(ltlSubExpr1[1], ltlExpr[1]),
			new Disjunction(ltlSubExpr1[2], ltlExpr[2]),
			new Disjunction(ltlSubExpr1[3], ltlExpr[3])
		};

	}


	/**
	 * Converts an implication.
	 *
	 * @param implication The implication to convert
	 */
	@Override
	public void visit(Implication implication)
	{
		
		// Convert first subexpression; the result will be stored in the ltlExpr member
		implication.subExpr1.accept(this);
		Expression[] ltlSubExpr1 = ltlExpr;

		// Convert second subexpression; the result will be stored in the ltlExpr member
		implication.subExpr2.accept(this);

		// Construct LTL expressions for negation
		ltlExpr = new Expression[]
		{
			Conjunction.makeConjunction4 ( 
					new Implication(ltlSubExpr1[0], ltlExpr[0]),
					new Implication(ltlSubExpr1[1].copy(),ltlExpr[1].copy()),
					new Implication(ltlSubExpr1[2].copy(),ltlExpr[2].copy()),
					new Implication(ltlSubExpr1[3].copy(),ltlExpr[3].copy())
					),

			Conjunction.makeConjunction3 ( 
					new Implication(ltlSubExpr1[1], ltlExpr[1]), 
					new Implication(ltlSubExpr1[2].copy(),ltlExpr[2].copy()),
					new Implication(ltlSubExpr1[3].copy(),ltlExpr[3].copy())
					),

			new Conjunction ( 
					new Implication(ltlSubExpr1[2], ltlExpr[2]), new Implication(ltlSubExpr1[3].copy(),ltlExpr[3].copy())
					),

			new Implication(ltlSubExpr1[3],ltlExpr[3])
		};

	}


	/**
	 * Converts a negation.
	 *
	 * @param negation The negation to convert
	 */
	@Override
	public void visit(Negation negation)
	{

		// Convert subexpression; the result will be stored in the ltlExpr member
		negation.subExpr.accept(this);

		// Construct LTL expressions for negation
		ltlExpr = new Expression[]
		{
			new Negation(ltlExpr[0]),
			new Negation(ltlExpr[0]),
			new Negation(ltlExpr[0]),
			new Negation(ltlExpr[0])
		};

	}


	/**
	 * Converts a robust finally expressions.
	 *
	 * @param finallyExpr The finally expression to convert
	 */
	@Override
	public void visit(Finally finallyExpr)
	{

		// Convert subexpression; the result will be stored in the ltlExpr member
		finallyExpr.subExpr.accept(this);

		// Construct LTL expressions for finally operator
		ltlExpr = new Expression[]
		{
			new Finally(ltlExpr[0]),
			new Finally(ltlExpr[1]),
			new Finally(ltlExpr[2]),
			new Finally(ltlExpr[3])
		};

	}


	/**
	 * Converts a robust globally expressions.
	 *
	 * @param globallyExpr The globally expression to convert
	 */
	@Override
	public void visit(Globally globallyExpr)
	{

		// Convert subexpression; the result will be stored in the ltlExpr member
		globallyExpr.subExpr.accept(this);

		// Construct LTL expressions for globally operator
		ltlExpr = new Expression[]
		{
			new Globally(ltlExpr[0]),
			new Finally(new Globally(ltlExpr[1])),
			new Globally(new Finally(ltlExpr[2])),
			new Finally(ltlExpr[3])
		};

	}


	/**
	 * Converts a robust next expressions.
	 *
	 * @param nextExpr The next expression to convert
	 */
	@Override
	public void visit(Next nextExpr)
	{

		// Convert subexpression; the result will be stored in the ltlExpr member
		nextExpr.subExpr.accept(this);

		// Construct LTL for next operator
		ltlExpr = new Expression[]
		{
			new Next(ltlExpr[0]),
			new Next(ltlExpr[1]),
			new Next(ltlExpr[2]),
			new Next(ltlExpr[3])
		};

	}


	/**
	 * Converts a robust until expressions.
	 *
	 * @param untilExpr The until expression to convert
	 */
	@Override
	public void visit(Until untilExpr)
	{
		// Convert first subexpression; the result will be stored in the ltlExpr member
		untilExpr.subExpr1.accept(this);
		Expression[] ltlSubExpr1 = ltlExpr;

		// Convert second subexpression; the result will be stored in the ltlExpr member
		untilExpr.subExpr2.accept(this);

		// Construct LTL expressions for Until operator
		ltlExpr = new Expression[]
		{
			new Until(ltlSubExpr1[0], ltlExpr[0]),
			new Until(ltlSubExpr1[1], ltlExpr[1]),
			new Until(ltlSubExpr1[2], ltlExpr[2]),
			new Until(ltlSubExpr1[3], ltlExpr[3])
		};
	}


	/**
	 * Converts a robust release expressions.
	 *
	 * @param releaseExpr The release expression to convert
	 */
	@Override
	public void visit(Release releaseExpr)
	{
		// Convert first subexpression; the result will be stored in the ltlExpr member
		releaseExpr.subExpr1.accept(this);
		Expression[] ltlSubExpr1 = ltlExpr;

		// Convert second subexpression; the result will be stored in the ltlExpr member
		releaseExpr.subExpr2.accept(this);

		// Construct LTL expressions for Release operator
		ltlExpr = new Expression[]
		{
			new Release(ltlSubExpr1[0], ltlExpr[0]),
			new Disjunction(new Finally (new Globally(ltlExpr[1])), new Finally(ltlSubExpr1[1])),
			new Disjunction(new Globally (new Finally(ltlExpr[2])), new Finally(ltlSubExpr1[2])),
			new Disjunction(new Finally(ltlExpr[3]), new Finally(ltlSubExpr1[3]))
		};

	}

}
