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
 * to LTL expressions using some optimizations.
 * <p>
 * See {@link RLTL2LTLVisitor} for more details about the translation in general. This visitor implements
 * an optimized translations. The underlying observation is that subformulas without the rebust globally
 * and release operator correspond to classical LTL formulas and, hence, all four LTL formulas obtained
 * during the translation are identical. This fact can be used to improve the translation of implications
 * and negations since one does not need to recur to all four subformulas in these cases.
 * <p>
 * All translations make sure that no two expressions share common subexpressions.
 *
 * @author Daniel Neider
 *
 * @version 1.0
 */
public class OptimizedRLTL2LTLVisitor implements Visitor
{

	/**
	 * Array of expressions (of length four) storing the translation so far.
	 */
	private Expression[] ltlExpr = null;

	/**
	 * Boolean indicating whether the processed expression if free of G and R operators.
	 * (In this case, all four translated LTL expressions are identical.)
	 */
	private boolean GRFree = false;


	/**
	 * Converts the given expression rLTL expression to four LTL expressions using the
	 * optimized translation.
	 *
	 * @param expr The rLTL expression to convert
	 *
	 * @return four LTL expressions obtained from the optimized translation of the given rLTL expression
	 */
	public static Expression[] convert(Expression expr)
	{

		// Run converter
		OptimizedRLTL2LTLVisitor converterVisitor = new OptimizedRLTL2LTLVisitor();
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


		// Update GRFree
		GRFree = true;

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
		boolean GRFreeSubExpr1 = GRFree;

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


		// Update GRFree
		GRFree = GRFree && GRFreeSubExpr1;

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
		boolean GRFreeSubExpr1 = GRFree;

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


		// Update GRFree
		GRFree = GRFree && GRFreeSubExpr1;

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
		boolean GRFreeSubExpr1 = GRFree;

		// Convert second subexpression; the result will be stored in the ltlExpr member
		implication.subExpr2.accept(this);

		
		//
		// Depending on whether the both subexpressions are GR-free, use optimized translation
		//
		
		// Standard translation of implication
		if (!GRFreeSubExpr1 || !GRFree)
		{

			ltlExpr = new Expression[]
			{
				new Implication(
					Disjunction.makeDisjunction(
						new Conjunction(ltlSubExpr1[0].copy(), new Negation(ltlExpr[0].copy())),
						new Conjunction(ltlSubExpr1[1].copy(), new Negation(ltlExpr[1].copy())),
						new Conjunction(ltlSubExpr1[2].copy(), new Negation(ltlExpr[2].copy())),
						new Conjunction(ltlSubExpr1[3].copy(), new Negation(ltlExpr[3].copy()))
					),
					ltlExpr[0]),
				new Implication(
					Disjunction.makeDisjunction(
						new Conjunction(ltlSubExpr1[0].copy(), new Negation(ltlExpr[0].copy())),
						new Conjunction(ltlSubExpr1[1].copy(), new Negation(ltlExpr[1].copy())),
						new Conjunction(ltlSubExpr1[2].copy(), new Negation(ltlExpr[2].copy())),
						new Conjunction(ltlSubExpr1[3].copy(), new Negation(ltlExpr[3].copy()))
					),
					ltlExpr[1]),
				new Implication(
					Disjunction.makeDisjunction(
						new Conjunction(ltlSubExpr1[0].copy(), new Negation(ltlExpr[0].copy())),
						new Conjunction(ltlSubExpr1[1].copy(), new Negation(ltlExpr[1].copy())),
						new Conjunction(ltlSubExpr1[2].copy(), new Negation(ltlExpr[2].copy())),
						new Conjunction(ltlSubExpr1[3].copy(), new Negation(ltlExpr[3].copy()))
					),
					ltlExpr[2]),
				new Implication(
					Disjunction.makeDisjunction(
						new Conjunction(ltlSubExpr1[0], new Negation(ltlExpr[0].copy())),
						new Conjunction(ltlSubExpr1[1], new Negation(ltlExpr[1].copy())),
						new Conjunction(ltlSubExpr1[2], new Negation(ltlExpr[2].copy())),
						new Conjunction(ltlSubExpr1[3], new Negation(ltlExpr[3].copy()))
					),
					ltlExpr[3])
			};

		}

		// Optimized translation
		else
		{

			ltlExpr = new Expression[]
			{
				new Implication(ltlSubExpr1[0], ltlExpr[0]),
				new Implication(ltlSubExpr1[1], ltlExpr[1]),
				new Implication(ltlSubExpr1[2], ltlExpr[2]),
				new Implication(ltlSubExpr1[3], ltlExpr[3])
			};
		}


		// Update GRFree
		GRFree = GRFree && GRFreeSubExpr1;

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
		boolean GRFreeSubExpr1 = GRFree;

		
		//
		// Depending on whether the both subexpressions are GR-free, use optimized translation
		//
		
		// Standard translation of implication
		if (!GRFreeSubExpr1 || !GRFree)
		{

			ltlExpr = new Expression[]
			{
				new Negation(Conjunction.makeConjunction(ltlExpr[0].copy(), ltlExpr[1].copy(), ltlExpr[2].copy(), ltlExpr[3].copy())),
				new Negation(Conjunction.makeConjunction(ltlExpr[0].copy(), ltlExpr[1].copy(), ltlExpr[2].copy(), ltlExpr[3].copy())),
				new Negation(Conjunction.makeConjunction(ltlExpr[0].copy(), ltlExpr[1].copy(), ltlExpr[2].copy(), ltlExpr[3].copy())),
				new Negation(Conjunction.makeConjunction(ltlExpr[0], ltlExpr[1], ltlExpr[2], ltlExpr[3]))
			};

		}

		// Optimized translation
		else
		{

			ltlExpr = new Expression[]
			{
				new Negation(ltlExpr[0]),
				new Negation(ltlExpr[1]),
				new Negation(ltlExpr[2]),
				new Negation(ltlExpr[3])
			};

		}


		// Update GRFree
		GRFree = GRFree && GRFreeSubExpr1;

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

		
		// Update GRFree
		GRFree = false;

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
		throw new UnsupportedOperationException("Translation of robust until operator not implemented");
	}


	/**
	 * Converts a robust release expressions.
	 *
	 * @param releaseExpr The release expression to convert
	 */
	@Override
	public void visit(Release releaseExpr)
	{
		throw new UnsupportedOperationException("Translation of robust release operator not implemented");
	}

}