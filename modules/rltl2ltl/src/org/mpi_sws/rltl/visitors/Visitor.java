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
 * Visitor interface for expressions, which all concrete expression visitors
 * must implement.
 *
 * @author Daniel Neider
 *
 * @version 1.0
 */
public interface Visitor
{

	/**
 	 * Visits an atomic expression.
	 *
	 * @param expr The atomic expression to visit
	 */
	public void visit(Atom expr);


	/**
 	 * Visits a conjunction.
	 *
	 * @param expr The conjunction to visit
	 */
	public void visit(Conjunction expr);


	/**
 	 * Visits a disjunction.
	 *
	 * @param expr The disjunction to visit
	 */
	public void visit(Disjunction expr);


	/**
 	 * Visits an implication.
	 *
	 * @param expr The implication to visit
	 */
	public void visit(Implication expr);


	/**
 	 * Visits a negation.
	 *
	 * @param expr The negation to visit
	 */
	public void visit(Negation expr);


	/**
 	 * Visits a next expression.
	 *
	 * @param expr The next expression to visit
	 */
	public void visit(Next expr);


	/**
 	 * Visits a finally expression.
	 *
	 * @param expr The finally expression to visit
	 */
	public void visit(Finally expr);


	/**
 	 * Visits a globally expression.
	 *
	 * @param expr The globally expression to visit
	 */
	public void visit(Globally expr);


	/**
 	 * Visits an until expression.
	 *
	 * @param expr The until expression to visit
	 */
	public void visit(Until expr);


	/**
 	 * Visits a release expression.
	 *
	 * @param expr The release expression to visit
	 */
	public void visit(Release expr);
}