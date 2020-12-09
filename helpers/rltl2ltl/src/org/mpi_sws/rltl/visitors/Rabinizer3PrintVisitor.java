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
 * A print visitor for creating a textual representation of an expression in
 * Rabinizer3 format.
 * <p>
 * This format is very similar to the classical LTL syntax but does not support
 * implications and the release operator. These cases are rewritten into their
 * equivalent LTL expressions.
 *
 * @author Daniel Neider
 *
 * @version 1.0
 */
public class Rabinizer3PrintVisitor extends PrettyPrintVisitor
{

	/**
 	 * Prints an implication in Rabinizer3 syntax.
	 *
	 * @param implication The implication to print
	 */
	@Override
	public void visit(Implication implication)
	{

		builder.append("(!");
		implication.subExpr1.accept(this);
		builder.append(" | ");
		implication.subExpr2.accept(this);
		builder.append(")");

	}


	/**
 	 * Prints a release expression in Rabinizer3 syntax.
	 *
	 * @param releaseExpr The release expression to print
	 */
	@Override
	public void visit(Release releaseExpr)
	{

		builder.append("!(!(");
		releaseExpr.subExpr1.accept(this);
		builder.append(") U !(");
		releaseExpr.subExpr2.accept(this);
		builder.append("))");

	}

}