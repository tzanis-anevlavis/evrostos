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
 * An atomic LTL expression (i.e., an atomic predicate with an arbitrary name or
 * a Boolean value). There is no distinction between atomic predicates and
 * Boolean values.
 *
 * @author Daniel Neider
 *
 * @version 1.0
 */
public class Atom extends Expression
{

	/**
	 * The identifier of this atomic expression.
	 */
	public String identifier = null;


	/**
	 * Creates a new atomic expression with the given string as identifier.
	 *
	 * @param identifier The identifier
	 */
	public Atom(String identifier)
	{
		this.identifier = identifier;
	}


	/**
	 * Creates a copy of the given atomic expression.
	 *
	 * @param other The atomic expression to copy
	 */
	public Atom(Atom other)
	{
		this(new String(other.identifier));
	}


	@Override
	public void accept(Visitor visitor)
	{
		visitor.visit(this);
	}


	@Override
	public Expression copy()
	{
		return new Atom(this);
	}

}
