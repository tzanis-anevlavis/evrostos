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

package org.mpi_sws.rltl.main;

import java.util.*;


/**
 * An interval of integers.
 *
 * @author Daniel Neider
 *
 * @version 1.0
 */
public class Interval
{

    /**
     * The smaller (left) bound of the interval.
     */
    public int left;


    /**
     * The larger (right) bound of the interval.
     */
    public int right;


    /**
     * Creates a new Interval with the given bounds.
     *
     * @param left The smaller (left) bound of the interval
     * @param right The larger (right) bound of the interval
     */
    private Interval(int left, int right)
    {
        this.left = left;
        this.right = right;
    }

    
    /**
     * Creates a new interval with the given bounds.
     * <p>
     * Throws an exception if {@code left} < {@code right}.
     *
     * @param left The smaller (left) bound of the interval
     * @param right The larger (right) bound of the interval
     *
     * @return a new interval with the given bounds
     */
    public static Interval newInstance(int left, int right)
    {
       
        if (left > right)
        {
            throw new IllegalArgumentException("left > right");
        }

        return new Interval(left, right);

    }


    /**
     * Turns a set of integers into the smallest set of contiguous intervals
     * that contain exactly the given set of integers.
     *
     * @param setOfInts A set of integers
     *
     * @return a set of contiguous intervals
     */
    public static Set<Interval> getIntervals(SortedSet<Integer> setOfInts)
    {

		// Check arguments
		if (setOfInts == null || setOfInts.isEmpty())
		{
			throw new IllegalArgumentException("No values given");
		}


        // Create result set
        HashSet<Interval> intervals = new HashSet<>();

        // Turn set into array
        Integer[] arr = setOfInts.toArray(new Integer[0]);

        // Iterate over all integers
        int pos = 0;
        while (pos < arr.length)
        {

            int left = arr[pos];

            // Forward over contiguous integers
            while (pos + 1 < arr.length && arr[pos + 1] == arr[pos] + 1)
            {
                ++pos;
            }


            intervals.add(Interval.newInstance(left, arr[pos]));
            ++pos;

        }


        return intervals;

    }


    /**
     * Returns a textual representation of this interval.
     *
     * @return a textual representation of this interval
     */
    public String toString()
    {
        return "[" + left + "; " + right + "]";
    }

} 