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

import java.io.*;
import java.util.*;
import org.mpi_sws.rltl.expressions.*;
import org.mpi_sws.rltl.parser.*;
import org.mpi_sws.rltl.visitors.*;


/**
 * Main program.
 *
 * @author Daniel Neider
 *
 * @version 1.0
 */
public class RLTL2LTL
{

	/**
	 * Prints help and usage information to the command line.
	 */
	public static void displayHelp()
	{
		System.out.println("Usage: RLTL2LTL {input_file} truth_values [options]");
		System.out.println("Translates an rLTL formula into an equivalent LTL formula with respect to the");
		System.out.println("given set of truth values.");
		System.out.println();
		System.out.println("The five truth values are represented by the intergers 0 to 4, where 0");
		System.out.println("represents 'false' and 4 represents 'true'. Multiple truth values can be");
		System.out.println("specified as a sequence of truth values without white spaces (e.g., 123).");
		System.out.println();
		System.out.println("If the option -i is given, the rLTL formula is read from the command line and");
		System.out.println("input_file must not be specified.");


		System.out.println();
		System.out.println("Options:");

		System.out.println("-f <format>\tUse <format> when writing outputting the final LTL formula.");
		System.out.println("\t\tSupported formats are 'classical' and 'rabinizer3'.");

		System.out.println("-h\t\tPrints this help message.");

		System.out.println("-i\t\tRead rLTL formula from command line. If this option is given,");
		System.out.println("\t\tinput_file must be omitted.");

		System.out.println("-o <file>\tWrite the final LTL formula to <file>. If this option is not");
		System.out.println("\t\tspecified, the formula is written to the command line.");

		System.out.println("-O\t\tUse the optimized translation.");

		System.out.println("-s\t\tPrints the parsed rLTL formula (mainly for debug purposes).");
	}


	// J-Edit:
	// Comment out set of intervals, we just want to do the translation to 4 LTL formulas.
	//
	// // @param truthIntervals a set of intervals of truth values (these should not overlap or be duplicated)
	// public static Expression combineExpressions(Expression[] ltlExpr, Set<Interval> truthIntervals)
	// {

	// 	// Check arguments
	// 	if (ltlExpr == null || ltlExpr.length != 4)
	// 	{
	// 		throw new IllegalArgumentException("ltlExpr is invalid");
	// 	}
	// 	if (truthIntervals == null || truthIntervals.isEmpty())
	// 	{
	// 		throw new IllegalArgumentException("truthIntervals is invalid");
	// 	}


	// 	// Construct disjunction over all intervals
	// 	Expression result = null;
	// 	for (Interval i : truthIntervals)
	// 	{

	// 		Expression leftBitExpr = i.right < 4 ? new Negation(ltlExpr[3 - i.right].copy()) : null;
	// 		Expression rightBitExpr = i.left > 0 ? ltlExpr[4 - i.left].copy() : null;

	// 		Expression curExpr = null;
	// 		if (leftBitExpr == null && rightBitExpr == null)
	// 		{
	// 			curExpr = new Atom("true");
	// 		}
	// 		else if (leftBitExpr != null && rightBitExpr == null)
	// 		{
	// 			curExpr = leftBitExpr;
	// 		}
	// 		else if (leftBitExpr == null && rightBitExpr != null)
	// 		{
	// 			curExpr = rightBitExpr;
	// 		}
	// 		else
	// 		{
	// 			curExpr = new Conjunction(leftBitExpr, rightBitExpr);
	// 		}


	// 		result = result == null ? curExpr : new Disjunction(result, curExpr);

	// 	}


	// 	return result == null ? new Atom("false") : result;

	// }


	/**
	 * Main entry point of the program.
	 *
	 * @param args The command line arguments
	 */
	public static void main(String[] args)
	{
		
		try
		{

			//
			// Parse command line options
			//
			CommandLineInterface cli = CommandLineInterface.parse(args);
			ArrayList<String> nonoptionalArguments = cli.getNonoptionalArguments();


			//
			// Display help message if desired
			//
			if (cli.showHelp())
			{
				displayHelp();
				System.exit(0);
			}


			// J-Edit:
			// Comment out set of intervals code, we just want to do the translation to 4 LTL formulas.
			// //
			// // Check correct number of arguments
			// //
			// if (cli.readFromConsole() && nonoptionalArguments.size() != 1)
			// {
			// 	throw new CommandLineParseError("Set of truth values required");
			// }
			// else if (!cli.readFromConsole() && nonoptionalArguments.size() != 2)
			// {
			// 	throw new CommandLineParseError("Name of input file required and set of truth values");
			// }

			
			// //
			// // Parse set of truth values into intervals
			// //

			// // Parse truth values from command line arguments
			// SortedSet<Integer> truthValues = CommandLineInterface.parseIntervals(nonoptionalArguments.get(nonoptionalArguments.size() - 1));
			// if (truthValues.size() == 0)
			// {
			// 	//throw new CommandLineParseError("No truth values given, result will always be the constant expression 'false'");
			// }
			// if (truthValues.size() == 5)
			// {
			// 	//throw new CommandLineParseError("All truth values given, result will always be the constant expression 'true'");
			// }

			// // Parse into intervals
			// Set<Interval> truthIntervals = Interval.getIntervals(truthValues);


			//
			// Parse input
			//
			LTLParser parser = null;

			// Set input source
			if (cli.readFromConsole())
			{
				parser = new LTLParser(new BufferedReader(new InputStreamReader(System.in)));
			}
			else
			{
				parser = new LTLParser(new BufferedReader(new FileReader(cli.getNonoptionalArguments().get(0))));
			}

			// Parse
			Expression rltlExpr = parser.expression();			


			//
			// Output parsed formula if desired (mainly for debug purposes)
			//
			if (cli.showParsedFormula())
			{
				System.out.println("Original expression: " + (new PrettyPrintVisitor()).expression2String(rltlExpr));
			}


			//
			// Do the conversation
			//
			Expression[] ltlExpr = null;
			if (cli.useOptimizedTranslation())
			{
				ltlExpr = OptimizedRLTL2LTLVisitor.convert(rltlExpr);
			}
			else
			{
				ltlExpr = RLTL2LTLVisitor.convert(rltlExpr);
			}

			
			// J-edit:
			// Instead of the below.....
			// //
			// // Combine ltl expressions depending on selected truth values
			// //
			// Expression finalLTLExpr = combineExpressions(ltlExpr, truthIntervals);

			//
			// .. create array of 4 LTL formulas
			//
			Expression[] finalLTLExpr = ltlExpr;


			//
			// Write output
			//
			PrintWriter outWriter = null;			

			// Choose output destination
			if (cli.getOutFile() == null)
			{
				outWriter = new PrintWriter(System.out);
			}
			else
			{
				outWriter = new PrintWriter(cli.getOutFile());
			}

			//
			// Write output specific to chosen format
			//
			PrintVisitor visitor = null;
			switch (cli.getOutputFormat())
			{

				case CLASSICAL:
					visitor = new PrettyPrintVisitor();
					break;

				case RABINIZER3:
					visitor = new Rabinizer3PrintVisitor();
					break;

			}

			// J-edit: 
			//
			// outWriter.println(visitor.expression2String(finalLTLExpr));
			// outWriter.close();
			//
			// Print the array of 4 LTL formulas instead:
			// 
			outWriter.println(visitor.expression2String(finalLTLExpr[0]));
			outWriter.println(visitor.expression2String(finalLTLExpr[1]));
			outWriter.println(visitor.expression2String(finalLTLExpr[2]));
			outWriter.println(visitor.expression2String(finalLTLExpr[3]));

			outWriter.close();
		}

		// Catch command line parsing errors
		catch (CommandLineParseError e)
		{
			System.out.println(e.getMessage());
			System.exit(1);
		}

		// Catch I/O exceptions
		catch (IOException e)
		{
			System.out.println(e.getMessage());
			System.exit(1);
		}

		// Catch LTL parser errors
		catch (Throwable e) {
			// Catching Throwable is ugly but JavaCC throws Error objects!
			System.out.println("Error: " + e.getMessage());
			System.exit(1);
		}


	}

}