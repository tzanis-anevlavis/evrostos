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
 * This class represents an error that occurred during parsing the command line
 * arguments.
 *
 * @author Daniel Neider
 *
 * @version 1.0
 */
class CommandLineParseError extends Exception
{

	/**
	 * Default constructor.
	 */
	public CommandLineParseError()
	{
		super();
	}	

	/**
	 * Creates a new CommandLineParseError with ethe given error message.
	 *
	 * @param message The error message of this error
	 */
	public CommandLineParseError(String message)
	{
		super(message);
	}

}


/**
 * This class represents the interface to read options from the command line.
 * The class also implements a parser for command line arguments.
 *
 * @author Daniel Neider
 * 
 * @version 1.0
 */
public class CommandLineInterface
{

	/**
	 * Enumeration of supported output formats for LTL expressions.
	 */
	public enum OutputFormat
	{
		CLASSICAL,
		RABINIZER3
	}


	/**
	 * The output format to use.
	 */
	private OutputFormat outFormat = OutputFormat.CLASSICAL;


	/**
	 * The name of the file to write to. If this is null, write to the concole.
	 */
	private String outFile = null;


	/**
	 * Print the parsed formula to the console.
	 */
	private boolean showParsedFormula = false;


	/**
	 * Indicates whether the input should be read from the console instead of a file.
	 */
	private boolean readFromConsole = false;


	/**
	 * Indicated whether to use the optimizied translation.
	 */
	private boolean useOptimizedTranslation = false;


	/**
	 * Indicates whether to diplay the help message.
	 */
	private boolean showHelp = false;


	/**
	 * List of non-optional arguments
	 */
	private ArrayList<String> nonoptionalArguments = new ArrayList<String>();


	/**
	 * Private default constructor.
	 */
	private CommandLineInterface()
	{
		// Nothing
	}


	/**
	 * Returns the output format selected by the user.
	 *
	 * @return the output format
	 */
	public OutputFormat getOutputFormat()
	{
		return outFormat;
	}


	/**
	 * Returns the output file specified by the user.
	 *
	 * return the output file
	 */
	public String getOutFile()
	{
		return outFile;
	}


	/**
	 * Returns whether the input should be read from the console.
	 *
	 * return whether the input should be read from the console
	 */
	public boolean readFromConsole()
	{
		return readFromConsole;
	}

	
	/**
	 * Returns whether the parsed Formula should be displayed on the console after parsing.
	 *
	 * return whether the parsed Formula should be displayed on the console
	 */
	public boolean showParsedFormula()
	{
		return showParsedFormula;
	}


	/**
	 * Returns whether to use the optimized translation.
	 *
	 * return whether to use the optimized translation
	 */
	public boolean useOptimizedTranslation()
	{
		return useOptimizedTranslation;
	}


	/**
	 * Returns whether to display the help message.
	 *
	 * return whether to display the help message
	 */
	public boolean showHelp()
	{
		return showHelp;
	}


	/**
	 * Returns the list of non-optional arguments.
	 *
	 * @return the list of non-optional arguments
	 */
	public ArrayList<String> getNonoptionalArguments()
	{
		return nonoptionalArguments;
	}


	/**
	 * Parses the command line arguments and constructs a CommandLineInterface object
	 * carrying the parsed arguments.
	 *
	 * @param arguments The raw arguments to parse
	 *
	 * @return a CommandLineInterface object carrying the parsed arguments
	 */
	public static CommandLineInterface parse(String[] arguments) throws CommandLineParseError
	{

		//
		// Check arguments
		//
		if (arguments == null)
		{
			throw new IllegalArgumentException("No options given");
		}

		//
		// Create new interface
		//
		CommandLineInterface cli = new CommandLineInterface();


		//
		// Parse
		//
		int i = 0;
		while(i < arguments.length)
		{

			String arg = arguments[i].trim();

			// Output file definition
			if (arg.equals("-o"))
			{

				// Check existence of argument specifying the output file				
				if (i + 1 >= arguments.length || arguments[i + 1].trim().startsWith("-"))
				{
					throw new CommandLineParseError("No output file specified");
				}

				cli.outFile = arguments[i + 1];
				i += 2;

			}

			// Select output format
			else if (arg.equals("-f"))
			{

				// Check existence of argument specifying the output format
				if (i + 1 >= arguments.length)
				{
					throw new CommandLineParseError("No output format specified");
				}

				String nextArg = arguments[i + 1].trim();

				if (nextArg.startsWith("-"))
				{
					throw new CommandLineParseError("No output format specified");
				}

				// Parse output format
				if (nextArg.toLowerCase().equals("classical"))
				{
					cli.outFormat = OutputFormat.CLASSICAL;
				}
				else if (nextArg.toLowerCase().equals("rabinizer3"))
				{
					cli.outFormat = OutputFormat.RABINIZER3;
				}
				else
				{
					throw new CommandLineParseError("Output format " + nextArg + " not supported");
				}

				i += 2;


			}

			// Read from console
			else if (arg.equals("-h"))
			{
				cli.showHelp = true;
				++i;
			}

			// Read from console
			else if (arg.equals("-i"))
			{
				cli.readFromConsole = true;
				++i;
			}

			// Show parsed formula
			else if (arg.equals("-s"))
			{
				cli.showParsedFormula = true;
				++i;
			}

			// Show parsed formula
			else if (arg.equals("-O"))
			{
				cli.useOptimizedTranslation = true;
				++i;
			}

			// Unknown option
			else if (arg.startsWith("-"))
			{
				throw new CommandLineParseError("Unknown option " + arg);
			}

			// Mandatory arguments
			else
			{
				cli.nonoptionalArguments.add(arg);
				++i;
			}

		}


		return cli;

	}



	/**
	 * Parses a string into individual truth values (i.e., integer values in the interval [0, 4]).
	 *
	 * @param str The string to parse
	 *
	 * @return the parsed set of truth values
	 */
	public static SortedSet<Integer> parseIntervals(String str) throws CommandLineParseError
	{

		//
		// Parse individual truth values
		//
		TreeSet<Integer> truthValues = new TreeSet<Integer>();
		for (int i = 0, n = str.length(); i < n; ++i)
		{

			char c = str.charAt(i);

			// Check that the current character is decimal
			if (!Character.isDigit(c))
			{
				throw new CommandLineParseError("Unknown truth value " + c);
			}

			int value = Character.getNumericValue(c);
			if (value < 0 || value > 4)
			{
				throw new CommandLineParseError("Unknown truth value " + c);
			}


			truthValues.add(value);

		}


		return truthValues;

	}


}
