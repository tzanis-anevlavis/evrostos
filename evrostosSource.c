/*
 *  Evrostos: The rLTL Verifier
 *  Authors: Tzanis Anevlavis, Daniel Neider, Matthew Philippe, Paulo Tabuada
 *  Copyright (C) 2018  UCLA
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "routines.c"
#include <sys/shm.h>
#include <sys/stat.h>
#include <time.h>

int main(int argc, char **argv){

    // Pointers for functions:
    //  linesNum = number of input lines = number of rLTL specifications
    //  formulas = array of char pointers = the rLTL specifications
    //  inputName = rLTL formula if -i is used; name of rLTL input file if -I is used
    //  modelName = contains the model's filename

    char *modelName[100], *inputName[100], *reportName[100];
    int linesNum;

	// Program inputs and interface
    startupRoutine(argc, argv, modelName, inputName, reportName, &linesNum);

    char *rLTLformulas[linesNum];

    if(!strcmp(argv[1], "-i"))  // if input is from keyboard put the content to rLTLformulas[0]
    {
        rLTLformulas[0] = inputName;
        printf("\n%s\n", rLTLformulas[0]);
    }
    else if(!strcmp(argv[1], "-I"))  // if input is from .txt file put content to rLTLformulas[0->linesNum-1]
    {
        FILE *fptr;
        fptr = fopen(inputName, "r");
        if (fptr == NULL)
        {
            printf("Cannot open file %s \n", inputName);
            exit(0);
        }

        //Get the contents of each line
        size_t len = 0;
        for (int i=0; i<linesNum; i=i+1)
        {
            rLTLformulas[i]=NULL;
            getline(&rLTLformulas[i], &len, fptr);
        }

        fclose(fptr);
    }

    // Create file for report
    FILE *fptrRep;
    fptrRep = fopen(reportName, "a");
    if (fptrRep == NULL)
    {
        printf("Cannot open file %s \n", reportName);
        exit(0);
    }
    
    char* rLTLresult;
    for (int input=0; input<linesNum; input=input+1)        // For EACH rLTL specification
    {
        write2input(rLTLformulas[input],"rLTLinput.txt"); // write the formula to pre-specified text file

        // STEP 1: rLTL to LTL translation  //////////////////////////////////////////////////////////////////////////////////////////////////////
        struct timeval  tv1, tv2;
        gettimeofday(&tv1, NULL);


     	// Run the rLTL to LTL translation
		system("java -jar ./rltl2ltl-master/rltl2ltl.jar ./rLTLinput.txt  -o ./LTLoutput.txt");

        //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // STEP 2: Model Checking using NuSMV   //////////////////////////////////////////////////////////////////////////////////////////////////

        // Copy the 4 LTL formulas to 4 different char pointers for efficient rLTL Model Checking
        // Open the Simplified LTL formulas:
        FILE *fptr;
        fptr = fopen("./LTLoutput.txt", "r");
        if (fptr == NULL)
        {
            printf("Cannot open file %s \n", "./LTLoutput.txt");
            exit(0);
        }
        
        size_t len = 0;

        char *LTLformula1 = NULL;
        char *LTLformula2 = NULL;
        char *LTLformula3 = NULL;
        char *LTLformula4 = NULL;

        getline(&LTLformula1, &len, fptr);
        getline(&LTLformula2, &len, fptr);
        getline(&LTLformula3, &len, fptr);
        getline(&LTLformula4, &len, fptr);

        fclose(fptr);

        remove("./LTLoutput.txt");
        
        // Copy model to a temporary file to append the LTL specifications to it
        copyFile(modelName,"modelANDspecs.smv",true);
        FILE *fptrMS;
        fptrMS = fopen("modelANDspecs.smv", "a");
        appendLTLSPECS(LTLformula4, fptrMS, '4', true);
        appendLTLSPECS(LTLformula3, fptrMS, '3', true);
        appendLTLSPECS(LTLformula2, fptrMS, '2', true);
        appendLTLSPECS(LTLformula1, fptrMS, '1', true);

        //Close the temporary model file
        fclose(fptrMS);

        // Run Model Checking bit-by-bit
        bool flag = true;
        int truthVal;

        // Check the 4th bit:
        if (flag==true)
        {
            // Add NuSMV to current path and run NuSMV
            system("./NuSMV-2.6.0/NuSMV/build/bin/NuSMV -coi -df -dcx -dynamic -n 0 modelANDspecs.smv");

            // Check result
            truthVal = checkMCres();
            if (truthVal==0)
            {
                rLTLresult = "0000";
                flag = false;
            }
            remove("ltl_mc_res.txt");
        }

        // Check the 3rd bit:
        if (flag==true)
        {
            // Add NuSMV to current path and run NuSMV
            system("./NuSMV-2.6.0/NuSMV/build/bin/NuSMV -coi -df -dcx -dynamic -n 1 modelANDspecs.smv");

            // Check result
            truthVal = checkMCres();
            if (truthVal==0)
            {
                rLTLresult = "0001";
                flag = false;
            }
            remove("ltl_mc_res.txt");
        }

        // Check the 2nd bit:
        if (flag==true)
        {
            // Add NuSMV to current path and run NuSMV
            system("./NuSMV-2.6.0/NuSMV/build/bin/NuSMV -coi -df -dcx -dynamic -n 2 modelANDspecs.smv");

            // Check result
            truthVal = checkMCres();
            if (truthVal==0)
            {
                rLTLresult = "0011";
                flag = false;
            }
            remove("ltl_mc_res.txt");
        }

        // Check the 1st bit:
        if (flag==true)
        {
            // Add NuSMV to current path and run NuSMV
            system("./NuSMV-2.6.0/NuSMV/build/bin/NuSMV -coi -df -dcx -dynamic -n 3 modelANDspecs.smv");

            // Check result
            truthVal = checkMCres();
            if (truthVal==0)
            {
                rLTLresult = "0111";
                flag = false;
            }
            else
            {
                rLTLresult = "1111";
            }
            remove("ltl_mc_res.txt");
        }

        //Delete the temporary model file
        remove("modelANDspecs.smv");

        //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // Stop time counting:
        gettimeofday(&tv2, NULL);
        double time_spent = (double) (tv2.tv_usec - tv1.tv_usec) / 1000000 + (double) (tv2.tv_sec - tv1.tv_sec);

        // STEP 4: Compile report   //////////////////////////////////////////////////////////////////////////////////////////////////////////////
        compileReport(time_spent, fptrRep, modelName, rLTLformulas[input], LTLformula1, LTLformula2, LTLformula3, LTLformula4, rLTLresult, input);
    }
    fclose(fptrRep);
    printf("\nReport complete!\n");

    // Delete intermediate files
    if (remove("rLTLinput.txt")==0)
        printf("\nCleared intermediate files successfully.\n");
    else
        printf("\nUnable to delete a file\n");
 
    printf("\nVerification Complete!\n");

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    return 0;
}