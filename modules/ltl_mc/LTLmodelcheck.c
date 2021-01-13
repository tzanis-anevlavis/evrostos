/*
 *  Author: Tzanis Anevlavis (janis10@ucla.edu)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "routines4ltl.c"
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/stat.h>
#include <time.h>

int main(){
	char *modelName[100], *inputName[100], *reportName[100];
    int linesNum;

	// Program inputs and interface
    startupRoutineLTL(modelName, inputName, reportName, &linesNum);

    char *LTLformulas[linesNum];

    // Input is from .txt file put content to rLTLformulas[0->linesNum-1]
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
        LTLformulas[i]=NULL;
        getline(&LTLformulas[i], &len, fptr);
	}
	fclose(fptr);

    // Create file for report
    FILE *fptrRep;
    fptrRep = fopen(reportName, "a");
    if (fptrRep == NULL)
    {
        printf("Cannot open file %s \n", reportName);
        exit(0);
    }

    for (int input=0; input<linesNum; input=input+1)        // For EACH LTL specification
    {
        // Copy model to a temporary file to append the LTL specifications to it
        copyFile(modelName,"modelANDspecs.smv",true);
        FILE *fptrMS;
        fptrMS = fopen("modelANDspecs.smv", "a");
        appendLTLSPECS(LTLformulas[input], fptrMS, '0', true);
        //Close the temporary model file
        fclose(fptrMS);

        // Run Model Checking
        int truthVal;

		// Execution Timing - Begin:
		struct timeval  tv1, tv2;
		gettimeofday(&tv1, NULL);

		// Add NuSMV to current path and run NuSMV
		system("./NuSMV-2.6.0/NuSMV/build/bin/NuSMV -coi -df -dcx -dynamic modelANDspecs.smv");

		// Check result
		truthVal = checkMCres();
		remove("ltl_mc_res.txt");

		//Delete the temporary model file
        remove("modelANDspecs.smv");

        // Execution Timing - End:
		gettimeofday(&tv2, NULL);
		double time_spent = (double) (tv2.tv_usec - tv1.tv_usec) / 1000000 + (double) (tv2.tv_sec - tv1.tv_sec);

		compileReportLTL(time_spent, fptrRep, modelName, LTLformulas[input], truthVal, input);
	}

	fclose(fptrRep);
    printf("\nReport complete!\n");
 
    printf("\nSimulations Complete!\n");

	return 0;
}