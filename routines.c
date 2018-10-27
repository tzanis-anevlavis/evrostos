/*
 *  Evrostos: The rLTL Verifier
 *  Authors: Tzanis Anevlavis, Daniel Neider, Matthew Philippe, Paulo Tabuada
 *  Copyright (C) 2018, Tzanis Anevlavis, Daniel Neider, Matthew Philippe, Paulo Tabuada
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful;
 *  See the GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

void write2input(char* rLTLspec, char* filenameOUT){
    // Here we copy the terminal input to a file to call our rLTL2LTL translator
    // rLTL formula: rLTLspec
    // Output file: filenameOUT

    FILE *fptrOUT;
 
    // Open a file for writing
    fptrOUT = fopen(filenameOUT, "w");

    if (fptrOUT == NULL)
    {
        printf("Cannot open file to write rLTL specification %s \n", filenameOUT);
        exit(0);
    }
 
    // Write rLTLspec to .txt file
    fprintf(fptrOUT, "%s", rLTLspec);
    
    fclose(fptrOUT);
}

void appendLTLSPECS(char *formula, FILE *fptrOUT, char bitNo, bool silent){
    // Here we copy a formula to the .smv file
    // *formula: LTL specification
    // *fptrOUT : Output file pointer
    // silent = true     -> not print
    // silent = false    -> print

    char c;
    char *p = formula;
 
    // Read contents from pointer *p and append to output file
    char spec[22];
    sprintf(spec, "LTLSPEC NAME BIT%c := ", bitNo);

    // char* spec = "LTLSPEC NAME ";
    fputc('\n', fptrOUT);
    fprintf(fptrOUT, "%s", spec);

    int length = strlen(p);

    for (int i=0; i<length; i=i+1)
    {
        c = p[i];
        if (c=='='){
            fputc('-',fptrOUT);
        }
        else{
             fputc(c, fptrOUT);
        }
        if ((c=='F')|(c=='G')|(c=='X')|(c=='U')|(c=='R')|(c=='!')){
            fputc(' ',fptrOUT);
        }
    }

    if (silent==false) 
        printf("\nContents copied.\n");
}

void appendContent2(char *formula, FILE *fptrOUT, bool silent){
    // Use this to copy to report
    // *formula: LTL specification
    // *fptrOUT : Output file pointer
    // silent = true     -> not print
    // silent = false    -> print

    char c;
    char *p = formula;

    int length = strlen(p);

    for (int i=0; i<length; i=i+1)
    {
        c = p[i];
        fputc(c, fptrOUT);
    }

    if (silent==false) 
        printf("\nContents copied.\n");
}

void copyFile(char* filenameIN, char* filenameOUT,bool silent){
    // Copy a .txt/.smv file to a new file.
    // Origin file: filenameIN
    // Destination file: filenameOUT
    // silent = true     -> not print
    // silent = false    -> print

    FILE *fptr1, *fptr2;
    char c;
 
    // Open one file for reading
    fptr1 = fopen(filenameIN, "r");
    if (fptr1 == NULL)
    {
        printf("Cannot open file %s \n", filenameIN);
        exit(0);
    }
 
    // Open another file for writing
    fptr2 = fopen(filenameOUT, "w");
    if (fptr2 == NULL)
    {
        printf("Cannot open file %s \n", filenameOUT);
        exit(0);
    }
 
    // Read contents from file
    c = fgetc(fptr1);
    while (c != EOF)
    {
        fputc(c, fptr2);
        c = fgetc(fptr1);
    }
    
    if (silent==false)
    {
        printf("\nFile copied.\n");
    }

    fclose(fptr1);
    fclose(fptr2);
}

int checkMCres(){
    // Checks the model checking result from NuSMV

    FILE *fptr1;
    char c;
 
    // Open MC result file
    fptr1 = fopen("ltl_mc_res.txt", "r");
    if (fptr1 == NULL)
    {
        printf("Cannot open file %s \n", "ltl_mc_res.txt");
        exit(0);
    }
 
    // Read truth value 0 or 1 from file
    c = fgetc(fptr1);

    if (c=='1')
        return 1;
    else if (c=='0')
        return 0;
    else
    {   
        printf("\nNot valid result in LTL MC\n");
        exit(0);
    }


    printf("\nLTL MC result read.\n");
 
    fclose(fptr1);
}

void startupRoutine(int argc, char **argv, char *modelName, char *inputName, char *reportName, int *lines){
    printf("\nThis is Evrostos: the rLTL Verifier! -- version 1.0 . \nAuthors: Tzanis Anevlavis, Daniel Neider, Matthew Philippe, Paulo Tabuada. \nCopyright (C) 2018, Tzanis Anevlavis, Daniel Neider, Matthew Philippe, Paulo Tabuada. \n\nFor any issue contact Tzanis Anevlavis @ janis10@ucla.edu . \n\n");

    // Interface and input options
    if (argc==1)
    {
        printf("Invalid input options. Enter input options or type '-h' for help.\n");
        exit(0);
    }
    // Help message
    if (!strcmp(argv[1], "-h"))
    {
        printf("Options usage:\n -h: help - displays this message\n -i: rLTL specification input is given via terminal\n -I: rLTL specification input is given via .txt file\n");
        exit(0);
    }
    // Input the rLTL specification
    if(!strcmp(argv[1], "-i"))  // input from keyboard
    {
        printf("Enter the rLTL specification:\n");
        scanf("%s", inputName); 

        *lines = 1;
    }
    else if (!strcmp(argv[1], "-I"))    // input from .txt file
    {
        printf("Enter the rLTL specification input file name (.txt):\n");

        // Use this to put specification directly to this pointer.
        scanf("%s", inputName);

        FILE *fptr;
        fptr = fopen(inputName, "r");
        if (fptr == NULL)
        {
            printf("Cannot open file %s \n", inputName);
            exit(0);
        }
        *lines =  lineNumber(fptr);
        fclose(fptr);

    }
    else    // invalid option
    {
        printf("Invalid input options. Enter input options or type '-h' for help.\n");
        exit(0);
    }

    // Input the Model .smv file
    printf("Enter the model file name (.smv):\n");

    // Use this to put filename directly to this pointer.
    scanf("%s", modelName);

    // Enter a name for the report .txt file
    printf("Enter file name (.txt) for the report:\n");

    // Use this to put filename directly to this pointer.
    scanf("%s", reportName);
}

void compileReport(double clocktime, FILE *fptr, char* modelName, char* rLTLformula, char* LTLformula1, char* LTLformula2, char* LTLformula3, char* LTLformula4, char* rLTLresult, int inputNo){
    // Compile final report with:
    // Original rLTL formula
    // LTL translations
    // rLTL model checking result and execution time
    
    if (inputNo==0) // First time we open to write in the report
    {
        // Title
        fprintf(fptr, "-------------------- Evrostos: the rLTL Verifier! --------------------\n---------------------- rLTL Verification Report ----------------------\n\n");

        // Model used
        fprintf(fptr, "Model used: ");
        fprintf(fptr, "%s", modelName);
    }

    // Original rLTL formula
    fprintf(fptr, "\n\n----------------------------------------------------------------------\nOriginal rLTL Formula No%d:\n", inputNo+1);

    appendContent2(rLTLformula, fptr, true);

    // Translated LTL formulas
    fprintf(fptr, "\n\nTranslated LTL formulas:\n");

    appendContent2(LTLformula1, fptr, true);
    appendContent2(LTLformula2, fptr, true);
    appendContent2(LTLformula3, fptr, true);
    appendContent2(LTLformula4, fptr, true);

    // rLTL model checking result:
    fprintf(fptr, "\nModel Checking of the original rLTL formula took %f seconds, and returns truth value %s.", clocktime, rLTLresult);
}

int lineNumber(FILE *fptr1){
    char c;
    int count = 0;
    bool flag = false;
 
    // Read contents from file
    c = fgetc(fptr1);
    while (c != EOF)
    {
        flag = true;

        if (c=='\n')
        {
            count = count + 1;
        }
        c = fgetc(fptr1);
    }

    if (flag==false)
    {
        printf("\nInput file is empty!\n");
        exit(0);
    }
    else
    {
        count = count + 1;
        return count;
    }
}
