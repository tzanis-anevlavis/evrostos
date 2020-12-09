/*
 *  Author: Tzanis Anevlavis (janis10@ucla.edu)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

void appendLTLSPECS(char *formula, FILE *fptrOUT, char bitNo, bool silent){
    // Here we copy a formula to the .smv file
    // Input formula: formula
    // Output file: fptrOUT
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
    // Here we copy a formula to the report file
    // Input formula: formula
    // Output file: fptrOUT
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
    // Make a copy of the filenameIN .txt file to the filenameOUT .txt file.
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

void startupRoutineLTL(char *modelName, char *inputName, char *reportName, int *lines){
    // Input the LTL specification
    printf("Enter the LTL specification input file name (.txt):\n");
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

    // Input the Model .smv file
    printf("Enter the model file name (.smv):\n");
    // Use this to put filename directly to this pointer.
    scanf("%s", modelName);

    // Enter a name for the report .txt file
    printf("Enter file name (.txt) for the report:\n");
    // Use this to put filename directly to this pointer.
    scanf("%s", reportName);
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

void compileReportLTL(double clocktime, FILE *fptr, char* modelName, char* LTLformula, int LTLresult, int inputNo){
    // Compile final report with:
    // Original LTL formula
    // LTL model checking result
    
    if (inputNo==0) // First time we open to write in the report
    {
        // Title
        fprintf(fptr, "---------------------- Report for corresponding LTL specifications ----------------------\n\n");

        // Model used
        fprintf(fptr, "Model used: ");
        fprintf(fptr, "%s", modelName);
    }

    // Original rLTL formula
    fprintf(fptr, "\n\n----------------------------------------------------------------------\nLTL Formula No%d:\n", inputNo+1);

    appendContent2(LTLformula, fptr, true);

    // LTL model checking result:
    fprintf(fptr, "\nModel Checking of the LTL formula took %f seconds, and returns truth value %d.", clocktime, LTLresult);
}
