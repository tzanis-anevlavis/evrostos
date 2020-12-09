//
//  main.cpp
//  evrostos-cpp-test
//
//  Created by Tzanis Anevlavis on 4/6/20.
//  Copyright © 2020 Tzanis Anevlavis. All rights reserved.
//

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <sys/shm.h>
#include <sys/stat.h>
#include <ctime>
#include <vector>
#include <fstream>
#include "routines.cpp"

using namespace std;
        
int main(int argc, const char * argv[]) {

    int select;
    vector<string> rLTLformulas;
    string modelName, reportName, flags;
    startupRoutine(argc, argv, modelName, rLTLformulas, reportName, select, flags);

    string rLTLresult;
    // For EACH rLTL specification in the input file:
    for (int spec=0; spec<rLTLformulas.size(); spec++){
        // Start recording time:
        const clock_t begin_time = clock();
        
        // STEP 1: rLTL to LTL translation
        // Write the rLTL formula to pre-specified text file:
        write2file("./rLTLinput.txt", rLTLformulas[spec]);
        // rLTL to LTL translation:
        system("java -jar ./helpers/rltl2ltl/rltl2ltl.jar ./rLTLinput.txt  -o ./LTLoutput.txt");
        //Delete the temporary rLTL input file
        remove("rLTLinput.txt");

        // STEP 2: Model Checking
        // Copy the 4 LTL formulas from the output of the translator to a vector:
        vector<string> LTLformulas(4);
        ifstream file("./LTLoutput.txt");
        for (int i=0; i<4; i++)
            getline(file,LTLformulas[i]);
        //Delete the temporary LTL output file
        remove("./LTLoutput.txt");
        
        // Copy model to a temporary file to append the LTL specifications without modifying the original:
        string modspecs;
        if (select==1)
            modspecs = "./tempmodel.smv";
        else if (select==2)
            modspecs = "./tempmodel.pml";
        copy2file(modelName, modspecs);
        // Append the LTL specifications:
        appendLTLspecs(modspecs, LTLformulas, select);
        // Run Model Checking bit-by-bit
        rLTLresult = bitwise_modelcheck(select,flags);
        //Delete the temporary model file
        char char_array1[modspecs.length()+1];
        strcpy(char_array1, modspecs.c_str());
        remove(char_array1);
        // If SPIN is used delete the trail file as well.
        if (select==2){
            modspecs.append(".trail");
            char char_array2[modspecs.length()+1];
            strcpy(char_array2, modspecs.c_str());
            remove(char_array2);
        }
        
        // Stop time counting:
        float end_time = static_cast<double>( clock() - begin_time ) /  CLOCKS_PER_SEC;
        cout << "Time: " << end_time << " sec." << endl;

        // STEP 4: Compile report
        compileReport(end_time, reportName, modelName, rLTLformulas[spec], LTLformulas, rLTLresult, spec, select);
    }
    // Remove "pan" exec leftover from SPIN execution. 
    remove("pan");
    
    cout << "Verification complete!" << endl;
    
    return 0;
}
