//  Copyright © 2020 Tzanis Anevlavis. All rights reserved.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
// See the GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.
//
//
// This code is part of the Evrostos: The rLTL Verifier repository, 
// and is publicly available at: https://github.com/janis10/evrostos .
//
// For any comments contact Tzanis Anevlavis @ janis10@ucla.edu.
//

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vector>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <sys/stat.h>
#include <unistd.h>

using namespace std;

// Check if filename exists:
inline bool nameExists (const string& name) {
  struct stat buffer;   
  return (stat (name.c_str(), &buffer) == 0); 
}

void startupRoutine(int argc, const char *argv[], string &modelName, vector<string> &rLTLformulas, string &reportName, int &select, string &flags){
    // Welcome message:
    cout << "\nThis is Evrostos: the rLTL Verifier! -- version 1.0 . \nAuthors: Tzanis Anevlavis, Daniel Neider, Matthew Philippe, Paulo Tabuada. \nCopyright (C) 2018, Tzanis Anevlavis, Daniel Neider, Matthew Philippe, Paulo Tabuada. \n\nFor any issue contact Tzanis Anevlavis @ janis10@ucla.edu . \n\n";
    cout << "Specify input file:" << endl;
    string fileName, mcName;
    getline (cin,fileName);
    ifstream inputFile(fileName);
    if(!inputFile){
      cout << "Input file not fount, check input file name.." << endl;
      exit (EXIT_FAILURE);
    }

    // Start parsing input files:
    // Recall the structure of the input file:
    // Model Checker:
    // value.. 
    //
    // Model Name:
    // value..
    //
    // rLTLspecs: number_of_specs
    // value..
    // .
    // .
    // value.. 
    //
    // Flags:
    // value.. 
    string line;
    while (getline(inputFile,line)){
        transform(line.begin(),line.end(),line.begin(),::tolower);
        if (line.compare("modelchecker:")==0 || line.compare("model checker:")==0)
            getline(inputFile,mcName);
        if (line.compare("modelname:")==0 || line.compare("model name:")==0)
            getline(inputFile,modelName);
        if (line.substr(0,line.length()-1).compare("rltlspecs:")==0 || line.substr(0,line.length()-1).compare("rltlspecs: ")==0 || line.substr(0,line.length()-1).compare("rltl specs: ")==0 || line.substr(0,line.length()-1).compare("rltl specs:")==0){
            int numSpecs = line.back()-'0';
            for (int i=0; i<numSpecs; i++){
                string spec;
                getline(inputFile,spec);
                rLTLformulas.push_back(spec);
            }
        }
        if (line.compare("flags:")==0)
            getline(inputFile,flags);
    }
    // If not all parameters found:
    if (mcName.empty() || modelName.empty() || rLTLformulas.empty()){
        cout << "Not all required parameters specified.." << endl;
        exit (EXIT_FAILURE);
    }

    transform(mcName.begin(),mcName.end(),mcName.begin(),::tolower);
    string suf = modelName.substr(modelName.length()-4); 
    if (mcName.compare("spin")==0){
        select = 2;
        if (suf.compare(".pml")!=0){
            cout << "Model-checker is SPIN, but model not in .pml extension.." << endl;
            exit (EXIT_FAILURE);
        }
    }
    else if (mcName.compare("nusmv")==0){
        select = 1;
        if (suf.compare(".smv")!=0){
            cout << "Model-checker is NuSMV, but model not in .smv extension.." << endl;
            exit (EXIT_FAILURE);
        }
    }
    else{
        cout << "Model-checker selection not available, please select either 'SPIN' or 'NuSMV'.." << endl;
        exit (EXIT_FAILURE);
    }

    // If model-checker is SPIN, and the "-ltl" flag is forgotten, append it:
    if (select==2)
        if (flags.find("-ltl")==string::npos)
            flags.append(" -ltl");
    
    // Report name:
    reportName = "evrostos-report.txt";
    int i = 0;
    while (nameExists(reportName)){
        i++;
        string idx = to_string(i);
        reportName = "evrostos-report-"+idx+".txt";
    }
}
        

// Function to append a string mystr, to file fileName:
void write2file(string fileName, string mystr){
    ofstream myfile;
    myfile.open (fileName, ios::out | ios::app); // open file for writing, and append to the end.
    
    if(!myfile){
      cout << "Error while writing rLTL spec to temp file for translation: could not create temp model file.. " << endl;
      exit (EXIT_FAILURE);
    }
    if (myfile.is_open()){
        myfile << mystr << endl;
        myfile.close();
    }
    else{
        cout << "Error while writing rLTL spec to temp file for translation: unable to open file.." << endl;
        exit(EXIT_FAILURE);
    }
}

// Function to substitute all "from" substrings in "str", with "to".
void replaceAll(string &str, const string &from, const string &to) {
    if(from.empty())
        return;

    size_t start_pos = 0;
    while((start_pos = str.find(from,start_pos))!=string::npos) {
        str.replace(start_pos, from.length(), to);
        start_pos += to.length(); // In case 'to' contains 'from', like replacing 'x' with 'yx'
    }
}


// Function to append vector of strings mystr, to file fileName.
void appendLTLspecs(string fileName, vector<string> &mystr, int select){
    
    // If SPIN is used, change the LTL operators, to the ones SPIN uses.
    if (select==2){
        for (int i=0; i<mystr.size(); i++){
            replaceAll(mystr[i],"G","[]");
            replaceAll(mystr[i],"F","<>");
            replaceAll(mystr[i],"R","V");
            replaceAll(mystr[i],"&","&&");
            replaceAll(mystr[i],"|","||");
            replaceAll(mystr[i], "F", "<>");
        }
    }
    
    ofstream myfile;
    myfile.open (fileName, ios::out | ios::app); // open file for writing, and append to the end.
    
    if (myfile.is_open()){
        // if NuSMV used:
        if (select==1){
            for (int i=0; i<mystr.size(); i++){
                myfile << "LTLSPEC NAME BIT" << 4-i << " := " << mystr[3-i] << endl;
            }
        }
        // if SPIN used:
        else if (select==2){
            for (int i=0; i<mystr.size(); i++){
                myfile << "ltl p" << 4-i << " { " << mystr[3-i] << " } " << endl;
            }
        }
        myfile.close();
    }
    else{
        cout << "Error while appending LTL specs to temp model file: unable to open file.." << endl;
        exit(EXIT_FAILURE);
    }
}


// Function to append vector of strings mystr, to file fileName
void copy2file(string fileNameIN, string fileNameOUT){
    string line;
    ifstream fileIN(fileNameIN);
    if(!fileIN){
      cout << "Error while copying model to temp file: original model file not found.. " << endl;
      exit (EXIT_FAILURE);
    }
    ofstream fileOUT;
    fileOUT.open (fileNameOUT, ios::out); // create file for writing.
    if(!fileOUT){
      cout << "Error while copying model to temp file: could not create temp model file.. " << endl;
      exit (EXIT_FAILURE);
    }
    
    while (getline(fileIN,line))
        fileOUT << line << endl;
}

int checkbit(int select){
    // Checks the model checking result from NuSMV and SPIN.
    // -- we have modified NuSMV and SPIN to write a to file named "bitvalue.txt"
    // to communicate the value of model checking.
    string res;
    ifstream file("./bitvalue.txt");
    getline(file,res);
    remove("./bitvalue.txt");
    
    // NuSMV reports the truth value (0 or 1) of the LTL spec.,
    if (select==1)
        if (res=="1")
            return 1;
        else
            return 0;
    // SPIN reports the number of error trails, therefore
    // if res>0, then truth value is 0.
    if (select==2)
        if (res=="0")
            return 1;
        else
            return 0;
}

string bitwise_modelcheck(int select, string flags){
    int truthVal;
    string command, tempmodel;
    string bits[4];
    if (select==1){
        command = "./modules/NuSMV-2.6.0/NuSMV/build/bin/NuSMV ";
        command.append(flags);
        bits[0]=" 0", bits[1]=" 1", bits[2]=" 2", bits[3]=" 3";
        tempmodel = " tempmodel.smv";
    }
    else if (select==2){
        command = "./modules/Spin/Src/spin ";
        command.append(flags);
        bits[0]=" p4", bits[1]=" p3", bits[2]=" p2", bits[3]=" p1";
        tempmodel = " tempmodel.pml";
    }
    else{
        cout << "Error in bitwise_modelcheck: not valid model-checker selection.." << endl;
        exit(EXIT_FAILURE);
    }

    // Check the 4th bit:
    // comment: for spin use either -a or -bfs after -search //
    string cmdstr = command+bits[0]+tempmodel;
    const char *cmd4 = cmdstr.c_str();
    system(cmd4);
    // Check result
    truthVal = checkbit(select);
    if (truthVal==0)
        return "0000";
    // Check the 3rd bit:
    cmdstr = command+bits[1]+tempmodel;
    const char *cmd3 = cmdstr.c_str();
    system(cmd3);
    // Check result
    truthVal = checkbit(select);
    if (truthVal==0)
        return "0001";
    // Check the 2nd bit:
    cmdstr = command+bits[2]+tempmodel;
    const char *cmd2 = cmdstr.c_str();
    system(cmd2);
    // Check result
    truthVal = checkbit(select);
    if (truthVal==0)
        return "0011";
    // Check the 1st bit:
    cmdstr = command+bits[3]+tempmodel;
    const char *cmd1 = cmdstr.c_str();
    system(cmd1);
    // Check result
    truthVal = checkbit(select);
    if (truthVal==0)
        return "0111";
    else
        return "1111";
}

void compileReport(float clock_time, string reportName, string modelName, string rLTLformula, vector<string> LTLformulas, string rLTLresult, int inputNo, int select){
    // Compile final report with:
    // Original rLTL formula
    // LTL translations
    // rLTL model checking result and execution time

    ofstream myfile;
    myfile.open(reportName, ios::out | ios::app); // open file for writing, and append to the end.
    
    if (!(myfile.is_open())){
        cout << "Error while compiling report: unable to open report file.." << endl;
        exit(EXIT_FAILURE);
    }
    
    if (inputNo==0){ // First time we open to write in the report
        // Title
        myfile << "----------------------------------------------------------------------" << endl;
        myfile << "-------------------- Evrostos: the rLTL Verifier! --------------------" << endl;
        myfile << "---------------------- rLTL Verification Report ----------------------" << endl;
        myfile << "----------------------------------------------------------------------" << endl << endl;
        // Underlying model-checker used:
        myfile << "----------------------------------------------------------------------" << endl;
        if (select==1)
            myfile << "Model-checker used: NuSMV" << endl;
        else if (select==2)
            myfile << "Model-checker used: SPIN" << endl;
        else{
            cout << "Model-checker selection not available, please select either 'SPIN' or 'NuSMV'.." << endl;
            myfile.close();
            const char *repName = reportName.c_str();
            remove(repName);
            exit (EXIT_FAILURE);
        }
        myfile << "----------------------------------------------------------------------" << endl << endl;
        // Model name:
        myfile << "----------------------------------------------------------------------" << endl;
        myfile << "Model name: " << modelName << endl;
        myfile << "----------------------------------------------------------------------" << endl << endl;
    }
    // Original rLTL formula
    myfile << "----------------------------------------------------------------------" << endl;
    myfile << "Original rLTL Formula No" << inputNo+1 << ":" << endl;
    myfile << rLTLformula << endl << endl;
    // Translated LTL formulas
    myfile << "Translated LTL formulas:" << endl;
    for (int i=0; i<LTLformulas.size(); i++){
        myfile << LTLformulas[i] << endl;
    }
    myfile << endl;
    // rLTL model checking result:
    myfile << "Model Checking of the original rLTL formula took " << clock_time<< " seconds, and" << endl;
    myfile << "returned truth value " << rLTLresult << "." << endl;
    myfile << "----------------------------------------------------------------------" << endl;
    
    myfile.close();
}
