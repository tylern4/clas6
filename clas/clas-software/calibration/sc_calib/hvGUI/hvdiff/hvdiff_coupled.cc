//Standard C++ headers:
//do i need all of these?
#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <math.h>

using namespace std;

/*----------------------------------- Program Execution -----------------------------------*/

void Write_DiffFile(const string &locInputFileName1, const string &locInputFileName2, const string &locOutputFileName1);
void Display_Help();

int hvForwardOnlyFlag, hvLargeOnlyFlag;

//do not change anything below!
int main(int argc, char *argv[]){
  cout << "Author: Paul Mattione (pmatt@jlab.org)" << endl;
  cout << "Run with '-H' or '-h' switch for help." << endl;

  string locInputFileName1, locInputFileName2;
  string locOutputFileName;
  int loc_i, locFileCounter = 0;
  hvForwardOnlyFlag = 0;
  hvLargeOnlyFlag = 0;
  for(loc_i = 1; loc_i < argc; loc_i++){
    if(argv[loc_i][0] != '-'){
      if(locFileCounter == 0){
        locInputFileName1 = argv[loc_i];
        locOutputFileName = locInputFileName1;
        locFileCounter++;
      }else if(locFileCounter == 1){
        locInputFileName2 = argv[loc_i];
        locOutputFileName += locInputFileName2;
        locOutputFileName += "_diff.txt";
        locFileCounter++;
      }
    }else{ //it's a switch:
      switch(argv[loc_i][1]){
        case 'h': //display help
          Display_Help();
          return 0; 
        case 'H': //display help
          Display_Help();
          return 0; 
        case 'F': //display help
          hvForwardOnlyFlag = 1;
          break;
        case 'f': //display help
          hvForwardOnlyFlag = 1;
          break;
        case 'L': //display help
          hvLargeOnlyFlag = 1;
          break;
        case 'l': //display help
          hvLargeOnlyFlag = 1;
          break;
      } //kills switch
    }
  } //kills for loop
  if((hvForwardOnlyFlag == 1) && (hvLargeOnlyFlag == 1)){
    cout << "ERROR: CANNOT USE FORWARD-ONLY AND LARGE-ONLY SWITCHES SIMULTANEOUSLY. EXITING." << endl;
    return 0;
  }

  Write_DiffFile(locInputFileName1, locInputFileName2, locOutputFileName);

  return 0;
}

void Display_Help(){
  cout << endl;
  cout << "Command Line: ExecutableFile FirstInputFile SecondInputFile -F/L" << endl;
  cout << "First and Second Input Files should be in standard .snap format, or no gaurantee this will work." << endl;
  cout << "Make the first file the one with the earlier date, to make it less confusing." << endl;
  cout << "The output filename is ugly: FirstInputFileSecondInputFile_diff.txt" << endl;
  cout << "Use the -F flag if you want to look at the difference of forward angle counters only." << endl;
  cout << "Use the -L flag if you want to look at the difference of large angle counters only." << endl;
  cout << "Using both the -F and -L flags will exit the program." << endl;
}

void Write_DiffFile(const string &locInputFileName1, const string &locInputFileName2, const string &locOutputFileName){

  ifstream locStream1(locInputFileName1.c_str());
  ifstream locStream2(locInputFileName2.c_str());

  ofstream locNumStream(locOutputFileName.c_str());

  int loc_i, loc_j, loc_k;
  double locNum1, locNum2;
  string locInput1, locInput2;

  vector<vector<vector<string> > > locLine1Vector, locLine2Vector;
  vector<vector<vector<double> > > locNum1Vector, locNum2Vector;

  locLine1Vector.resize(6);
  locLine2Vector.resize(6);
  locNum1Vector.resize(6);
  locNum2Vector.resize(6);
  for(loc_i = 0; loc_i < 6; loc_i++){
    locLine1Vector[loc_i].resize(48 + 9);
    locLine2Vector[loc_i].resize(48 + 9);
    locNum1Vector[loc_i].resize(48 + 9);
    locNum2Vector[loc_i].resize(48 + 9);
    for(loc_j = 0; loc_j < (48 + 9); loc_j++){
      locLine1Vector[loc_i][loc_j].resize(2);
      locLine2Vector[loc_i][loc_j].resize(2);
      locNum1Vector[loc_i][loc_j].resize(2);
      locNum2Vector[loc_i][loc_j].resize(2);
      for(loc_k = 0; loc_k < 2; loc_k++){
        locLine1Vector[loc_i][loc_j][loc_k] = "NULL";
        locLine2Vector[loc_i][loc_j][loc_k] = "NULL";
        locNum1Vector[loc_i][loc_j][loc_k] = 0.0;
        locNum2Vector[loc_i][loc_j][loc_k] = 0.0;
      }
    }
  }

  double locDiff;
  int locSector, locCounter, locOkFlag, locShift, locLRIndex;
  while(!locStream1.eof()){
    locStream1 >> locInput1;
    if(int(locInput1.size()) >= 18){
      string loc16Char(locInput1, 16, 1);
      string loc17Char(locInput1, 17, 1);
      string loc18Char(locInput1, 18, 1);
      locOkFlag = 0;
      locShift = 0;
      if((loc16Char == "D") && (loc17Char == "V"))
        locOkFlag = 1;
      else if((loc17Char == "D") && (loc18Char == "V")){
        locOkFlag = 1;
        locShift = 1;
      }
      if(locOkFlag == 1){
        string locSChar(locInput1, 9, 1);
        locSector = atoi(locSChar.c_str());
        string locCounterChar(locInput1, 11, 2);
        locCounter = atoi(locCounterChar.c_str());
        string locABChar(locInput1, 13, 1);
        if(locABChar == "B")
          locCounter += 9;
        string locLRChar(locInput1, 14 + locShift, 1);
        if(locLRChar == "L")
          locLRIndex = 0;
        else if(locLRChar == "R")
          locLRIndex = 1;
        else{continue;}
//B_hv_SC_S4_40B_L_DV
//cout << "sector, counter, lr: " << locSector << ", " << locCounter << ", " << locLRIndex << endl;
        if(!((hvForwardOnlyFlag == 1) && (locCounter > 23)) && !((hvLargeOnlyFlag == 1) && (locCounter <= 23))){
          locLine1Vector[locSector - 1][locCounter - 1][locLRIndex] = locInput1;
          locStream1 >> locInput1;
          locStream1 >> locNum1;
          locNum1Vector[locSector - 1][locCounter - 1][locLRIndex] = locNum1;
        }
      }
    }
  }
  locStream1.close();

  while(!locStream2.eof()){
    locStream2 >> locInput2;
    if(int(locInput2.size()) >= 18){
      string loc16Char(locInput2, 16, 1);
      string loc17Char(locInput2, 17, 1);
      string loc18Char(locInput2, 18, 1);
      locOkFlag = 0;
      locShift = 0;
      if((loc16Char == "D") && (loc17Char == "V"))
        locOkFlag = 1;
      else if((loc17Char == "D") && (loc18Char == "V")){
        locOkFlag = 1;
        locShift = 1;
      }
      if(locOkFlag == 1){
        string locSChar(locInput2, 9, 1);
        locSector = atoi(locSChar.c_str());
        string locCounterChar(locInput2, 11, 2);
        locCounter = atoi(locCounterChar.c_str());
        string locABChar(locInput2, 13, 1);
        if(locABChar == "B")
          locCounter += 9;
        string locLRChar(locInput2, 14 + locShift, 1);
        if(locLRChar == "L")
          locLRIndex = 0;
        else if(locLRChar == "R")
          locLRIndex = 1;
        else{continue;}
//B_hv_SC_S4_40B_L_DV
//cout << "sector, counter, lr: " << locSector << ", " << locCounter << ", " << locLRIndex << endl;
        if(!((hvForwardOnlyFlag == 1) && (locCounter > 23)) && !((hvLargeOnlyFlag == 1) && (locCounter <= 23))){
          locLine2Vector[locSector - 1][locCounter - 1][locLRIndex] = locInput2;
          locStream2 >> locInput2;
          locStream2 >> locNum2;
          locNum2Vector[locSector - 1][locCounter - 1][locLRIndex] = locNum2;
        }
      }
    }
  }
  locStream2.close();

  locNumStream << "TOF HV Differences between files:" << endl;
  locNumStream << "FirstFileName: " << locInputFileName1 << endl;
  locNumStream << "SecondFileName: " << locInputFileName2 << endl;
  if(hvForwardOnlyFlag == 1)
    locNumStream << "FORWARD ANGLE DIFFERENCES ONLY" << endl;
  else if(hvLargeOnlyFlag == 1)
    locNumStream << "LARGE ANGLE DIFFERENCES ONLY" << endl;
  locNumStream << "NOTE: Only PMTs that changed HV by > 1 V are listed." << endl;
  locNumStream << "NOTE: The First Column is PMT, then FirstFileHV, then SecondFileHV, then Delta HV Magnitude." << endl;
  locNumStream << "NOTE: Delta HV Magnitude is calculated as: -1.0*(SecondFileHV - FirstFileHV)." << endl; 
  for(loc_i = 0; loc_i < 6; loc_i++){
    for(loc_j = 0; loc_j < (48 + 9); loc_j++){
      for(loc_k = 0; loc_k < 2; loc_k++){
        locDiff = -1.0*(locNum2Vector[loc_i][loc_j][loc_k] - locNum1Vector[loc_i][loc_j][loc_k]);
        if((locLine1Vector[loc_i][loc_j][loc_k] != "NULL") && (locLine2Vector[loc_i][loc_j][loc_k] != "NULL") && (fabs(locDiff) > 1.0)){
          locNumStream << locLine1Vector[loc_i][loc_j][loc_k];
          locNumStream << ": ";
          locNumStream << locNum1Vector[loc_i][loc_j][loc_k];
          locNumStream << ", ";
          locNumStream << locNum2Vector[loc_i][loc_j][loc_k];
          locNumStream << ", ";
          locNumStream << locDiff;
          locNumStream << endl;
        }
      }
    }
  }
}

