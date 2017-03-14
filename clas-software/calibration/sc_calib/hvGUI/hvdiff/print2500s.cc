//Standard C++ headers:
//do i need all of these?
#include <iostream>
#include <string>
#include <fstream>
#include <vector>

using namespace std;

/*----------------------------------- Program Execution -----------------------------------*/

void Write_DiffFile(const string &locInputFileName1, const string &locOutputFileName1);
void Display_Help();

int hvForwardOnlyFlag, hvLargeOnlyFlag;

//do not change anything below!
int main(int argc, char *argv[]){
  cout << "Author: Paul Mattione (pmatt@jlab.org)" << endl;
  cout << "Run with '-H' or '-h' switch for help." << endl;

  string locInputFileName1;
  string locOutputFileName;
  int loc_i, locFileCounter = 0;
  hvForwardOnlyFlag = 0;
  hvLargeOnlyFlag = 0;
  for(loc_i = 1; loc_i < argc; loc_i++){
    if(argv[loc_i][0] != '-'){
      if(locFileCounter == 0){
        locInputFileName1 = argv[loc_i];
        locOutputFileName = locInputFileName1;
        locOutputFileName += "_2500s.list";
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

  Write_DiffFile(locInputFileName1, locOutputFileName);

  return 0;
}

void Display_Help(){
  cout << endl;
  cout << "Command Line: ExecutableFile InputFile -F/L" << endl;
  cout << "Input File should be in standard .snap format, or no gaurantee this will work." << endl;
  cout << "The output filename is ugly: InputFile_2500s.list" << endl;
  cout << "Use the -F flag if you want to look at the 2500s of forward angle counters only." << endl;
  cout << "Use the -L flag if you want to look at the 2500s of large angle counters only." << endl;
  cout << "Using both the -F and -L flags will exit the program." << endl;
}

void Write_DiffFile(const string &locInputFileName1, const string &locOutputFileName){

  ifstream locStream1(locInputFileName1.c_str());

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
    locLine1Vector[loc_i].resize(57);
    locLine2Vector[loc_i].resize(57);
    locNum1Vector[loc_i].resize(57);
    locNum2Vector[loc_i].resize(57);
    for(loc_j = 0; loc_j < (57); loc_j++){
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
    if(int(locInput1.size()) >= 18){ //skips all header lines
      string loc16Char(locInput1, 16, 1); //char after full name of pmt + "_"
      string loc17Char(locInput1, 17, 1); //2nd char after full name of pmt + "_"
      if((loc16Char == "D") && (loc17Char == "V")){
        //e.g. B_hv_SC_S4_40_L_DV
        string locSChar(locInput1, 9, 1);
        locSector = atoi(locSChar.c_str());
        string locCounterChar(locInput1, 11, 2);
        locCounter = atoi(locCounterChar.c_str());
        string locLRChar(locInput1, 14, 1);
        if(locLRChar == "L")
          locLRIndex = 0;
        else if(locLRChar == "R")
          locLRIndex = 1;
        else{continue;} //should never happen
//cout << "sector, counter, lr: " << locSector << ", " << locCounter << ", " << locLRIndex << endl;
//        if(!((hvForwardOnlyFlag == 1) && (locCounter > 23)) && !((hvLargeOnlyFlag == 1) && (locCounter <= 23))){

        if((hvForwardOnlyFlag == 1) && (locCounter > 23))
          continue;
        if((hvLargeOnlyFlag == 1) && (locCounter <= 23))
          continue;
        locLine1Vector[locSector - 1][locCounter - 1][locLRIndex] = locInput1;
        locStream1 >> locInput1;
        locStream1 >> locNum1;
        locNum1Vector[locSector - 1][locCounter - 1][locLRIndex] = locNum1;

      }
    }
  }
  locStream1.close();

  locNumStream << "TOF HV = -2500 in file " << locInputFileName1 << endl;
  if(hvForwardOnlyFlag == 1)
    locNumStream << "FORWARD ANGLE 2500s ONLY" << endl;
  else if(hvLargeOnlyFlag == 1)
    locNumStream << "LARGE ANGLE 2500s ONLY" << endl;
  locNumStream << "NOTE: The First Column is PMT, then FirstFileHV." << endl;

  int locMinIndex = 0;
  int locMaxIndex = 57;
  if(hvForwardOnlyFlag == 1)
    locMaxIndex = 22;
  if(hvLargeOnlyFlag == 1)
    locMinIndex = 23;

  for(loc_i = 0; loc_i < 6; loc_i++){
    for(loc_j = locMinIndex; loc_j < locMaxIndex; loc_j++){
      for(loc_k = 0; loc_k < 2; loc_k++){
        if((locLine1Vector[loc_i][loc_j][loc_k] != "NULL") && (locNum1Vector[loc_i][loc_j][loc_k] < -2499.0)){
          locNumStream << locLine1Vector[loc_i][loc_j][loc_k];
          locNumStream << ": ";
          locNumStream << locNum1Vector[loc_i][loc_j][loc_k];
          locNumStream << endl;
        }
      }
    }
  }
}

