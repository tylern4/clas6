//Standard C++ headers:
#include <iostream>
#include <sstream>
#include <string>
#include <fstream>
#include <vector>
#include <math.h>
#include <stdio.h>

#include "TFile.h"
#include "TMath.h"
#include "TNtuple.h"
#include "TObject.h"
#include "TH1D.h"
#include "TH2D.h"
#include "TF1.h"

using namespace std;

/*--------------------------------------- Cut Values --------------------------------------*/

double tfLivetimeCutValue = 0.97, tfFrequencyCutValue = 400.0;

/*----------------------------------- Program Execution -----------------------------------*/

int tfDebugFlag = 0;
int tfBOSFileSplitFlag = 0;
void Display_Help();
bool Get_RunAndFileNumber(const string& locGFluxFileName, int& locRunNumber, int& locFileNumber, int& locPartNumber);
void Determine_BadTripFiles(const string &locGFluxDirectory, const string& locTripFileDirectory);
void Build_TripFileName(string& locTripFileName, int locRunNumber, int locFileNumber, int locPartNumber);
void Test_TripFiles(const string& locGoodTripFileList);
void Make_NewTripFiles_List(const string& locAllTripFileList);
void Make_NewTripFiles_File(const string& locTripFileName, int &locTotalNumRegionsLost, int &locTotalNumRegionsGained, int &locTotalNumBadRegionsUnchanged, int& locTotalNumGoodRegionsUnchanged);
void Toss_TripRegions(const vector<int>& locNumEventsVector, const vector<int>& locEventFrequencyVector, const vector<double>& locLiveTimeVector, const vector<int>& locTripFlagVector, vector<int>& locTossedEntries, int locCutOffFlag);

int main(int argc, char *argv[]){
  cout << "Author: Paul Mattione (pmatt@jlab.org)" << endl;
  cout << "Run with '-H' or '-h' switch for help." << endl;

  string locGFluxDirectory, locTripFileDirectory;
  int loc_i, locFileCounter = 0, locInputFlag = -1;
  double locLivetimeCutValue, locFrequencyCutValue;

  for(loc_i = 1; loc_i < argc; loc_i++){
    if(argv[loc_i][0] != '-'){
      if(locFileCounter == 0){
        locGFluxDirectory = argv[loc_i];
        locFileCounter++;
      }else if(locFileCounter == 1){
        locTripFileDirectory = argv[loc_i];
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
        case 'B': //determine bad trip files
          locInputFlag = 0;
          break;
        case 'T': //test cuts
          locInputFlag = 1;
          break;
        case 'F': //make new trip file (single file input)
          locInputFlag = 2;
          break;
        case 'L': //make new trip files (list file input)
          locInputFlag = 3;
          break;
        case 'S':
          tfBOSFileSplitFlag = 1;
          break;
        default:
          break;
      } //kills switch
    }
  } //kills for loop

  if(locInputFlag == -1)
    return 0;

  if(locInputFlag == 0) //loop over gflux to determine bad files
    Determine_BadTripFiles(locGFluxDirectory, locTripFileDirectory);
  if(locInputFlag == 1) //eval over good files to determine cuts
    Test_TripFiles(locGFluxDirectory);
  if(locInputFlag == 2){ //create new trip files, single file
    int locTotalNumRegionsLost = 0, locTotalNumRegionsGained = 0, locTotalNumBadRegionsUnchanged = 0, locTotalNumGoodRegionsUnchanged = 0;
    Make_NewTripFiles_File(locGFluxDirectory, locTotalNumRegionsLost, locTotalNumRegionsGained, locTotalNumBadRegionsUnchanged, locTotalNumGoodRegionsUnchanged);
  }
  if(locInputFlag == 3) //create new trip files, list file
    Make_NewTripFiles_List(locGFluxDirectory);

  return 0;
}

void Display_Help(){
  cout << endl;
  cout << "Command Line is flag dependent.  Either:" << endl;
  cout << "Command Line: ExecutableFile GFluxDirectory TripFileDirectory -B" << endl;
  cout << "Command Line: ExecutableFile GoodTripFileList -T" << endl;
  cout << "Command Line: ExecutableFile TripFileName -F" << endl;
  cout << "Command Line: ExecutableFile AllTripFileList -L" << endl;
  cout << "Flag: '-B': loops over the gflux directory to determine bad files (and create an output file of them)." << endl;
  cout << "Flag: '-T': tests cuts by looping over the good trip files and making histograms." << endl;
  cout << "Flag: '-F': single file input - makes new, fixed trip files in the local directory." << endl;
  cout << "Flag: '-L': list input - makes new, fixed trip files in the local directory." << endl;
  cout << "Flag: '-S': use if RAW BOS files are split into A*B* (e.g. clas_099999.A02.B01)." << endl;
  cout << "NOTE: This program must be executed from a directory that you have write permissions to" << endl;
  cout << "NOTE: You must clean the local directory of all trip files with the same name(s) before each use." << endl;
}

void Determine_BadTripFiles(const string &locGFluxDirectory, const string& locTripFileDirectory){
  ostringstream locExecutionOStream;
  string locGFluxFileName, locFullGFluxPath, locTripFileName, locFullTripPath, locInput;
  double locTempPhotonFlux, locTempPhotonFluxError, locFilePhotonFlux;
  int locRunNumber, locFileNumber, locPartNumber;

  //build gflux file list
  system("rm -f inputlist.txt");
  locExecutionOStream.str("");
  locExecutionOStream << "ls " << locGFluxDirectory << " | grep gflux | grep _tc.a > inputlist.txt" << endl;
  system(locExecutionOStream.str().c_str());

  //create output file & ntuple
  TFile *locFile = new TFile("dh_PhotonFluxFile.root", "RECREATE", "Total Photon Flux File");
  TNtuple *locNTuple = new TNtuple("dn_PhotonFluxTotal", "Photon Flux Total", "RunNumber:FileNumber:PartNumber:NumPhotons");
  locNTuple->SetDirectory(locFile);

  //setup file streams
  system("rm -f badtripfiles.txt");
  system("rm -f goodtripfiles.txt");
  system("rm -f alltripfiles.txt");
  system("rm -f badgfluxfiles.txt");
  system("rm -f goodgfluxfiles.txt");
  ifstream locStream1("inputlist.txt");
  ofstream locTripOutStreamBad("badtripfiles.txt");
  ofstream locTripOutStreamGood("goodtripfiles.txt");
  ofstream locTripOutStreamAll("alltripfiles.txt");
  ofstream locGFluxOutStreamBad("badgfluxfiles.txt");
  ofstream locGFluxOutStreamGood("goodgfluxfiles.txt");

  //loop through gflux file list
  int loc_i;
  while(!locStream1.eof()){

    //open gflux file
    locStream1 >> locGFluxFileName;
    locFullGFluxPath = locGFluxDirectory;
    locFullGFluxPath += "/";
    locFullGFluxPath += locGFluxFileName;
    ifstream locGFluxStream(locFullGFluxPath.c_str());
    if((!locGFluxStream.is_open()) || (locGFluxFileName == "")){
      cout << "WARNING: GFlux file " << locFullGFluxPath << " doesn't exist or permission is denied." << endl;
      continue;
    }

    //Get Run# & File Number from file name
    if(!Get_RunAndFileNumber(locGFluxFileName, locRunNumber, locFileNumber, locPartNumber))
      continue;
    if(tfDebugFlag == 1)
      cout << "filename, run#, file#, part# = " << locFullGFluxPath << ", " << locRunNumber << ", " << locFileNumber << ", " << locPartNumber << endl;

    //read gflux file
    locFilePhotonFlux = 0.0;
    for(loc_i = 0; loc_i < 61; loc_i++){
      if(locGFluxStream.eof()){ //again bogus somehow...
        cout << "WARNING: GFlux file " << locFullGFluxPath << " ended prematurely." << endl;
        break;
      }

      locGFluxStream >> locTempPhotonFlux >> locTempPhotonFluxError;
      locFilePhotonFlux += locTempPhotonFlux;
    }
    if(tfDebugFlag == 1)
      cout << "totalflux = " << locFilePhotonFlux << endl;

    //fill ntuple
    locNTuple->Fill(locRunNumber, locFileNumber, locPartNumber, locFilePhotonFlux);

    //output list of trip files
    Build_TripFileName(locTripFileName, locRunNumber, locFileNumber, locPartNumber);
    if(tfDebugFlag == 1)
      cout << "tripname = " << locTripFileName << endl;
    locFullTripPath = locTripFileDirectory;
    locFullTripPath += "/";
    locFullTripPath += locTripFileName;

    ifstream locInputTripStream(locFullTripPath.c_str());
    if((!locInputTripStream.is_open()) || (locFullTripPath == "")){
      cout << "WARNING: TRIP file " << locFullTripPath << " doesn't exist or permission is denied." << endl;
      continue;
    }

    locTripOutStreamAll << locFullTripPath << endl;
    if(locFilePhotonFlux < 1.0){
      locTripOutStreamBad << locFullTripPath << endl;
      locGFluxOutStreamBad << locFullGFluxPath << endl;
    }else{ //check how many trips the file has: if mostly tripped, assume it's bad

      locInputTripStream >> locInput; //go off phase so that last one in while loop triggers .eof()
      int locTripFlag, locTripFileLineCount = 0, locTripFlagZeroCount = 0;
      while(!locInputTripStream.eof()){
        locTripFileLineCount++;
        locInputTripStream >> locInput >> locTripFlag >> locInput >> locInput >> locInput;
        locInputTripStream >> locInput >> locInput >> locInput >> locInput >> locInput >> locInput;
        if(locTripFlag == 0)
          locTripFlagZeroCount++;
        locInputTripStream >> locInput; //triggers .eof()
      }
      locInputTripStream.close();

      if(locTripFileLineCount <= 5){ //consider it good no matter what: can't gdistinguish or guarantee correct convergence
        locTripOutStreamGood << locFullTripPath << endl;
        locGFluxOutStreamGood << locFullGFluxPath << endl;
      }else{
        if(((double)locTripFlagZeroCount/(double)locTripFileLineCount) < 0.2){ //mostly trips, try to revive
          locTripOutStreamBad << locFullTripPath << endl;
          locGFluxOutStreamBad << locFullGFluxPath << endl;
        }else{
          locTripOutStreamGood << locFullTripPath << endl;
          locGFluxOutStreamGood << locFullGFluxPath << endl;
        }
      }
    }
  }

  //write ntuple, close file
  locFile->Write();
  locFile->Close();
  delete locFile;
}

bool Get_RunAndFileNumber(const string& locGFluxFileName, int& locRunNumber, int& locFileNumber, int& locPartNumber){
  istringstream locRunIStream, locFileIStream, locPartIStream;
  if(tfBOSFileSplitFlag == 0){
    //e.g. gflux53737_tc.a04.dat
    string::size_type locUnderscoreIndex = locGFluxFileName.find_last_of("_", locGFluxFileName.length() - 1);
    if(locUnderscoreIndex == string::npos)
      return false;

    locRunIStream.str(locGFluxFileName.substr(locUnderscoreIndex - 5, 5).c_str());
    if(!(locRunIStream >> locRunNumber))
      return false;

    locFileIStream.str(locGFluxFileName.substr(locUnderscoreIndex + 5, 2).c_str());
    if(!(locFileIStream >> locFileNumber))
      return false;
  }else{
    //e.g. gflux53737_tc.a04.b00.dat
    string::size_type locUnderscoreIndex = locGFluxFileName.find_last_of("_", locGFluxFileName.length() - 1);
    if(locUnderscoreIndex == string::npos)
      return false;

    locRunIStream.str(locGFluxFileName.substr(locUnderscoreIndex - 5, 5).c_str());
    if(!(locRunIStream >> locRunNumber))
      return false;

    locFileIStream.str(locGFluxFileName.substr(locUnderscoreIndex + 5, 2).c_str());
    if(!(locFileIStream >> locFileNumber))
      return false;

    locPartIStream.str(locGFluxFileName.substr(locUnderscoreIndex + 9, 2).c_str());
    if(!(locPartIStream >> locPartNumber))
      return false;
  }
  return true;
}

void Build_TripFileName(string& locTripFileName, int locRunNumber, int locFileNumber, int locPartNumber){
  ostringstream locOStream;
  if(tfBOSFileSplitFlag == 0){
    //e.g. clas_053600.A20.trip
    locOStream << "clas_0" << locRunNumber << ".A";
    if(locFileNumber < 10)
      locOStream << "0";
    locOStream << locFileNumber << ".trip";
    locTripFileName = locOStream.str();
  }else{
    //e.g. clas_053600.A20.B00.trip
    locOStream << "clas_0" << locRunNumber << ".A";
    if(locFileNumber < 10)
      locOStream << "0";
    locOStream << locFileNumber << ".B";
    if(locPartNumber < 10)
      locOStream << "0";
    locOStream << locPartNumber << ".trip";
    locTripFileName = locOStream.str();
  }
}

void Test_TripFiles(const string& locGoodTripFileList){
  string locTripFileName;
  double locInput;
  int loc_i, loc_j;

  double locLiveTime;
  int locTripFlag, locEventFrequency, locNumEvents;
  vector<int> locNumEventsVector, locEventFrequencyVector, locTripFlagVector;
  vector<double> locLiveTimeVector;

  //create output file & hists
  TFile *locFile = new TFile("dh_TripTestFile.root", "RECREATE", "Total Photon Flux File");

  TH1D *locGoodHist = new TH1D("dh_GoodLivetime", "Good Trip Files, Good Intervals;Livetime %", 500, 0.7, 1.0);
  TH1D *locBadHist = new TH1D("dh_BadLivetime", "Good Trip Files, Bad Intervals;Livetime %", 500, 0.7, 1.0);
  TH1D *locGoodHist_Frequency = new TH1D("dh_GoodFrequency", "Good Trip Files, Good Intervals;Frequency", 500, -0.5, 499.5);
  TH1D *locBadHist_Frequency = new TH1D("dh_BadFrequency", "Good Trip Files, Bad Intervals;Frequency", 500, -0.5, 499.5);
  TH1D *locGoodHist_NumEvents = new TH1D("dh_GoodNumEvents", "Good Trip Files, Good Intervals;NumEvents", 1000, -0.5, 199999.5);
  TH1D *locBadHist_NumEvents = new TH1D("dh_BadNumEvents", "Good Trip Files, Bad Intervals;NumEvents", 1000, -0.5, 199999.5);

  TH2D *locBadHist_2D_LF = new TH2D("dh_Bad2D_LF", "Good Trip Files, Bad Intervals;Livetime %;Frequency", 500, 0.7, 1.0, 500, -0.5, 499.5);
  TH2D *locGoodHist_2D_LF = new TH2D("dh_Good2D_LF", "Good Trip Files, Good Intervals;Livetime %;Frequency", 500, 0.7, 1.0, 500, -0.5, 499.5);
  TH2D *locBadHist_2D_LN = new TH2D("dh_Bad2D_LN", "Good Trip Files, Bad Intervals;Livetime %;NumEvents", 500, 0.7, 1.0, 1000, -0.5, 99999.5);
  TH2D *locGoodHist_2D_LN = new TH2D("dh_Good2D_LN", "Good Trip Files, Good Intervals;Livetime %;NumEvents", 500, 0.7, 1.0, 1000, -0.5, 99999.5);
  TH2D *locBadHist_2D_NF = new TH2D("dh_Bad2D_NF", "Good Trip Files, Bad Intervals;NumEvents %;Frequency", 1000, -0.5, 199999.5, 500, -0.5, 499.5);
  TH2D *locGoodHist_2D_NF = new TH2D("dh_Good2D_NF", "Good Trip Files, Good Intervals;NumEvents %;Frequency", 1000, -0.5, 199999.5, 500, -0.5, 499.5);

  ifstream locStream1(locGoodTripFileList.c_str());
  int locGuessedBad_ButGood = 0, locGuessedBad_Accurate = 0, locGuessedGood_Accurate = 0, locGuessedGood_ButBad = 0;
  int locGuessedBad_ButGood_PreviousValue = 0, locGuessedGood_ButBad_PreviousValue = 0;

  while(!locStream1.eof()){
    //open trip file
    locStream1 >> locTripFileName;
    if(tfDebugFlag == 1)
      locTripFileName = "/home/pmatt/pass1v2/TRIP/clas_053236.A06.trip";

    if(tfDebugFlag == 1)
      cout << "tripname = " << locTripFileName << endl;
    ifstream locInputTripStream(locTripFileName.c_str());
    if((!locInputTripStream.is_open()) || (locTripFileName == "")){
      cout << "WARNING: Trip file " << locTripFileName << " doesn't exist or permission is denied." << endl;
      continue; //bogus somehow...
    }

    //compile statistics from file
    locNumEventsVector.resize(0);  locEventFrequencyVector.resize(0);  locLiveTimeVector.resize(0);  locTripFlagVector.resize(0);
    locInputTripStream >> locInput; //go off phase so that last one in while loop triggers .eof()
    while(!locInputTripStream.eof()){
      locInputTripStream >> locInput >> locTripFlag >> locInput >> locInput >> locNumEvents;
      locInputTripStream >> locInput >> locEventFrequency >> locInput >> locInput >> locInput >> locLiveTime;
      //fill vectors
      locNumEventsVector.push_back(locNumEvents);
      locEventFrequencyVector.push_back(locEventFrequency);
      locLiveTimeVector.push_back(locLiveTime);
      locTripFlagVector.push_back(locTripFlag);
      locInputTripStream >> locInput; //triggers .eof()
    }
    locInputTripStream.close();

    if(locNumEventsVector.size() < 3)
      continue;

    //remove the 1st & last lines
    locNumEventsVector.erase(locNumEventsVector.begin());
    locEventFrequencyVector.erase(locEventFrequencyVector.begin());
    locLiveTimeVector.erase(locLiveTimeVector.begin());
    locTripFlagVector.erase(locTripFlagVector.begin());
    locNumEventsVector.pop_back();
    locEventFrequencyVector.pop_back();
    locLiveTimeVector.pop_back();
    locTripFlagVector.pop_back();

    //histogram quantities
    for(loc_j = 0; loc_j < int(locNumEventsVector.size()); loc_j++){
      if(locTripFlagVector[loc_j] == 0){
        locGoodHist->Fill(locLiveTimeVector[loc_j]);
        locGoodHist_Frequency->Fill(locEventFrequencyVector[loc_j]);
        locGoodHist_NumEvents->Fill(locNumEventsVector[loc_j]);
        locGoodHist_2D_LF->Fill(locLiveTimeVector[loc_j], locEventFrequencyVector[loc_j]);
        locGoodHist_2D_LN->Fill(locLiveTimeVector[loc_j], locNumEventsVector[loc_j]);
        locGoodHist_2D_NF->Fill(locNumEventsVector[loc_j], locEventFrequencyVector[loc_j]);
      }else{
        locBadHist->Fill(locLiveTimeVector[loc_j]);
        locBadHist_Frequency->Fill(locEventFrequencyVector[loc_j]);
        locBadHist_NumEvents->Fill(locNumEventsVector[loc_j]);
        locBadHist_2D_LF->Fill(locLiveTimeVector[loc_j], locEventFrequencyVector[loc_j]);
        locBadHist_2D_LN->Fill(locLiveTimeVector[loc_j], locNumEventsVector[loc_j]);
        locBadHist_2D_NF->Fill(locNumEventsVector[loc_j], locEventFrequencyVector[loc_j]);
      }
    }

    //eval trip files
    vector<int> locTossedEntries;
    Toss_TripRegions(locNumEventsVector, locEventFrequencyVector, locLiveTimeVector, locTripFlagVector, locTossedEntries, 1);

    //compare entries i flagged as bad to the original ones, compare
    int locTossedBadFlag;
    for(loc_j = 0; loc_j < int(locTripFlagVector.size()); loc_j++){
      //check to see if this entry was tossed
      locTossedBadFlag = 0;
      for(loc_i = 0; loc_i < int(locTossedEntries.size()); loc_i++){
        //the last entry wasn't bad, it was simply the worst before the delta-mean got evaluated as stable (converged)
        if(locTossedEntries[loc_i] == loc_j){ //tossed due to bad delta-avg
          locTossedBadFlag = 1;
          break;
        }
      }
      if(tfDebugFlag == 1)
        cout << "j, locTossedBadFlag = " << loc_j << ", " << locTossedBadFlag << endl;
      if(locTripFlagVector[loc_j] == 0) //a good region
        (locTossedBadFlag == 1) ? locGuessedBad_ButGood++ : locGuessedGood_Accurate++;
      else{(locTossedBadFlag == 1) ? locGuessedBad_Accurate++ : locGuessedGood_ButBad++;} //a bad region
    }
    if(locGuessedGood_ButBad > locGuessedGood_ButBad_PreviousValue){
      cout << "guessed good but orig bad filename = " << locTripFileName << endl;
      locGuessedGood_ButBad_PreviousValue = locGuessedGood_ButBad;
    }
    if(locGuessedBad_ButGood > locGuessedBad_ButGood_PreviousValue){
      cout << "guessed bad but orig good filename = " << locTripFileName << endl;
      locGuessedBad_ButGood_PreviousValue = locGuessedBad_ButGood;
    }
    if(tfDebugFlag == 1)
      break;
  }

  cout << "locGuessedBad_ButGood = " << locGuessedBad_ButGood << endl;
  cout << "locGuessedBad_Accurate = " << locGuessedBad_Accurate << endl;
  cout << "locGuessedGood_ButBad = " << locGuessedGood_ButBad << endl;
  cout << "locGuessedGood_Accurate = " << locGuessedGood_Accurate << endl;
  //write hist, close file
  locFile->Write();
  locFile->Close();
  delete locFile;
}

void Toss_TripRegions(const vector<int>& locNumEventsVector, const vector<int>& locEventFrequencyVector, const vector<double>& locLiveTimeVector, const vector<int>& locTripFlagVector, vector<int>& locTossedEntries, int locCutOffFlag){
  //locTossedEntries is output
  //if cutoffflag = 1, use own cuts, if 0: hist to determine what cuts are
  int loc_j, loc_i;
  double locAverageLiveTime, locAverageEventFrequency, locAverageNumEvents;
  double locLargestLiveTimeDeviation, locLargestEventFrequencyDeviation, locLargestNumEventsDeviation;
  int locLargestLiveTimeDeviationIndex, locLargestEventFrequencyDeviationIndex, locLargestNumEventsDeviationIndex;
  vector<int> locOrigEntryLine;
  vector<int> locNumEventsVectorSubset = locNumEventsVector, locEventFrequencyVectorSubset = locEventFrequencyVector;
  vector<double> locLiveTimeVectorSubset = locLiveTimeVector;
  vector<double> locAverageLiveTimeVector, locAverageEventFrequencyVector, locAverageNumEventsVector;
  vector<double> locNumEventsDeviationVector, locEventFrequencyDeviationVector, locLiveTimeDeviationVector;

  TH1D *locCutOffMinHist, *locCutOffMaxHist;
  if(locCutOffFlag == 0){
    locCutOffMinHist = new TH1D("dh_CutOffMinHist", "Good Trip Files, CutOff Min Hist;NumEvents Ratio", 1000, 0.0, 1.0);
    locCutOffMaxHist = new TH1D("dh_CutOffMaxHist", "Good Trip Files, CutOff Max Hist;NumEvents Ratio", 1000, 0.0, 1.0);
  }

  for(loc_j = 0; loc_j < int(locNumEventsVector.size()); loc_j++)
    locOrigEntryLine.push_back(loc_j);

  //toss out ridiculously bad entries:
  for(loc_j = locNumEventsVectorSubset.size() - 1; loc_j >= 0; loc_j--){
    if((locEventFrequencyVectorSubset[loc_j] > tfFrequencyCutValue) || (locLiveTimeVectorSubset[loc_j] > tfLivetimeCutValue)){
      locNumEventsVectorSubset.erase(locNumEventsVectorSubset.begin() + loc_j);
      locEventFrequencyVectorSubset.erase(locEventFrequencyVectorSubset.begin() + loc_j);
      locLiveTimeVectorSubset.erase(locLiveTimeVectorSubset.begin() + loc_j);

      locTossedEntries.push_back(locOrigEntryLine[loc_j]);
      locOrigEntryLine.erase(locOrigEntryLine.begin() + loc_j);
      if(tfDebugFlag == 1)
        cout << "tossed orig index " << locTossedEntries[locTossedEntries.size() - 1] << endl;
    }
  }

  if(locNumEventsVectorSubset.size() == 0)
    return;
  //guess which remaining regions are bad by:
  //calc average & deviation of each quantity, reject the largest deviation that is less than the mean
  //repeat until deviation/average is small
  //NOTE: this won't converge correctly if the #trips is high: 
    //assume that the run will be rejected with other studies
  int locSearchCounter = 0;
  do{
    //exits upon break (when deviation/average is small) or if only 1 line left

    //calc average quantities
    locAverageLiveTime = 0.0;  locAverageEventFrequency = 0.0;  locAverageNumEvents = 0.0;
    for(loc_i = 0; loc_i < int(locNumEventsVectorSubset.size()); loc_i++){
      locAverageLiveTime += locLiveTimeVectorSubset[loc_i];
      locAverageEventFrequency += locEventFrequencyVectorSubset[loc_i];
      locAverageNumEvents += locNumEventsVectorSubset[loc_i];
    }
    locAverageLiveTime /= locNumEventsVectorSubset.size();
    locAverageEventFrequency /= locNumEventsVectorSubset.size();
    locAverageNumEvents /= locNumEventsVectorSubset.size();
    locAverageLiveTimeVector.push_back(locAverageLiveTime);
    locAverageEventFrequencyVector.push_back(locAverageEventFrequency);
    locAverageNumEventsVector.push_back(locAverageNumEvents);
    if(tfDebugFlag == 1)
      cout << "avg #events = " << locAverageNumEvents << endl;

    //calc deviations from the average
    locNumEventsDeviationVector.resize(0);  locEventFrequencyDeviationVector.resize(0);  locLiveTimeDeviationVector.resize(0);
    for(loc_i = 0; loc_i < int(locNumEventsVectorSubset.size()); loc_i++){
      locLiveTimeDeviationVector.push_back(locAverageLiveTime - locLiveTimeVectorSubset[loc_i]);
      locEventFrequencyDeviationVector.push_back(locAverageEventFrequency - locEventFrequencyVectorSubset[loc_i]);
      locNumEventsDeviationVector.push_back(locAverageNumEvents - locNumEventsVectorSubset[loc_i]);
      if(tfDebugFlag == 1)
        cout << "dev " << loc_i << " = " << locNumEventsDeviationVector[loc_i] << endl;
    }

    //find the largest deviation from the average
    locLargestLiveTimeDeviation = 0.0;  locLargestEventFrequencyDeviation = 0.0;  locLargestNumEventsDeviation = 0.0;
    locLargestLiveTimeDeviationIndex = 0;  locLargestEventFrequencyDeviationIndex = 0;  locLargestNumEventsDeviationIndex = 0;
    for(loc_i = 0; loc_i < int(locNumEventsVectorSubset.size()); loc_i++){
      if(locLargestLiveTimeDeviation < fabs(locLiveTimeDeviationVector[loc_i])){
        locLargestLiveTimeDeviation = fabs(locLiveTimeDeviationVector[loc_i]);
        locLargestLiveTimeDeviationIndex = loc_i;
      }
      if(locLargestEventFrequencyDeviation < fabs(locEventFrequencyDeviationVector[loc_i])){
        locLargestEventFrequencyDeviation = fabs(locEventFrequencyDeviationVector[loc_i]);
        locLargestEventFrequencyDeviationIndex = loc_i;
      }
      //dev = avg - value.  so if < avg, dev is > 0
      if(locLargestNumEventsDeviation < locNumEventsDeviationVector[loc_i]){ //only look amongst those BELOW average!!!
        locLargestNumEventsDeviation = locNumEventsDeviationVector[loc_i];
        locLargestNumEventsDeviationIndex = loc_i;
      }
    }
    if(tfDebugFlag == 1)
      cout << "largest dev, value, index = " << locLargestNumEventsDeviation << ", " << locNumEventsVectorSubset[locLargestNumEventsDeviationIndex] << ", " << locLargestNumEventsDeviationIndex << endl;

    //calculate deviation/avg, determine if break
    double locDeviationPercentageCutOff = 0.1; //when dev/avg
    if(tfDebugFlag == 1)
      cout << "dev/avg cutoff, dev/avg #events = " << locDeviationPercentageCutOff << ", " << fabs(locLargestNumEventsDeviation/locAverageNumEventsVector[locSearchCounter]) << endl;
    if(locCutOffFlag == 0){ //dunno cutoff yet, cut based on orig trip regions, determine cut off from those
      if(locTripFlagVector[locOrigEntryLine[locLargestNumEventsDeviationIndex]] == 0){ //about to toss a good one, so converged
        locCutOffMinHist->Fill(fabs(locLargestNumEventsDeviation/locAverageNumEventsVector[locSearchCounter])); //cutoff needs to be the value of this or higher
        break;
        //else cutoff needs to be the value of this or lower
      }else{locCutOffMaxHist->Fill(fabs(locLargestNumEventsDeviation/locAverageNumEventsVector[locSearchCounter]));}
    }else{ //cutoff known, use it
      //if true: deviation is small, converged
      if(fabs(locLargestNumEventsDeviation/locAverageNumEventsVector[locSearchCounter]) <= locDeviationPercentageCutOff)
        break;
    }

    //didn't break: reject the largest deviation in num events
    locNumEventsVectorSubset.erase(locNumEventsVectorSubset.begin() + locLargestNumEventsDeviationIndex);
    locEventFrequencyVectorSubset.erase(locEventFrequencyVectorSubset.begin() + locLargestNumEventsDeviationIndex);
    locLiveTimeVectorSubset.erase(locLiveTimeVectorSubset.begin() + locLargestNumEventsDeviationIndex);

    //determine the index of the ORIGINAL vector (not the subset vector) that just got tossed
    locTossedEntries.push_back(locOrigEntryLine[locLargestNumEventsDeviationIndex]);
    locOrigEntryLine.erase(locOrigEntryLine.begin() + locLargestNumEventsDeviationIndex);
    if(tfDebugFlag == 1)
      cout << "tossed orig index, size = " << locTossedEntries[locTossedEntries.size() - 1] << ", " << locTossedEntries.size() << endl;

    locSearchCounter++;
  }while(locNumEventsVectorSubset.size() > 1);
}

void Make_NewTripFiles_List(const string& locAllTripFileList){
  string locTripFileName;

  ifstream locStream1(locAllTripFileList.c_str());
  int locTotalNumRegionsLost = 0, locTotalNumRegionsGained = 0, locTotalNumBadRegionsUnchanged = 0, locTotalNumGoodRegionsUnchanged = 0;
  while(!locStream1.eof()){
    //open trip file
    locStream1 >> locTripFileName;
    if(tfDebugFlag == 1)
      cout << "tripname = " << locTripFileName << endl;

    Make_NewTripFiles_File(locTripFileName, locTotalNumRegionsLost, locTotalNumRegionsGained, locTotalNumBadRegionsUnchanged, locTotalNumGoodRegionsUnchanged);
  }
  double locTotalNumRegions = locTotalNumRegionsLost + locTotalNumRegionsGained + locTotalNumBadRegionsUnchanged + locTotalNumGoodRegionsUnchanged;
  cout << "Total # Scalar Regions Lost, % = " << locTotalNumRegionsLost << ", " << 100.0*(double)locTotalNumRegionsLost/locTotalNumRegions << "%" << endl;
  cout << "Total # Scalar Regions Gained, % = " << locTotalNumRegionsGained << ", " << 100.0*(double)locTotalNumRegionsGained/locTotalNumRegions << "%" << endl;
  cout << "Total # Bad Scalar Regions Unchanged, % = " << locTotalNumBadRegionsUnchanged << ", " << 100.0*(double)locTotalNumBadRegionsUnchanged/locTotalNumRegions << "%" << endl;
  cout << "Total # Good Scalar Regions Unchanged, % = " << locTotalNumGoodRegionsUnchanged << ", " << 100.0*(double)locTotalNumGoodRegionsUnchanged/locTotalNumRegions << "%" << endl;
}

void Make_NewTripFiles_File(const string& locTripFileName, int &locTotalNumRegionsLost, int &locTotalNumRegionsGained, int &locTotalNumBadRegionsUnchanged, int& locTotalNumGoodRegionsUnchanged){
  double locInput_Double;
  int locInput_Int;
  int loc_i, loc_j;

  vector<int> locNumEventsVector, locEventFrequencyVector, locTripFlagVector;
  vector<double> locLiveTimeVector;

  ifstream locInputTripStream(locTripFileName.c_str());
  if((!locInputTripStream.is_open()) || (locTripFileName == "")){
    cout << "WARNING: Trip file " << locTripFileName << " doesn't exist or permission is denied." << endl;
    return;
  }

  //compile statistics from file
  locNumEventsVector.resize(0);  locEventFrequencyVector.resize(0);  locLiveTimeVector.resize(0);  locTripFlagVector.resize(0);

  vector<int> locColumn1, locColumn2, locColumn3, locColumn4, locColumn5, locColumn6, locColumn7, locColumn8, locColumn9;
  vector<double> locColumn10, locColumn11, locColumn12;

  locInputTripStream >> locInput_Int; //go off phase so that last one in while loop triggers .eof()
  while(!locInputTripStream.eof()){
    locColumn1.push_back(locInput_Int);

    if(!(locInputTripStream >> locInput_Int))
      break;
    locColumn2.push_back(locInput_Int);

    if(!(locInputTripStream >> locInput_Int))
      break;
    locColumn3.push_back(locInput_Int);

    if(!(locInputTripStream >> locInput_Int))
      break;
    locColumn4.push_back(locInput_Int);

    if(!(locInputTripStream >> locInput_Int))
      break;
    locColumn5.push_back(locInput_Int);

    if(!(locInputTripStream >> locInput_Int))
      break;
    locColumn6.push_back(locInput_Int);

    if(!(locInputTripStream >> locInput_Int))
      break;
    locColumn7.push_back(locInput_Int);

    if(!(locInputTripStream >> locInput_Int))
      break;
    locColumn8.push_back(locInput_Int);

    if(!(locInputTripStream >> locInput_Int))
      break;
    locColumn9.push_back(locInput_Int);

    if(!(locInputTripStream >> locInput_Double))
      break;
    locColumn10.push_back(locInput_Double);

    if(!(locInputTripStream >> locInput_Double))
      break;
    locColumn11.push_back(locInput_Double);

    if(!(locInputTripStream >> locInput_Double))
      break;
    locColumn12.push_back(locInput_Double);

    //fill vectors
    locNumEventsVector.push_back(locColumn6[locColumn6.size() - 1]);
    locEventFrequencyVector.push_back(locColumn8[locColumn6.size() - 1]);
    locLiveTimeVector.push_back(locColumn12[locColumn6.size() - 1]);
    locTripFlagVector.push_back(locColumn3[locColumn6.size() - 1]);
    if(!(locInputTripStream >> locInput_Int)) //triggers .eof()
      break;
  }
  locInputTripStream.close();

  if(locNumEventsVector.size() < 3) //too small to determine, leave it alone
    return;

  //remove the 1st & last lines
  locNumEventsVector.erase(locNumEventsVector.begin());
  locEventFrequencyVector.erase(locEventFrequencyVector.begin());
  locLiveTimeVector.erase(locLiveTimeVector.begin());
  locTripFlagVector.erase(locTripFlagVector.begin());
  locNumEventsVector.pop_back();
  locEventFrequencyVector.pop_back();
  locLiveTimeVector.pop_back();
  locTripFlagVector.pop_back();

  //eval trip files
  vector<int> locTossedEntries;
  Toss_TripRegions(locNumEventsVector, locEventFrequencyVector, locLiveTimeVector, locTripFlagVector, locTossedEntries, 1);

  //compare tosses to orig tosses.  if different, need to make new trip file.  also update trip data vector
  int locLocalMatchFlag, locMatchFlag = 1;
  for(loc_i = 0; loc_i < int(locTripFlagVector.size()); loc_i++){
    locLocalMatchFlag = 0;
    for(loc_j = 0; loc_j < int(locTossedEntries.size()); loc_j++){
      if(locTossedEntries[loc_j] == loc_i){ //tossed this entry as bad
        locLocalMatchFlag = 1;
        if(locTripFlagVector[loc_i] == 0){ //tossed bad but orig good: no match
          locMatchFlag = 0;
          locTotalNumRegionsLost++;
          locColumn3[loc_i + 1] = 1; //+1 because tossed first row
        }else{locTotalNumBadRegionsUnchanged++;}
        break; //matched or not: move on
      }
    }
    if(locLocalMatchFlag == 0){ //this entry not tossed, test if bad
      if(locTripFlagVector[loc_i] != 0){ //kept but orig bad: no match
        locMatchFlag = 0;
        locTotalNumRegionsGained++;
        locColumn3[loc_i + 1] = 0; //+1 because tossed first row
      }else{locTotalNumGoodRegionsUnchanged++;}
    }
  }

  //trip files are different: make new trip file
  if(locMatchFlag == 0){
    string::size_type locSlashIndex = locTripFileName.find_last_of("/", locTripFileName.length() - 1);
    string locNewTripFileName = locTripFileName.substr(locSlashIndex + 1, locTripFileName.length() - locSlashIndex - 1);
    FILE *locNewTripFile = fopen(locNewTripFileName.c_str(), "a");
    cout << "old trip file name fixed = " << locTripFileName << endl;
    for(loc_i = 0; loc_i < int(locColumn3.size()); loc_i++){
      fprintf(locNewTripFile, "%5d %5d %5d %10d %10d %10d %10d %10.0f %10.0f\t%10u %10u\t%f\n", \
        locColumn1[loc_i], locColumn2[loc_i], locColumn3[loc_i], locColumn4[loc_i], \
        locColumn5[loc_i], locColumn6[loc_i], locColumn7[loc_i], (double)locColumn8[loc_i], \
        (double)locColumn9[loc_i], (unsigned int)locColumn10[loc_i], (unsigned int)locColumn11[loc_i], locColumn12[loc_i]);
    }
    fclose(locNewTripFile);
  }
}
