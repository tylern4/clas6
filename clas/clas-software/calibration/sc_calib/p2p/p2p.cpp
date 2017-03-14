//Paddle2Paddle Photon Calibration for Gamecock (Command Line Version)
//Arthur Sabintsev (arthur@jlab.org)

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include <sys/types.h>
#include <TPhotTiming.h>
using namespace std;

//Declare WriteRootDst Variables
string WRD_path; //WriteRootDst Output File Director (User Input)
const char * WRDc; //WriteRootDst Output File Director (Pointer from WRD_path)
string BOS_path; //Directory of BOS files that need to be recooked (User Input)
const char * BOSc; //Directory of BOS files that need to be recooked (Pointer from BOS_path)
string BOS_files; //The BOS files that need to be recooked (User Input)
const char * BOSf; //The BOS files that need to be recooked (Pointer from BOS_files)
char WriteRootDst[] = "WriteRootDst -o ";
char exec_combo[256]; //Used to combine location of BOS Directory and Files for use in an executable command
string TPhotT;
const char * TPhotTc;
char symbolic[] = "ln -s ";
char symbolic_combo[256]; //Used to create a Symbolic Link (TPhotTiming directory  will link to dst_output in WRD_path)

void WRD_exec() //Run WriteRootDST
{
  cout << "Please enter the name of each file you would like to recook (please separate each file with spaces): ";
  getline(cin, BOS_files);
  BOSf = BOS_files.c_str();
  chdir(BOSc); //Change to BOS files directory
  cout << "WriteRootDST will now create a DST file, name \"dst_output\" in \"" << WRD_path <<"\". This may take some time..." << endl;
  strcpy(exec_combo, WriteRootDst);
  strcat(exec_combo, WRDc);
  strcat(exec_combo, "dst_output ");
  strcat(exec_combo, BOSf);
  system(exec_combo); //This line should output in xterm as follows: WriteRootDst -o /path/to/save/dir/dst_output /BOS/files
}


void WRD_dir() //Prompt user to choose directory in which they wish to save the WriteRootDst Output File
{
  cout << "Where would you like to save the WriteRootDst Output file? (Please start & end with a /): ";
  getline(cin, WRD_path);
  WRDc = WRD_path.c_str();

  if (chdir(WRDc) == 0)
    {
      chdir(WRDc); 
      cout << "The WriteRootDst output file will be saved in the following directory: " << WRD_path << endl;
    }
  
  else if (chdir(WRDc) == -1)
    {
      cout << "The directory you specified, \"" << WRD_path << "\" does not exist. Please try again" << endl;
      WRD_dir();
    }


}

void BOS_dir() //Prompt user to choose directory where cooked files are located
{
  cout << "Enter the full path for the BOS files (start & end with /) if it's not \"/home/clasg9/cooklinks/pass0/v1/BOS/\". Otherwise, press enter/return: ";
  getline(cin, BOS_path);

      BOSc = BOS_path.c_str();    
      if (chdir(BOSc) == 0)
	{
	  WRD_exec();
	}
      
      else if (chdir(BOSc) == -1)
	{
	  cout << "The directory you specified, \"" << BOS_path << "\" does not exist. Please try again" << endl;
	  BOS_dir();
	}

  
}


void Symbolic_Link() // Create Symbolic Link for dst_output to the TPhotTiming directory
{
  cout << "Please enter the location of the TPhotTiming directory: ";
  getline(cin, TPhotT);
  TPhotTc = TPhotT.c_str();
  if (chdir(TPhotTc) == 0)
    {
      chdir(TPhotTc); 
      strcpy(symbolic_combo, symbolic);
      strcpy(symbolic_combo, WRDc);
      strcat(symbolic_combo, "dst_output .");
      system(symbolic_combo);
    }
  
  else if (chdir(TPhotTc) == -1)
    {
      cout << "The directory you specified, \"" << TPhotT << "\" does not exist. Please try again" << endl;
      Symbolic_Link();
    }

}

void loadLibs() //Import TPhotTiming Libraries
{
Int_t loadSharedLibs() 
  {
    Int_t status = 0;
    cout << "Loading Libraries" << endl;
    TString LibList( gSystem->GetLibraries() );
    
    if ( ! LibList.Contains("libTree.so") )
      status |= gSystem->Load("libTree.so") ;
    if ( ! LibList.Contains("libmysqlclient.so") )
      status |= gSystem->Load("libmysqlclient.so") ;
    if ( ! LibList.Contains("Physics.so") )
      status |= gSystem->Load("libPhysics.so") ;
    if ( ! LibList.Contains("EG.so") )
      status |= gSystem->Load("libEG.so") ;  
    if ( ! LibList.Contains("ClasTool") )
      status |= gSystem->Load("libClasTool.so");
    if ( ! LibList.Contains("libMapUtils.so") )
      status |= gSystem->Load("libMapUtils.so");
    if ( ! LibList.Contains("libPhotTiming.so" ) ) //I think only this library is necessary. The rest can probably be commented out.
      status |= gSystem->Load("libPhotTiming.so");
    return status;
  }
}




//Where all of the action happens
int main()
{
  
 //Step 1: Prompt user to choose directory in which they wish to save the WriteRootDst Output File
  WRD_dir();

  //Step 2: Prompt user to choose directory where cooked files are located.
  //Upon successful completition, of this step, WriteRootDst will execute.
  //This function might needs to be re-written as of the middle of July, due to the transfer of the v0/v1 BOS files to the tape archives. 
  //Please contact Eugene Pasyuk for more information on how to do this (This step involves jcache /mss/clas/g9a/production/pass0/v1/BOS/)
  BOS_dir();

  //Step 3: Create Symbolic Link to TPhotTiming Directory
  Symbolic_Link();
  
  //Step 4: Load Necessary Libraries
  loadLibs();
  
  
  //Step 5: Run the Root Commands (Will produce tof_timing.dat file, but will not produce histograms)
  //Functions are taken from TPhotTiming.h
  string runNo;
  int N;
  char root_combo[256];
  
  
  cout << "Please enter the RUN NUMBER that you would like to calibrate (without the leading zero): ";
  getline(cin, runNo);

  cout << "Please enter the number of events you would like to process: ";
  cin >> N;

  TPhotTiming X("dst_output", "root_output", "calib_user.RunIndexg9", runNo);
  X.Process(N);
  X.Fit();

  strcpy(root_combo, "tof_timing_");
  strcat(root_combo, runNo);
  strcat(root_combo, ".dat");

  X.WriteConstants(root_combo); //Output constants file: tof_timing_runNo.dat
  system("rm tag*"); //Removes tagger_timing.dat file that is created by default when running WriteConstants()
  
  //Step 6: Reprocess/Parse tof_timing.dat (Get Delay4Map column, trash the rest, and upload to calDB);
  ifstream infile;
  ofstream outfile;
  string headerReader[7], delayMap[342];
  
  infile.open(root_combo, ios::in);
  outfile.open("p2p_constants.dat", ios::out);
  
  infile >> headerReader[0] >> headerReader[1] >>headerReader[2] >>headerReader[3] >>headerReader[4] >>headerReader[5] >> headerReader[6];
  for (int i = 0; i <= 341; i++)
    {
      infile >> delayMap[i] >> delayMap[i] >> delayMap[i] >> delayMap[i] >> delayMap[i] >> delayMap[i] >> delayMap[i];
      outfile << delayMap[i];
      if (i < 341) outfile << endl;
    }
  
  infile.close();
  outfile.close();

  //Step 7: upload the constants - This part might need to be done manually 
  //caldb_write_and_link.pl s=SC_CALIBRATIONS_V2 ss=delta_T i=paddle2paddle min=runNo max=65000 ci="new p2p for run runNo" cc="new p2p for run runNo" srminrunNo srmax=runNo f=p2p_constants.dat it=calib_user.RunIndexg9

  return 0;  
}
