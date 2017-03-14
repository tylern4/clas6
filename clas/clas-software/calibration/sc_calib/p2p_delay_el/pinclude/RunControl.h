/***********************************************************

This is a class to run a phyana session.
Just one object of this class is expected

************************************************************/

#ifndef __RUN_CONTROL_H__
#define __RUN_CONTROL_H__

#ifdef __OLD_GCC_VERSION__
#include <iostream.h>
#include <vector.h>
#include <strstream.h>
#else
#include <iostream>
#include <sstream>
#include <vector>
#endif

#include <string>
#include "c_stds.h"
#include "phys_consts.h"
#include "bos_pod.h"
#include "RootF.h"
#include "BatchService.h"
#include "TPhysProc.h"
#include "JEventCount.h"
#include "call.h"

#include "NumStrips.h"
#include "TShowCalib.h"

class TPhysProc ;


class 	RunControl  { 
protected:
  int 	ConfigFileDefined;
  int 	RootFileDefined ;
  int 	LogFileDefined ;
  int	AccpFileDefined ;
  
  bool 	RootFileOpened ;
  bool 	LogFileOpened ;
  bool 	DataFileOpened;
  bool  ShowHistogram;
  bool  Gamecock;

  std::string         ConfigFileName;
  std::string         RootFileName;
  std::string         LogFileName;
  std::vector<std::string> DataFileNameVec;
  std::string         AccpFileName;
  
  int 	N_Event ;
  int 	iEvent_tot;
  int   i_File;
  int	N_Events_Update ;
  
  int 	RunNumber ;
  int	LogNumber ;
  
  FILE* 	LogFile ;
  TFile* 	RootFile ;
  
  TPhysProc* Physics ;
  
  TStopwatch T_Clock;
  
  float 	FCUP_Live ;
  float  	FCUP_Live_st ;
  float 	FCUP_Live_old_file;
	
  float 	Beam_E ;
  float 	B_Field;
  float         Lastpercent;
  JFileCount*   mFileCount;
		
 private:
 	
  void 	 ReadCommandLine( int argc, char *argv[] );
  std::string GetParam (int& iarg, int argc, char* argv[]);
  float  GetFparam (int& iarg, int argc, char* argv[]);

  void	 MakeVector();
  
  void	 OpenLogFile() ;
  void 	 CloseLogFile() ;
  
  void 	 OpenRootFile() ;
  void 	 CloseRootFile() ;
  
  void	 InitBOS() ;
  
  void	 AddFCUP();
  void   SetDefault();

  int 	 ReadConfigFile();
  int 	 GetFirstEvent();
  
  float	 InitBeamEnergy();
  float 	InitBField();
  
 public:

  
  RunControl();
  ~RunControl();
  
  void 	Start( int argc, char *argv[] );
  int	ProcessEvent();

  
  int 	OpenDataFile( int iFile ) ;
  void	CloseDataFile() ;
  
  int 	GetDataFileNumber()     { return DataFileNameVec.size();       }
  int	GetEventNumber()        { return N_Event;                      }  
  float	GetBeamEnergy()         { return Beam_E;                       }
  bool  GetShowHistogram()      { return ShowHistogram;                }

  float GetBField()             { return B_Field;                      }
  float	GetFCUP()               { return FCUP_Live;                    }
  int 	GetRunNumber()          { return RunNumber;                    }
  int   GetFileIndex()          { return i_File;                       } 

  TFile* GetRootFile()          { return RootFile;                     }
  const char* GetRootFileName() { return RootFileName.c_str();         }
  const char* GetAccpFileName() { return AccpFileName.c_str();         }
  bool 	RunIsMC()               { return (RunNumber == MC_RUN_NUMBER); }
  bool  MaxIsUnlimited()        { return N_Event == 0;                 } 
  bool  IsGamecockMode()        { return Gamecock;                     }
  void 	End();

  
  void	PrintMessage( const std::string ModuleName, const std::string Message );
  void	PrintUsage( std::string ProgName );
  void	PrintComp();	
  
  
//	BatchService World ;
 
} ;


#endif

