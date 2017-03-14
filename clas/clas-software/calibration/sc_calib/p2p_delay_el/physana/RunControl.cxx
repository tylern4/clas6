#include "RunControl.h"

using namespace std;

int  map_get_float(const char filename[], const char subsystemname[], 
            const char itemname[], int arraylength, float farray[], int atime, 
            int *firsttime);


/* ------------- Constructor ------------------- */


RunControl::RunControl()	// Default Constructor
{
  
  ConfigFileDefined = 0 ;  	// No Config File Defined
  RootFileDefined   = 0 ;    	// No Root File Defined
  LogFileDefined    = 0 ;     	// No Log File Defined
  AccpFileDefined   = 0 ;	// No Acceptance File Defined

  AccpFileName = "accp.acp";    // Default acceptance file name

  RootFileOpened = false;	// No Root File opened
  LogFileOpened  = false;       // No Log File opened
  DataFileOpened = false;       // No data File Opened
  ShowHistogram  = false;       // No histogram plot at end of analysis
  Gamecock       = false;       // Launched from GAMECOCK
  Lastpercent    =    0.;       // Percent events processed so far;

  LogFile = NULL ;
  RootFile =  NULL;

  i_File          = 0;
  N_Event         = 0;
  N_Events_Update = 10000;
  Beam_E = -9999. ;
  B_Field = -999999. ;
  
  iEvent_tot = 0;		// No Event Processed

  FCUP_Live = 0.;  		// Initialize FCup Live at zero

//  Physics = new TPhysProc ;	// Create pointer to physics processes structure
    				
}



/* -------------- Destructor -----------------------*/


RunControl::~RunControl()	// Default Destructor
{
 MakeVector();			// Create vector containing FCUP Live and write
 CloseRootFile();		// Close Root File and do some stuff
 CloseLogFile();		// Close Log File
 T_Clock.Print();		// Print some stats
}

/*-----------------------------------
	Start a run and initialize
------------------------------------*/	

void RunControl::SetDefault () {
  if (! RootFileDefined) {
    RootFileName = "p2p_delay.root"; RootFileDefined = 1; 
  }

  if (! LogFileDefined) {
    LogFileName = "p2p_delay.log"; LogFileDefined = 1; 
  }

  N_Event         = 0;          // unlimited
  N_Events_Update = 10000;
}

void RunControl::Start( int argc, char *argv[] )
{
 string ModuleName("RunControl::StartRun") ;
 ostringstream Message;

 if ( argc < 2 ) {
   PrintUsage(argv[0]);
   exit(1);
 } 

 ReadCommandLine( argc, argv ) ;	// Parse the Command Line

 OpenLogFile();				// Create a log file

 if ( ConfigFileDefined ) 
    ReadConfigFile();			// Read info from Config File
 else {
   Message << "No config file specified";
   SetDefault();
   PrintMessage(ModuleName, Message.str());
 }  
 if (!  DataFileNameVec.size() ) {
   Message << "No input file defined";
   PrintMessage(ModuleName, Message.str());
   exit(1);
 }
 if (Gamecock) {
   mFileCount = new JFileCount (& DataFileNameVec);
 }

 OpenRootFile();			
 InitBOS();
  if (DataFileNameVec.size() >= 0) {
    if ( access( DataFileNameVec[0].c_str(), R_OK ) != EOF ) {
      open_fpack_unit((char*)DataFileNameVec[0].c_str(),"BOSINPUT",1);
      int iFirst = GetFirstEvent();		// Find First Event in a file
      int locRunNumber = GetRunNumber();  
      NumStrips::Instance().Initialize_NumStrips(locRunNumber);
      close_fpack_unit("BOSINPUT");
    }
  }

 Physics = new TPhysProc ;	// Create pointer to physics processes structure

 T_Clock.Start() ;
}



/* ------ Parse Command Line ---------------------*/

string RunControl::GetParam (int& iarg, int argc, char* argv[]) {
  string ModuleName("RunControl::GetParam") ;
  ostringstream Message;

  if (argv[iarg][2]) {         // option given as -ooption
    return string(&(argv[iarg][2]));
  }

  iarg++;
  if (iarg >= argc) {          // option without parameter
    Message << "Additional parameter needed for option " <<  argv[iarg-1];
    PrintMessage(ModuleName, Message.str()); 
    exit(1);
  }
  
  return string(argv[iarg]);
}

float RunControl::GetFparam (int& iarg, int argc, char* argv[]) {
#ifdef __OLD_GCC_VERSION__
  istrstream ss(GetParam (iarg, argc, argv));
#else
  istringstream ss(GetParam (iarg, argc, argv));
#endif
  float ff;
  ss >> ff;
  return ff;
}

void RunControl::ReadCommandLine( int argc, char*argv []) {
  string ModuleName("RunControl::ReadCommandLine") ;
  ostringstream Message;
  
  int iarg = 1;                               // start behind the program name
  while ( iarg < argc ) {
    
    if ( argv[iarg][0] == '-') {              // Option   
      
      switch (argv[iarg][1]) {
      case 'B':				      // Explicit B Field setting
	B_Field = GetFparam(iarg, argc, argv);// could be overwritten by
	break;				      // the info from Map	
       
      case 'E':				      // Explicit energy setting
	Beam_E = GetFparam(iarg, argc, argv); // could be overwritten by
	break;                                // the info from Map	
	
      case 'a':                               // Set acceptance file 
	AccpFileName = GetParam(iarg, argc, argv);
	AccpFileDefined = 1 ;							
	break;
	// the info from Map       
      case 'c':                               // Set config file 
	ConfigFileName = GetParam(iarg, argc, argv);
	ConfigFileDefined = 1 ;							
	break;
	
      case 'h':
	PrintUsage(argv[0]);
	exit(1);
       break;
       
      case 'l':                               // Log file
	LogFileName = GetParam(iarg, argc, argv);
	LogFileDefined = 1 ;
	break;  
	
      case 'G':
	Gamecock = true;
	break;

      case 's':
	ShowHistogram = true;
	break;

      case 'r':                               // Root output file
	RootFileName = GetParam(iarg, argc, argv);
	RootFileDefined = 1 ;
	break;
	
      default:
	Message << "Bad Option " << argv[iarg];
	PrintMessage(ModuleName, Message.str());
	PrintUsage(argv[0]);    
	exit(1);
	break;
      }                                      // end switch
    }                                        // end option
    else {
      DataFileNameVec.push_back(argv[iarg]);
    }
    iarg++;
  }                                          // end loop
}



/* --------------- Read Configuration File ------------------*/


int RunControl::ReadConfigFile() {
  FILE *ConfigFile ;
  
  string ModuleName("RunControl::ReadConfigFile") ;
  ostringstream Message;
  
  if ( access(ConfigFileName.c_str(), R_OK) == EOF ) {
    Message << "Error Opening Config File " << ConfigFileName 
	    << ", Permission Denied";
    PrintMessage( ModuleName, Message.str());
    exit(1); 
  } 
  
  Message << "Opening Config File " << ConfigFileName;
  PrintMessage( ModuleName, Message.str());
  
  ConfigFile = fopen(ConfigFileName.c_str(), "r");
  
  if ( ConfigFile == NULL ) {
    Message << "Error Opening Config File " << ConfigFileName;
    PrintMessage( ModuleName, Message.str());
    exit(1);
  }
  
  int iline = 0;
  char ConfigLine[255] ;
  while( fgets(ConfigLine, sizeof(ConfigLine)-1, ConfigFile) != NULL) {                        
    string command, var, equalsign, value ;
    iline++;
    istringstream cfstream(ConfigLine);
    cfstream >> command;

    if (cfstream.good() && command == "set") {
      cfstream >> var >> equalsign;
      if (!cfstream.good() || equalsign != "=") {
	Message << "Error in file " << ConfigFileName << " line " << iline << ":"
	  "\n should be: set <variable> = <value>";
	PrintMessage(ModuleName, Message.str());
	exit(1);    
      } 

      if (var == "events") {
	N_Event = -1 ;
	cfstream >> N_Event ; 
	if ( N_Event < 0 ) {
	  Message << "Error in file " << ConfigFileName << " line " << iline << ":"
	    "\n not a valid integer number";
	  PrintMessage( ModuleName, Message.str());
	  exit(1);    
	}
      }
   
      else if (var == "update") {
	N_Events_Update = -1;
	cfstream >> N_Events_Update;
	if ( N_Events_Update < 0 ) {
	  Message << "Error in file " << ConfigFileName << " line " << iline << ":"
	    "\n not a valid integer number";
	  PrintMessage( ModuleName, Message.str());
	  exit(1);
	}    
      } 

      else if (var == "inputfile") {
	cfstream >> value;
	DataFileNameVec.push_back(value);
      }

      else if (var == "rootfile") {
	cfstream >> RootFileName;
	RootFileDefined = 1;
      }      
    }
  }
  fclose(ConfigFile);
}


/* ---------------- Create a Log File ------------------ */


void RunControl::OpenLogFile() {
  string ModuleName("RunControl::OpenLogFile") ;
  ostringstream Message;
  
  FILE *SrvFile ;
  char *UserDir ;
  char UserFile[128] ;
  
  UserDir = getenv("HOME");	// Get value of UNIX env. variable HOME 
  sprintf(UserFile, "%s/.phylog", UserDir );
  Message << "Opening Log Index File " << UserFile;
  PrintMessage( ModuleName, Message.str());
 
  if( access( UserFile, R_OK ) == EOF) {
 
/* Try to create new Log Number from ~/.phylog if it doesnt exist */

    Message << "File " << UserFile << " doesnt exist, ereating a new one";
    PrintMessage( ModuleName, Message.str());
    if ( ( SrvFile = fopen( UserFile, "w") )  == NULL )  {
      Message << "Couldn't open file " <<  UserFile ;
      PrintMessage( ModuleName, Message.str());
      exit(1);
    }
    LogNumber = 2 ;
    fprintf( SrvFile, "%d\n", LogNumber);
    fclose(SrvFile);
  } 
 
  else {
 
/* Read Log Number from ~/.phylog if it does exist */
    
    Message << "File " << UserFile << "does exist, opening";
    PrintMessage( ModuleName, Message.str());  
    SrvFile = fopen( UserFile, "r" );
    fscanf( SrvFile, "%d", &LogNumber);
    fclose(SrvFile);
    SrvFile = fopen( UserFile,"w" );
    fprintf( SrvFile, "%d\n", ++LogNumber );
    fclose( SrvFile );
 }

/*************************** 
   If LogFile is not Defined from Command line it s name is 
       PhyanaXXX.log 
****************************/
       
 if ( !LogFileDefined ) {
   char cnumber [80];
   LogFileName = "Phyana";
   sprintf (cnumber, "%d", LogNumber);
   LogFileName += cnumber;
   LogFileName += ".log";
 }
  
 LogFile = fopen(LogFileName.c_str(),"w");
 if ( LogFile == NULL )  { 
   Message << "Cant Open Log file " << LogFileName;
   PrintMessage( ModuleName, Message.str());
 }
 else {
   Message << "Opened Log File " <<  LogFileName;
   PrintMessage( ModuleName, Message.str());  
   LogFileOpened = true ;			// Keep track of opened Log File
 }
}


/* ---------- Close Log File ------------------*/


void RunControl::CloseLogFile()
{
 string ModuleName("RunControl::CloseLogFile") ;
 ostringstream Message;

 if ( LogFileOpened ) {
   fclose( LogFile );
   LogFileOpened = false;
 }
 else {
   Message << "No log file is open";
   PrintMessage( ModuleName, Message.str());  
 }      
}

void RunControl::End() {
  delete Physics;                      
}

void RunControl::OpenRootFile() {
  string ModuleName("RunControl::OpenRootFile") ;
  ostringstream Message;
  
  RootFile = new TFile( RootFileName.c_str(), "RECREATE", "Physics Analysis", 5 ) ;
  if (RootFile->IsZombie()) {
    Message << "Can't open ROOT file " << RootFileName << " for output";
    PrintMessage(ModuleName, Message.str());
    exit (1);
  }
  RootFileOpened = true ;
  Message << "Opened Root File " << RootFileName;
  PrintMessage( ModuleName, Message.str());
}

void RunControl::CloseRootFile()
{
  string ModuleName("RunControl::CloseRootFile") ;
  ostringstream Message;

  if ( RootFileOpened ) {
    Message << "Closing Root File " << RootFileName;
    PrintMessage(ModuleName, Message.str());
    RootFile->Write();
    //  RootFile->ls(); 
    RootFile->Close();
    RootFileOpened = false;
  }
  else {
    Message << "No Root File Is Open ";
    PrintMessage( ModuleName, Message.str());  
  } 
}


void RunControl::InitBOS()
{
 string ModuleName("RunControl::InitBOS") ;
 ostringstream Message;
 
 initbos();
 Message << "BOS initialized";
 PrintMessage( ModuleName, Message.str()); 
}



int RunControl::OpenDataFile( int iFile ) {
  string ModuleName("RunControl::OpenDataFile") ;
  ostringstream Message;
  
  int iFirst;
  
  if ( iFile >= DataFileNameVec.size() ) {
    Message << "Wrong file number " << iFile ;
    PrintMessage( ModuleName, Message.str());
    return -1 ; 
  }

  Message << "Processing File " << DataFileNameVec[iFile];
  PrintMessage( ModuleName, Message.str() );
  
  if ( access( DataFileNameVec[iFile].c_str(), R_OK ) != EOF ) {
    open_fpack_unit((char*)DataFileNameVec[iFile].c_str(),"BOSINPUT",1);
    iFirst = GetFirstEvent();		// Find First Event in a file
  }
  else {
    Message << "Can't access file " << DataFileNameVec[iFile];
    PrintMessage( ModuleName, Message.str()); 
    return -1;
  }
  
  InitBeamEnergy();	// Read Beam energy from Map
  InitBField();		// Read BField from Map
  int locRunNumber = GetRunNumber();
  initCL01(locRunNumber);
  
  if ( iFirst >=  0 ) {
    DataFileOpened = true;
    i_File = iFile;
    return 0;
  }
  else {
    close_fpack_unit("BOSINPUT");   
    return -1; 
  }
}


void RunControl::CloseDataFile() {
 string ModuleName("RunControl::CloseDataFile") ;
 ostringstream Message;

 if  ( DataFileOpened ) {
  close_fpack_unit("BOSINPUT"); 
  DataFileOpened = false;
 }
 else {
   Message << "No Data File Is Open" ;
   PrintMessage( ModuleName, Message.str());  
 } 
}



/* ---- Find the first Event in the file containing FCUP info ---*/

int RunControl::GetFirstEvent() {
  string ModuleName ("RunControl::GetFirstEvent") ;
  ostringstream Message;
  
  clasHEAD_t *Header = NULL ;
  clasHEVT_t *Hevt = NULL ;
  int No_FCup = 1;
  int iskipped = 0 ;
  
  while ( ( Header == NULL ) || ( No_FCup ) ) {
    dropAllBanks(&bcs_,"HEADEVNTHEVTDCPBECPBCCPBSCPBTBIDPARTCL01CALL");
    cleanBanks(&bcs_);   
    if ( getBOS(&bcs_,1,"E") != 0 ) {
      Header = (clasHEAD_t *) getBank(&bcs_,"HEAD");
      Hevt   = (clasHEVT_t *) getBank(&bcs_,"HEVT");
      if (  ( Header != NULL ) && ( Hevt != NULL ) && 
	    ( ( Hevt->hevt->fcg >= 0. ) || 
	      ( Header->head->nrun == MC_RUN_NUMBER ) ) ) 
	{
	  No_FCup = 0 ;
	  iskipped++ ; 
	} 
      else  { 
	No_FCup = 1 ;
	iskipped++ ; 
      }
    }
    else {
      Message << "Couldnt Find First Event";
      PrintMessage(ModuleName, Message.str());       
      return -1;
    }
  }
  Message << "Skipping first " << iskipped << " events";
  PrintMessage(ModuleName, Message.str());       

  FCUP_Live_st = Hevt->hevt->fcg ;
  FCUP_Live_old_file = FCUP_Live ;
  RunNumber = Header->head->nrun;
  
  Message << "Run Number is " << RunNumber;
  PrintMessage(ModuleName, Message.str());       
  
  Message << "\n\t\tOld FCup " << FCUP_Live_old_file << 
    "\t New file FCUP starts at " << Hevt->hevt->fcg;
  PrintMessage(ModuleName, Message.str());
  
  return iskipped;
}


/* ---------- Process Event --------------- */


int RunControl::ProcessEvent() {
  string ModuleName("RunControl::ProcessEvent") ;
  ostringstream Message;
  
  physEVNT_c* EVENT ;
  
 // World.CheckRequest();
 
  dropAllBanks(&bcs_,"HEADEVNTHEVTDCPBECPBCCPBSCPBTBIDPARTCL01CALL");
  cleanBanks(&bcs_);   

  if ( getBOS(&bcs_,1,"E") != 0 ) {
    
    if ( (  ( EVENT = new physEVNT_c ) != 0 ) && 
	 ( EVENT->IsOK() || RunIsMC() )  ) {
      RootFile->cd() ;
      make_CL01_bank();
      Physics->Process( EVENT );
      if ( !RunIsMC() )  		// If not Monte Carlo, then 
	AddFCUP();		// Keep track of FCup Live
    } 
    delete EVENT ; 
   
    if( N_Events_Update && ++iEvent_tot % N_Events_Update == 0 ) { // Message about # events 
      T_Clock.Stop();
      Message << "run #" << RunNumber << ", ev.proc. ";
      Message.width(8);
      Message << iEvent_tot << ",";
      Message.width(7); Message.precision(3);
      Message << fixed << 1000. * T_Clock.CpuTime() / N_Events_Update 
	      << " msec/ev";
      PrintMessage(ModuleName, Message.str());       

      T_Clock.Start(); 
    }

    if (Gamecock) {
      clasHEAD_t* HEAD = (clasHEAD_t*) getBank(&bcs_, "HEAD"); 
      if (HEAD) {
	double perc = mFileCount->GetPercent(i_File, 
					      HEAD->head[0].nevent);
	if (perc >= Lastpercent + 1.) {
	  cout << "Processed: ";
	  cout.width(8);
	  cout.precision(3);
	  cout << fixed << perc << " %" << endl;
	  Lastpercent = perc;
	}
      }
    }

    return 0 ;
  } 
  else return -1; 
}

void RunControl::MakeVector() {
  /*
  TVector Vect( 1, 1, FCUP_Live, "END") ;
  Vect.Write("RunControl");
  */
}

float RunControl::InitBeamEnergy() {
  string ModuleName("RunControl::InitBeamEnergy") ;
  ostringstream Message;
  
  char   *ParmsDir, MapName[128];
  int iFirst;
  
  if ( !RunIsMC() ) { 
    
    ParmsDir = getenv("CLAS_PARMS");
    //  Beam_E = 0;
    sprintf(MapName,"%s/Maps/RUN_CONTROL.map",ParmsDir);
    
    map_get_float(MapName, "beam","energy", 1, &Beam_E, RunNumber, &iFirst);
    Beam_E /= 1000. ;		// Convert MeV to GeV
    
  }	
  if ( Beam_E > 0 ) {
    Message << "Beam Energy initialized " << Beam_E << " (GeV)";
    PrintMessage(ModuleName, Message.str()); 
  }		
  else {
    Message << "Error initializing Beam Energy...";
    PrintMessage(ModuleName, Message.str()); 
    exit(1);
  }
 
  return Beam_E;            
}



float RunControl::InitBField() {
 string ModuleName("RunControl::InitBField") ;
 ostringstream Message;

 char   *ParmsDir, MapName[128];
 int iFirst;

 if ( !RunIsMC() ) 
 {
  ParmsDir = getenv("CLAS_PARMS");
  // B_Field = 0 ;
  sprintf(MapName,"%s/Maps/RUN_CONTROL.map",ParmsDir);
	
  map_get_float(MapName, "currents","torus", 1, &B_Field, RunNumber, &iFirst);   	
 }
 	
 if ( B_Field > -99999. )  {
   Message << "Torus Field initialized " << B_Field << " (Amps)";
   PrintMessage(ModuleName, Message.str()); 
 }		
 else {
   Message << "Error initializing Torus Field...";
   PrintMessage(ModuleName, Message.str()); 
   exit(1);
 }

 return B_Field ;
}



void RunControl::AddFCUP() {
  string ModuleName("RunControl::AddFCUP");
  ostringstream Message;
  
// if ( RunIsMC() ) return ;
  
  clasHEVT_t* Hevt = NULL;
  
  Hevt = (clasHEVT_t *) getBank(&bcs_,"HEVT");
  if ( Hevt != NULL ) 
    FCUP_Live = Hevt->hevt->fcg - FCUP_Live_st + FCUP_Live_old_file;
  else {
    Message << "Couldnt find HEVT for FCUP";
    PrintMessage(ModuleName, Message.str() );  
  }
}

void RunControl::PrintMessage( const string ModuleName, const string Message ) {
// cout << "From " << ModuleName << "--> " << Message ;
  fprintf(stdout, "[%.27s] %s\n",ModuleName.c_str(), Message.c_str() );
  fflush(stdout);
  if ( LogFileOpened ) {
    fprintf( LogFile,"[%.27s] %s\n",ModuleName.c_str(), Message.c_str() );
    fflush( LogFile );
  }
}


void RunControl::PrintUsage(string ProgName) {
  cout << "\nUsage:\n\n   " << ProgName << 
    " [Options] [Datafiles] \n\n"
    "Purpose:\n\n   "
    "paddle to paddle time-of-flight alignment for electron runs\n\n"
    "Options:\n\n"
    "   -c<file-inp>     Configuration file - sample in cvs directory\n"
    "   -B<#.#>          Torus field [A] \n"
    "   -E<#.#>          Beam energy [GeV]\n"
    "   -l<file-outp>    Log file for all messages\n"
    "   -r<file-outp>    ROOT file to save histograms and ntuple\n"
    "   -s -show         Show quality of calibration before terminating\n"
    "   -G               Batch mode, program launched from GAMECOCK\n" << endl;
  PrintComp();
}


void RunControl::PrintComp() {
  cout << "This version of code was compiled:" << endl;
  cout << "Date: " << __DATE__ << endl;
  cout << "Time: " << __TIME__ << endl;
}
