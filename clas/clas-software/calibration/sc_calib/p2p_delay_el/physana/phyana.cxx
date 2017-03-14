//******************************************************************************
//
//                    PROGRAM  PHYANA
//                    Physics Analysis Program
//
//   Author           : Gagik  Gavalian
//   Creation date    : 10/04/1998
//   Modified         : Hovanes Egiyan
//   Last Modified    : 08/27/98
//
//******************************************************************************
#ifdef __OLD_GCC_VERSION__
#include <iostream.h>
#else
#include <iostream>
#endif
#include "c_stds.h"
#include "RootF.h"
#include "BatchService.h"
#include "RunControl.h"
#include "TShowCalib.h"

/*=======Function Prototypes======*/

static void SignalInt(int sig);
static void SignalAbrt(int sig);

/*===============================*/

int intrp_flg = 0;	// Variable for exiting if interrupted: intrp_flg=1 --> exit loops

RunControl *PhyAna ;
TRint* theApp ;


main( int argc, char *argv[] )
{
  std::ios::sync_with_stdio() ;	// Make iostream work with stdio 
  
  signal(SIGINT, SignalInt);
  signal(SIGABRT, SignalAbrt);
  //  signal(SIGSEGV, SignalInt);
  //  signal(SIGBUS, SignalInt);
  //    int Error; //left undefined by Motif
    
  extern void  InitGui();  // initializer for GUI needed for interactive interface
  VoidFuncPtr_t initfuncs[] = { InitGui, 0 };
    
  //Initialize the ROOT system
  TROOT analysis("ANALYSIS","PHYSICS ANALYSIS PROGRAM", initfuncs );
  printf("ROOT Framework initialization done..\n" );
  theApp = new TRint("Interactive", 0, 0, 0, 0, 0 ); 


  PhyAna = new RunControl;
  signal(SIGINT, SignalInt);
  signal(SIGABRT, SignalAbrt);
    
  PhyAna->Start( argc, argv );

  for (int iFile = 0; iFile < PhyAna->GetDataFileNumber() && intrp_flg != 1 ; 
       iFile++ ) {
    if ( PhyAna->OpenDataFile(iFile) == 0 ) {
      for (int iEvent = 0;
	   (PhyAna->MaxIsUnlimited() ||
	    iEvent < PhyAna->GetEventNumber())  &&  
	     intrp_flg != 1  &&
	     PhyAna->ProcessEvent() == 0   ; iEvent++) ;
      
	PhyAna->CloseDataFile();
    }
    
  }
  

  char RootFileName[256] ;
  strcpy( RootFileName,  PhyAna->GetRootFileName()) ;

  PhyAna->End();
  
  delete PhyAna;
  

  //
  //    Check the quality of the Calibrations 
  // 
  
  if (PhyAna->GetShowHistogram() ) {    
    printf("\nCheck the quality of the calibration, ROOT file <%s>\n", RootFileName);
    printf("Exit the ROOT session when done \n\n");
    TShowCalib* ShowCalib = new TShowCalib( RootFileName ) ;
    theApp->Run();
  }
}





static void SignalInt(int sig){
  
  extern int intrp_flg;
  
  fprintf(stderr, "\nCaught interrupt signal %d: will exit cleanly.\n", sig);
  intrp_flg = 1;
  return;
}

static void SignalAbrt(int sig){
  
  fprintf(stderr, "\nCaught abort signal %d: will try to ignore it.\n", sig);
}


