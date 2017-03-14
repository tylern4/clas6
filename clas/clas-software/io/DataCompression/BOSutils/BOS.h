// Author: Mike Williams 12/17/2003
#ifndef BOS_h
#define BOS_h 

#include <string.h>
#include <stdlib.h>
#include <iostream>
#include <math.h>

// htmldoc flag 1 (flag for perl script to generate html documentation)

// CLAS headers
extern "C" {
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <ec.h>
#include <clas_cern.h>
#include <ctype.h>
#include <kinematics.h>
#include <map_manager.h>
#include <trk.h>
#include <clasmdl.h>
#include <utility.h>
#include <pid.h>
#include <makebanks.h>
#include <call.h>
#include <bosddl.h>
#include <tagtnorm.h>
#include <vertex.h>
#ifndef eloss_h
#define eloss_h
#include <eloss.h>
#endif
}

// htmldoc flag 3 (used for generating html documentation)

using namespace std;
 
int pdg2geant(int pid);

class BOS {

 public:

  // Constructor/Destructor 
  BOS();

  virtual ~BOS();

  // initialize BOS banks (calls initbos() from /packages/c_bos_io/readbos.c)
  inline void InitBOS() {initbos();};

  // define and open a BOS output file
  bool OutFile(const char *bof);

  // define and open BOS input file
  bool InFile(const char *bif);

  // get next event (calls getBOS from /packages/c_bos_io/readbos.c) 
  bool GetBOS(int remakeflag, int PidGroup = 1, int mcflag = 1);

  // close the bos files
  void CloseInFile();
  void CloseOutFile();

  // remake banks with updated calibration data
  void RemakeBanks(int bosrunno, int PidGroup = 1);

  // Make PART, MVRT, and TAGR banks from MCTK and MCVX banks
  void ConvertMCBanks(int PidGroup);

  // Make the MVRT bank only
  void MakeMVRT(int bosrunno,int mcflag=1);

  // Get the required banks
  bool GetBanks(int PidGroup = 1, int PidFlag = 2); 

  // write current BOS event to the BOS output file (calls putBOS)
  inline void Write() {putBOS(&bcs_, 7, "E");};

  // drop banks and clean them prior to getting the next event
  inline void CleanBOS();

  // add banks to the list to be written in a bos output file
  inline void AddMVRT();

  // functions to return pointers to the BOS banks
  inline clasHEAD_t* const HEADbank() const {return HEAD;};
  inline clasPART_t* const PARTbank() const {return PART;};
  inline clasPART_t* PARTbank(int group);
  inline clasTAGR_t* const TAGRbank() const {return TAGR;};
  inline clasMVRT_t* const MVRTbank() const {return MVRT;};
  inline clasTBID_t* const TBIDbank() const {return TBID;};
  inline clasTBID_t* TBIDbank(int group);
  inline clasTBER_t* const TBERbank() const {return TBER;};
  inline clasSCPB_t* const SCPBbank() const {return SCPB;};
  inline clasHEVT_t* const HEVTbank() const {return HEVT;};
  inline clasEVNT_t* const EVNTbank() const {return EVNT;};
  inline clasDCPB_t* const DCPBbank() const {return DCPB;};
  inline clasECPB_t* ECPBbank();
  inline clasTGPB_t* const TGPBbank() const {return TGPB;};
  inline clasSTPB_t* const STPBbank() const {return STPB;};
  inline clasCL01_t* const CL01bank() const {return CL01;};
  inline clasSCRC_t* SCRCbank(int bsec);
  inline clasMCTK_t* const MCTKbank() const {return MCTK;};
  inline clasMCVX_t* const MCVXbank() const {return MCVX;};
  inline clasECHB_t* ECHBbank(int sec);
  inline clasSTN0_t* STN0bank();

 private:

  clasHEAD_t *HEAD;    // Pointer to the HEAD bank
  clasPART_t *PART;    // Pointer to the PART bank 
  clasTAGR_t *TAGR;    // Pointer to the TAGR bank
  clasMVRT_t *MVRT;    // Pointer to the MVRT bank
  clasTBID_t *TBID;    // Pointer to the TBID bank 
  clasSCRC_t *SCRC;    // Pointer to the SCRC bank 
  clasTBER_t *TBER;    // Pointer to the TBER bank 
  clasSCPB_t *SCPB;    // Pointer to the SCPB bank
  clasEVNT_t *EVNT;    // Pointer to the EVNT bank
  clasDCPB_t *DCPB;    // Pointer to the DCPB bank
  clasECPB_t *ECPB;    // Pointer to the ECPB bank
  clasTDPL_t* TDPL;    // Pointer to the TDPL bank
  clasTGPB_t* TGPB;    // Pointer to the TGPB bank
  clasSTPB_t* STPB;    // Pointer to the STPB bank
  clasCL01_t* CL01;    // Pointer to the CL01 bank
  clasHEVT_t* HEVT;    // Pointer to the HEVT bank
  clasECHB_t* ECHB;    // Pointer to the ECHB bank
  clasSTN0_t* STN0;    // Pointer to the STN0 bank

  // Dave's monte carlo banks
  clasMCTK_t *MCTK;    // Pointer to the MCTK bank
  clasMCVX_t *MCVX;    // Pointer to the MCVX bank

  int _RunNum;       // Run Number of this BOS event

  // htmldoc flag 2 (flag for perl script to generate html documentation)

};
//_____________________________________________________________________________

// Inline member functions

inline void BOS::CleanBOS() {
  // Drop the banks to get ready for the next event
  dropAllBanks(&bcs_,"E");
  cleanBanks(&bcs_);
}

inline void BOS::AddMVRT() {
  // Add MVRT to the list of output banks
  formatBank("MVRT","2I,10F,I");
  bankList(&bcs_,"E+","MVRT");
}

inline clasSCRC_t* BOS::SCRCbank(int sec) {
  // Get a pointer to SCRC for sector sec
  SCRC = (clasSCRC_t*)getGroup(&bcs_,"SCRC",sec);
  return SCRC;
}

inline clasPART_t* BOS::PARTbank(int group){
  // Get a pointer to the PART bank fro group
  PART = (clasPART_t*)getGroup(&bcs_,"PART",group);
  return PART;
}

inline clasTBID_t* BOS::TBIDbank(int group){
  // Get a pointer to the TBID bank fro group
  TBID = (clasTBID_t*)getGroup(&bcs_,"TBID",group);
  return TBID;
}

inline clasECPB_t* BOS::ECPBbank(){
  // Get a pointer to the ECPB bank
  ECPB = (clasECPB_t*)getBank(&bcs_,"ECPB");
  return ECPB;
}

inline clasECHB_t* BOS::ECHBbank(int group){
  // Get a pointer to the ECHB bank fro group
  ECHB = (clasECHB_t*)getGroup(&bcs_,"ECHB",group);
  return ECHB;
}

inline clasSTN0_t* BOS::STN0bank(){
  // Get a pointer to the STN0 bank from group
  STN0 = (clasSTN0_t*)getBank(&bcs_,"STN0");
  return STN0;
}

#endif
