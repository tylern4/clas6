// Author: Mike Williams (12/17/2003)

///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// This class handles BOS file operations.  Its data members are pointers to //
// the BOS banks needed to process an event.  It has functions for opening a //
// BOS input file and BOS output file.  It also has functions for remaking   //
// BOS banks with the correct calibration data and can call the              //
// functions from readbos.c needed to process an event.                      //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

#include "BOS.h"
#include <iostream>

// htmldoc flag 1 (flag for perl script to generate html documentation)

BOS::BOS(){
  // Constructor (also calls initbos())
  
  HEAD = NULL;
  PART = NULL;
  TAGR = NULL;
  MVRT = NULL; 
  TBID = NULL; 
  SCRC = NULL; 
  TBER = NULL; 
  MCTK = NULL; 
  MCVX = NULL;
  EVNT = NULL;
  TGPB = NULL;
  DCPB = NULL;
  SCPB = NULL;
  ECPB = NULL;
  STPB = NULL;
  CL01 = NULL;
  HEVT = NULL;

  _RunNum = 0;

  this->InitBOS();
}
//_____________________________________________________________________________

BOS::~BOS() {
  // Destructor
}
//_____________________________________________________________________________

bool BOS::OutFile(const char *bof) {
  // Creates and opens a  BOS file for output
  char bofmess[100];
  unlink(bof);
  sprintf(bofmess, "OPEN BOSOUTPUT UNIT=7 FILE=\"%s\" WRITE STATUS=NEW RECL=3600", bof);
  if(!fparm_c(bofmess)) {    
    return false;    
  }
  else{  
    return true;    
  }
}
//_____________________________________________________________________________

bool BOS::InFile(const char *bif) {
  // Opens a BOS file for input
  char bifmess[100];
  sprintf(bifmess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", bif);
  
  if(!fparm_c(bifmess)) {    
    return false;    
  }
  else{  
    return true;   
  }
}
//_____________________________________________________________________________

void BOS::CloseInFile() {
  // Close  BOS input file
  char bifmess[100];
  sprintf(bifmess,"CLOSE BOSINPUT UNIT=1");  
  fparm_c(bifmess);  
  return; 
}
//_____________________________________________________________________________

void BOS::CloseOutFile() {
  // Close  BOS output file
  char bifmess[100];
  sprintf(bifmess,"CLOSE BOSINPUT UNIT=7");  
  fparm_c(bifmess);  
  return; 
}
//_____________________________________________________________________________

bool BOS::GetBOS(int remake, int PidGroup, int mcflag) {
  // Gets the next event from the BOS file.  
  // remake = 1  -> remake the BID and PART banks if the argument 
  // remake = 2  -> remake the MVRT bank only (calls BOS::MakeMVRT)
  // PidGroup = 0  -> calls BOS::ConvertMCBanks
  // mcflag is passed to BOS::MakeMVRT 
  //
  // Note: also initializes the eloss package.
  // Default Values: PidGroup = 1, mcflag = 1

  if(getBOS(&bcs_,1,"E")){
    
    int runnum;

    if((HEAD = (clasHEAD_t *)getBank(&bcs_, "HEAD"))){
      
      runnum = HEAD->head[0].nrun;
      // if(_RunNum == 0) InitEloss(runnum); don't need this right now
      if(runnum != _RunNum) _RunNum = runnum;      
      if(PidGroup==0) this->ConvertMCBanks(PidGroup);
      else{
	if(remake == 1){
	  this->RemakeBanks(runnum,PidGroup);
	}
	if(remake == 2){
	  this->MakeMVRT(runnum,mcflag);
	}
      }
    }
    return true;
  }
  else{    
    return false;
  }
}
//_____________________________________________________________________________

void BOS::RemakeBanks(int runnum, int PidGroup) {
  // Remake the BOS banks with corrected calibration data
  // Calibration data is kept in CLAS_PARMS 
  // Banks are remade using CLAS software found in /packages/pid
    
  ConfigEvent(_RunNum,1);
  if(runnum != _RunNum){
    vertex_brun(_RunNum);
  }
  make_mvrt();
  this->AddMVRT();    
  // REMAKE "BID" BANKS (THIS INCLUDES TAGR BANK)
    
  dropBank(&bcs_, BID_BANKS,PidGroup);
  bankList(&bcs_, "E+", BID_BANKS); 
  make_BID_banks(PidGroup);
    
  dropBank(&bcs_,"PART",PidGroup);
  bankList(&bcs_,"E+","PART");
  make_PART_group(PidGroup) ;   
}
//_____________________________________________________________________________

void BOS::MakeMVRT(int bosrunno,int mcflag){
  // Make the MVRT bank (calls make_mvrt())
  // Note that RemakeBanks calls make_mvrt(), so this function should only 
  // be called if the other banks are NOT being remade.
  // (note that if the MVRT bank doesn't exist, this function must be called
  // prior to calling BOS::GetBanks, or that function will return false).
  //
  // Note: mcflag = 0 for monte carlo and 1 for data

  //  ConfigEvent(_RunNum,mcflag);
  if(bosrunno != _RunNum){
    vertex_brun(_RunNum);
  }
  make_mvrt();
  this->AddMVRT();    
}
//_____________________________________________________________________________

void BOS::ConvertMCBanks(int PidGroup){
  // Create PART and MVRT banks with the group number PidGroup from the monte carlo
  // banks MCTK and MCVX.  If there are no TAGR banks available, create one with information
  // from the last particle listed in the MCTK bank.
  // This routine currently assumes the MC banks were generated by the genova generator he3.
  // The genova generator presently adds the incident photon as the last particle in the 
  // MCTK bank.
  // Also, the MVRT bank is filled with the information from the MCVX bank.  Since the thrown
  // monte carlo do not have tracking information (TBID,TBER,ect.), many entries in the created
  // MVRT bank will be zero.
  
  int npart=0,i,vid=0;

  clasMCTK_t *MCTK = NULL;
  clasMCVX_t *MCVX = NULL;
  clasTAGR_t *TAGR = NULL;
  clasPART_t *PART = NULL;
  clasMVRT_t *MVRT = NULL;

  MCTK = (clasMCTK_t *)getBank(&bcs_,"MCTK");
  MCVX = (clasMCVX_t *)getBank(&bcs_,"MCVX");
  TAGR = (clasTAGR_t *)getBank(&bcs_,"TAGR");
  PART = (clasPART_t *)getGroup(&bcs_,"PART",PidGroup);
  MVRT = (clasMVRT_t *)getGroup(&bcs_, "MVRT",PidGroup);

  if (MCTK && MCVX) {

    npart=MCTK->bank.nrow; // number of particles in MCTK bank

    // make PART from MCTK and MCVX banks with group number PidGroup.
    // If the genova generator he3 is used, the last particle in the bank
    // is the incident photon and can be used to fill the TAGR bank.
    // First check that a PART bank with group PidGroup does not exist
    if(!PART){
      PART=(clasPART_t *)makeBank(&bcs_,"PART",PidGroup,sizeof(part_t)/4,npart-1);
      bankList(&bcs_, "E+", "PART");
      for (i=0;i<(npart-1); i++) {
	PART->part[i].pid = pdg2geant(MCTK->mctk[i].id);
	PART->part[i].p.space.x = MCTK->mctk[i].pmom*MCTK->mctk[i].cx;
	PART->part[i].p.space.y = MCTK->mctk[i].pmom*MCTK->mctk[i].cy;
	PART->part[i].p.space.z = MCTK->mctk[i].pmom*MCTK->mctk[i].cz;
	PART->part[i].p.t = sqrt(MCTK->mctk[i].pmom*MCTK->mctk[i].pmom + MCTK->mctk[i].mass*MCTK->mctk[i].mass);
	PART->part[i].q = MCTK->mctk[i].charge;
	PART->part[i].vert.x = MCVX->mcvx[0].x;
	PART->part[i].vert.y = MCVX->mcvx[0].y;
	PART->part[i].vert.z = MCVX->mcvx[0].z;
      }
    }
    
    // make MVRT from MCVX bank with group number PidGroup
    // First check that a MVRT bank with group PidGroup does not exist
    if(!MVRT){
      MVRT=(clasMVRT_t *)makeBank(&bcs_,"MVRT",PidGroup,sizeof(mvrt_t)/4,1);
      AddMVRT();
      if(npart>2){
	for(i=1;i<=(npart-1);i++){
	  vid=2*(1+vid);    /*all tracks in events are used*/
	}
      }
      else{
	vid = -1;
      }
      MVRT->mvrt->v_id = vid;
      MVRT->mvrt->ntrk = npart-1;
      MVRT->mvrt->vert.x = MCVX->mcvx[0].x;
      MVRT->mvrt->vert.y = MCVX->mcvx[0].y;
      MVRT->mvrt->vert.z = MCVX->mcvx[0].z;
      MVRT->mvrt->chi2 = -1;
      MVRT->mvrt->cxx = 0.;
      MVRT->mvrt->cxy = 0.;
      MVRT->mvrt->cxz = 0.;
      MVRT->mvrt->cyy = 0.;
      MVRT->mvrt->cyz = 0.;
      MVRT->mvrt->czz = 0.;
      MVRT->mvrt->stat = 0; /*reserved*/  
    }

    // Make TAGR from last particle in list of MCTK bank entries
    // First check that any TAGR banks do not exist
    if(!TAGR){
      TAGR=(clasTAGR_t *)makeBank(&bcs_,"TAGR",1,sizeof(tagr_t)/4,1);
      bankList(&bcs_, "E+", "TAGR");
      TAGR->tagr[0].erg = MCTK->mctk[npart-1].pmom;
      TAGR->tagr[0].ttag = 0.0;
      TAGR->tagr[0].tpho = 0.0;
      TAGR->tagr[0].stat = 7;
      TAGR->tagr[0].t_id = 1;
      TAGR->tagr[0].e_id = 1;
    }
  }
}
//_____________________________________________________________________________

bool BOS::GetBanks(int PidGroup, int PidFlag){
  // Gets pointers to the required BOS banks 
  // If the group number is not zero, the banks it gets are determined by PidFlag.
  // If the PidGroup=0 (monte carlo thrown), the banks to get are: HEAD,MVRT,TAGR,PART.
  //
  // All id flags require the HEAD,MVRT,TAGR and TBER banks.
  // PidFlag = 0 pid id, Get PART,TBID
  // PidFlag = 1 seb id, Get EVNT,TGPB,DCPB,STPB,SCPB
  // PidFlag = 2 get 'em all
  //
  // Note: In the special case where MC banks exist, use MVRT with group 0 
  // which was created by the MCVX bank.  Otherwise, use any MVRT bank that exists.
  //
  // Default values: PidGroup = 1, PidFlag = 2

  bool BankCheck = false;

  HEAD = (clasHEAD_t *)getBank(&bcs_,"HEAD");
  TAGR = (clasTAGR_t *)getBank(&bcs_, "TAGR");
  TBER = (clasTBER_t*) getBank(&bcs_,"TBER");
  TBID =(clasTBID_t *) getBank(&bcs_, "TBID");
  PART = (clasPART_t *)getGroup(&bcs_,"PART",PidGroup);
  SCPB = (clasSCPB_t*)getBank(&bcs_,"SCPB");
  EVNT = (clasEVNT_t*)getBank(&bcs_,"EVNT");
  DCPB = (clasDCPB_t*)getBank(&bcs_,"DCPB");
  ECPB = (clasECPB_t*)getBank(&bcs_,"ECPB");
  TGPB = (clasTGPB_t*)getBank(&bcs_,"TGPB");
  STPB = (clasSTPB_t*)getBank(&bcs_,"STPB");
  HEVT = (clasHEVT_t*)getBank(&bcs_,"HEVT");
  CL01 = (clasCL01_t*)getBank(&bcs_,"CL01");
  MCTK = (clasMCTK_t *)getBank(&bcs_,"MCTK");
  MCVX = (clasMCVX_t *)getBank(&bcs_,"MCVX");

  if(PidGroup==0){  // monte carlo thrown
    if(MCTK && MCVX){ 
      MVRT = (clasMVRT_t *)getGroup(&bcs_, "MVRT",PidGroup);
    }
    else{
      MVRT = (clasMVRT_t *)getBank(&bcs_, "MVRT");
    }
    BankCheck = (HEAD && PART && TAGR && MVRT);
  }
  else{
    MVRT = (clasMVRT_t *)getBank(&bcs_, "MVRT");
    if(PidFlag == 0){
      BankCheck = (HEAD && TBID && MVRT && TAGR && PART && TBER && CL01);
    }
    else if(PidFlag == 1){
      BankCheck = (HEAD && MVRT && TAGR && TBER && EVNT && TGPB && SCPB && DCPB && STPB && HEVT);
    }
    else if(PidFlag == 2){
      //      BankCheck = (HEAD && MVRT && TAGR && TBER && EVNT && TGPB && SCPB && DCPB && STPB && PART && TBID && CL01 && HEVT);
      //BankCheck = (HEAD && MVRT && TAGR && TBER && EVNT && TGPB && SCPB && DCPB && PART && TBID && CL01 && HEVT);
      // Removed TGPB and CL01 to work with frost stuff MB 03/23/06
      BankCheck = (HEAD && MVRT && TAGR && TBER && EVNT && SCPB && DCPB && PART && TBID && HEVT);
    }   
    else {
      BankCheck = false;
      cout << "BOS::GetBanks Unknown PidFlag = " << PidFlag << endl;
    }
  }
  if(!BankCheck){
    /*    
    cout << "for Event: " << this->HEADbank()->head[0].nevent << " the missing bank(s) is(are):" << endl;
    cout << "\t";
    if(!HEAD) cout << "HEAD ";
    if(!MVRT) cout << "MVRT ";
    if(!TAGR) cout << "TAGR ";
    if(!TBER) cout << "TBER ";
    if(!EVNT) cout << "EVNT ";
    if(!TGPB) cout << "TGPB ";
    if(!SCPB) cout << "SCPB ";
    if(!DCPB) cout << "DCPB ";
    if(!STPB) cout << "STPB ";
    if(!PART) cout << "PART ";
    if(!TBID) cout << "TBID ";
    if(!CL01) cout << "CL01 ";
    if(!HEVT) cout << "HEVT ";
    if(!ECPB) cout << "ECPB ";
    cout << endl;
    */
  }
  return BankCheck;
}
//_____________________________________________________________________________

int pdg2geant(int pid){
  // input is particle data group pid, output is geant pid 
  int result=0;

  if (pid == 22) {          /* photon */
    result=1;
  } else if (pid == -11) {  /* positron */
    result=2;
  }else if (pid == 11) {    /* electron */
    result=3;
  } else if(pid == 12) {    /* neutrino */
    result=4;
  } else if(pid == 14) {    /* neutrino */
    result=4;
  } else if(pid == 16) {    /* neutrino */
    result=4;
  } else if(pid == 13) {    /* mu+ */
    result=5;
  } else if(pid == -13) {   /* mu- */
    result=6;
  } else if (pid == 111) {  /* pi 0 */
    result=7;
  } else if (pid == 211) {  /* pi + */
    result=8;
  } else if (pid == -211) { /* pi - */
    result=9;
  } else if(pid == 130) {   /* K0 long */
    result=10;
  } else if(pid == 321) {   /* K+ */
    result=11;
  } else if(pid == -321) {  /* K- */
    result=12;
  } else if(pid == 2112) {  /* neutron */
    result=13;
  } else if (pid == 2212) { /* proton */
    result=14;
  } else if (pid == -2212) { /* anti-proton */
    result=15;
  } else if (pid == 310) {  /* anti-proton */
    result=16;
  } else if(pid == 40221) { /* eta */
    result=17;
  } else if(pid == 3122) {  /* lambda */
    result=18;
  }
  return(result);
}

