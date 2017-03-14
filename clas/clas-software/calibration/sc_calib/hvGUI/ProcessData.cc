//*************************
/* File : ProcessData.C
   Initial Author :  Joe Santoro (VPI)
   Mod Author: Paul Mattione (Rice)
   Creation Date  :  9/2004
*/
//******************************* Database interface ****************************************************
// Uses the calDB interface routines located in packages/ClasTool/MapUtils
// The latest version of these sources is included in this release 9/2004
//*********************************************************************************

#include <MapUtils/TMapUtils.h>
#include "HistoWindow.cc"
#include "MainWindow.h"

//**** CLAS HEADERS *******
extern "C"
{
#include <ntypes.h>
#include <bostypes.h>
#include <clas_cern.h>
#include <kinematics.h>
#include <map_manager.h>
#include <sc.h>
#include <utility.h>
#include <particleType.h>
#include <printBOS.h>
#include <pid.h>
#include <call.h>
#include <ec.h>
#include <vertex.h>
#include <itape.h>
#include <mysql.h>
#include <makebanks.h>
}

//******************************************
int MainWindow::ProcessLargeAngle()
{

int i,Nevents, max, sector=0, index=0;
char mess[256];
clasHEAD_t *HEAD  = NULL;
clasSC_t   *SC    = NULL;

string locFileName = (string)((char *)Get_OUTPUTDIR_name()->GetText());
locFileName += "/LargeAngleCounters.root";
TFile*  counters  = new TFile(locFileName.c_str(),"RECREATE","");

TTree* GAINTREEaS = new TTree("GAINTREEaS", "A Tree to hold Gain-Matching Variables for a file with just As on");
event_t gainA;
GAINTREEaS->Branch("gainA", &gainA.ADCL,"ADCL/I:ADCR/I:ADC/D:log_ADCL_ADCR/D:SECTOR/I:COUNTER/I:runno/I:eventnumber/I");

max = Get_MaxLA();

//********** THE A FILES **********************************************
initbos();
Progress *ProgressA;
  ProgressA = new Progress(fClient->GetRoot(), this, 400, 50, fileAString.c_str());
  sprintf(mess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", fileAString.c_str());

if (!fparm_c(mess)) {
TGMsgBox *HALT = new TGMsgBox(fClient->GetRoot(),this,"Open Error","Couldn't Open BOS File!",kMBIconStop);
}
else {
cout<<  "WE ARE PROCESSING THE LARGE ANGLES NOW"<<endl;
Nevents=0;

while ((max ? Nevents < max : 1) && getBOS(&bcs_,1,"E")) {
 Nevents++;
 HEAD = (clasHEAD_t *) getBank(&bcs_, "HEAD");

 if(Nevents==1){
 gainA.runno = HEAD->head[0].nrun;
 ProgressA->TheBar->SetRange(0,max);
 LEFT_PED  = getSCpedestals(gainA.runno, "left" ,RUNINDEX_name);
 RIGHT_PED = getSCpedestals(gainA.runno, "right",RUNINDEX_name);
 }
 gainA.eventnumber = HEAD->head[0].nevent;

 for(sector=1; sector<=6; sector++){
   SC = (clasSC_t *)getGroup(&bcs_,"SC  ", sector);

 if(SC){
  for (i=0; i< SC->bank.nrow; i++){
  gainA.COUNTER =  (SC->sc[i].id)&0xFF;
  gainA.SECTOR  =  sector;
  index = (gainA.SECTOR-1)*57 + (gainA.COUNTER-1);
//sc_index(sector,gainForward.COUNTER); //PAUL
  gainA.ADCL  = SC->sc[i].adcl-(int)LEFT_PED[index];
  if(gainA.ADCL<0){gainA.ADCL=0;}
  gainA.ADCR  = SC->sc[i].adcr-(int)RIGHT_PED[index];
  if(gainA.ADCR<0){gainA.ADCR=0;}
  gainA.ADC           = sqrt((double)gainA.ADCL*(double)gainA.ADCR);
  gainA.log_ADCL_ADCR = log((double)gainA.ADCR/(double)gainA.ADCL);
  GAINTREEaS->Fill();
  }
 }
}

 ProgressA->TheBar->SetPosition(Nevents);
 gSystem->ProcessEvents();
 if(Nevents%100==0){
 fprintf(stderr,"%d\r",Nevents);
 fflush(stderr);
 }

  dropBank(&bcs_, "SC  ", sector);
  cleanBanks(&bcs_);
  }
 }

fparm_c("CLOSE BOSINPUT");
ProgressA->~Progress();

  counters->Write();
  counters->Close();
}

//*********************************************
int MainWindow::ProcessForwardAngle()
{
int i,Nevents, max, sector=0, index;
char mess[256];
clasHEAD_t *HEAD  = NULL;
clasSC_t   *SC    = NULL;

string locFileName = (string)((char *)Get_OUTPUTDIR_name()->GetText());
locFileName += "/ForwardCounters.root";
TFile*  counters  = new TFile(locFileName.c_str(),"RECREATE","");

TTree* GAINTREEforward = new TTree("GAINTREEforward", "A Tree to hold Gain-Matching Variables for forward counters");
event_t gainForward;
GAINTREEforward->Branch("gainForward", &gainForward.ADCL,"ADCL/I:ADCR/I:ADC/D:log_ADCL_ADCR/D:SECTOR/I:COUNTER/I:runno/I:eventnumber/I");

max = Get_MaxFA();

//********** FORWARD COUNTERS **********************************************
initbos();
Progress *ProgressForward;
  ProgressForward = new Progress(fClient->GetRoot(), this, 400, 50, fileCString.c_str());
  sprintf(mess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", fileCString.c_str());

if (!fparm_c(mess)) {
TGMsgBox *HALT = new TGMsgBox(fClient->GetRoot(), this, "Open Error", "Couldn't Open BOS File!", kMBIconStop);
}
else {
cout<<"WE ARE PROCESSING THE FORWARD ANGLES NOW"<<endl;
Nevents=0;

while ((max ? Nevents < max : 1) && getBOS(&bcs_,1,"E")) {
 Nevents++;
 HEAD = (clasHEAD_t *) getBank(&bcs_, "HEAD");
 if(Nevents==1){
 gainForward.runno            = HEAD->head[0].nrun;
 ProgressForward->TheBar->SetRange(0,max);
 LEFT_PED  = getSCpedestals(gainForward.runno, "left" ,RUNINDEX_name);
 RIGHT_PED = getSCpedestals(gainForward.runno, "right",RUNINDEX_name);
 }
 gainForward.eventnumber = HEAD->head[0].nevent;

 for(sector=1; sector<=6; sector++){
   SC = (clasSC_t *)getGroup(&bcs_,"SC  ", sector);

 if(SC){
  for (i=0; i< SC->bank.nrow; i++){
  gainForward.COUNTER =  (SC->sc[i].id)&0xFF;
  gainForward.SECTOR  =  sector;
  index = (gainForward.SECTOR-1)*57 + (gainForward.COUNTER-1);
//sc_index(sector,gainForward.COUNTER); //PAUL
  gainForward.ADCL = SC->sc[i].adcl-(int)LEFT_PED[index];
  if(gainForward.ADCL<0){gainForward.ADCL=0;}
  gainForward.ADCR = SC->sc[i].adcr-(int)RIGHT_PED[index];
  if(gainForward.ADCR<0){gainForward.ADCR=0;}
  gainForward.ADC           = sqrt((double)gainForward.ADCL*(double)gainForward.ADCR);
  gainForward.log_ADCL_ADCR = log((double)gainForward.ADCR/(double)gainForward.ADCL);
  GAINTREEforward->Fill();
  }
 }
}

 ProgressForward->TheBar->SetPosition(Nevents);
 gSystem->ProcessEvents();
 if(Nevents%100==0){
 fprintf(stderr,"%d\r",Nevents);
 fflush(stderr);
 }

  dropBank(&bcs_, "SC   ", sector);
  cleanBanks(&bcs_);
  }
 }

counters->Write();
counters->Close();
fparm_c("CLOSE BOSINPUT");
ProgressForward->~Progress();

}

//*********** Gets SC pedestals from the calDB *********************
TArrayF MainWindow::getSCpedestals(int runno, const char *item, TGTextEntry *RunIndexName)
{
TArrayF  Pedestals(342);
TMapUtils  *CalibMap = new TMapUtils("clasdb.jlab.org","calib","clasuser", (char *)RunIndexName->GetText());

CalibMap->PrintInfo();
cout<< "OBTAINING PEDESTAL CONSTANTS FOR RUN NUMBER ====> "<< runno << " AND ITEM: " << item<<endl;

CalibMap->Get_Map_Float("SC_CALIBRATIONS_V2","pedestals",item,runno,&Pedestals,"");
CalibMap->PrintFloatArray(Pedestals);
return Pedestals;
}

//**************** Progress Window Constructor for File Processing *********************
Progress::Progress(const TGWindow *p, const TGWindow *main,
               UInt_t w, UInt_t h, const char *title):TGTransientFrame(p, main, w, h)
{
 fHframe  = new TGHorizontalFrame(this, w, h, 0);
 TheBar   = new TGHProgressBar(fHframe, TGProgressBar::kFancy, w);
 TheBar->SetBarColor("lightblue");
 //TheBar->ShowPosition(kTRUE, kFALSE, "%.0f Events Processed");
 TheBar->ShowPosition(kTRUE, kTRUE, "%.0f Events Processed");

 fHint  = new TGLayoutHints(kLHintsTop | kLHintsLeft | kLHintsExpandX, 1, 1,  1, 1);
 AddFrame(fHframe, fHint);
 SetWindowName(title);
 MapSubwindows();
 MapWindow();
}

//******* Progress Window Destructor *********
Progress::~Progress()
{
Cleanup();
}

