#include <iostream>
#include <cmath>
#include <fstream>
#include <cstdio>
using namespace std;

#include "TROOT.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"

extern "C"
{
#include <ntypes.h>
#include <bostypes.h>
}

extern BOSbank bcs_;
clasTBLA_t *TBLA;

int main(int argc, char *argv[])
{
 char  command[100];
 int   Counter=0;
 float residual;
 float doca;
 int   sl ;
 int   status;

 TFile output("muana.root","RECREATE","");
 TH2D* reg1 = new TH2D("reg1", "reg1", 40, -.05 , .9  , 60, -.18, .18);
 TH2D* reg2 = new TH2D("reg2", "reg2", 40, -.1  , 1.5 , 60, -.22, .22);
 TH2D* reg3 = new TH2D("reg3", "reg3", 40, -.1  , 2.4 , 60, -.22, .22);

 sprintf(command,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", argv[1]);

 initbos();
 if(!fparm_c(command)) cout << "\nError opening file   " << argv[1] << "\n\n\n" <<  endl;
 else // Begin of Event reconstruction
 {
  cout << endl << " Filling the histograms..." << endl;
  while (getBOS(&bcs_, 1, "E") && Counter < 50000)
  {
   if(fmod((double)++Counter, (double)5000) == 0) cerr << " Number of events: " << Counter  << endl;
   for(int sec=1;sec<7;sec++)
   {
    TBLA=(clasTBLA_t *) getGroup(&bcs_, "TBLA", sec);
    if(TBLA)
    {
     for(int i=0;i<TBLA->bank.nrow;i++)
     {
//      doca     = fabs(TBLA->tbla[i].calcdoca) ;
      doca     = fabs(TBLA->tbla[i].fitdoca);
      residual = fabs(TBLA->tbla[i].calcdoca)-fabs(TBLA->tbla[i].fitdoca) ;
      sl       = ((TBLA->tbla[i].trk_pln)%100 - 4)/6 + 1;
      status   = TBLA->tbla[i].stat ;
      if((sl==1 || sl==2) && status==0) reg1->Fill(doca, residual);
      if((sl==4 || sl==4) && status==0) reg2->Fill(doca, residual);
      if((sl==5 || sl==6) && status==0) reg3->Fill(doca, residual);
     }
    }
   }
   cleanBanks(&bcs_);
  }
  output.Write();
  cout << endl << "Total events written: " << Counter << endl << endl;
 }
 return 0;
}
