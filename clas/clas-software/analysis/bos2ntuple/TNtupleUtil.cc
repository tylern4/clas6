//*-- Author :    Paul Eugenio 

////////////////////////////////////////////////////////////////////////
// TNtupleUtil
//
////////////////////////////////////////////////////////////////

#include "TNtupleUtil.h"


Char_t *TntpLables::gflables=0;

TntpLables::~TntpLables(){
  /*  if(flength !=0){
    delete[] fnames;
    flength=0;
    }*/
}

TntpLables::TntpLables(){
  fNlabels=0;
  //if(!gflables) gflables = new char[1];
  //flables=gflables;
}


void TntpLables::Print(void){
  cerr << flables << endl;
}

void TntpLables::Add(Char_t *string){
  char *tempstr=0; 
  cerr<<"Adding new label " <<string<<endl;
  Int_t lstr= strlen(string);
  Int_t length = strlen(flables);
  tempstr = new char[length+1];
  /*** 
  sprintf(tempstr,"%s",flables);
  delete[] flables;
  if(fNlabels){
    flables = new char[length+lstr+1];
    sprintf(flables,"%s:%s",tempstr,string);
  }
  else{
    flables = new char[lstr+1];
    sprintf(flables,"%s",string);
  }
  *************/
  if(fNlabels){
    sprintf(tempstr,"%s",flables);
    if((length+lstr+1)>2000){
      cerr<<"You must increase TntpNames::fname array size\nExiting\n";
      exit(-1);
    }
    sprintf(flables,"%s:%s",tempstr,string);
  }
  else
    sprintf(flables,"%s",string);
  

  fNlabels++;
  delete [] tempstr;
}

