#include <iostream>
#include <string>
#include <vector>
#include <cstdlib>
#include <cstring>
#include <errno.h>
#include <map>

using namespace std;

void copy_pipeline_tdc();

extern "C" {
#include "ntypes.h"
#include "bostypes.h"
#include "sc.h"
  void copy_pipeline_tdc_ () {
    copy_pipeline_tdc();
  }
}

using namespace std;

class Jsc {
  //  bool ldone;
  //  bool rdone;
  bool got_adc;
  vector<int> tleft;
  vector<int> tright;
public:
  sc_t bank;
  //  Jsc() : ldone(false), rdone(false)
  Jsc() : got_adc(false), tleft(0), tright(0)
  { memset (&bank, 0, sizeof(bank));};
  void SetADC(sc_t* ptr) { 
    bank.id   = ptr->id & 0xFF;
    bank.adcl = ptr->adcl;
    bank.adcr = ptr->adcr;
    got_adc = true;
  }
  void SetPipelineTDC(sct_t* ptr) {
    if (!ptr->tdc) return;
    if (ptr->id & 0x100 ) {
      //      if (rdone) return;
      if( !got_adc) {
      bank.id   = ptr->id & 0xFF;
      bank.tdcr = ptr->tdc;
      }
      tright.push_back(ptr->tdc);
      //      rdone     = true;
    }
    else {
      //      if (ldone) return;
      if( !got_adc ){
      bank.id   = ptr->id & 0xFF;
      bank.tdcl = ptr->tdc;
      }
      tleft.push_back(ptr->tdc);
      //      ldone     = true;
    }
  }
  void FillBank(sc_t* ptr) {
    int diff=100000;
    int ldiff=100000;
    if( tleft.size() && tright.size() ){
      for(vector<int>::iterator il = tleft.begin(); il != tleft.end(); il++) {
	int myldiff=(*il<3800) ? 3800-(*il) : (*il)-3800;
	for(vector<int>::iterator ir = tright.begin(); ir != tright.end(); ir++) {
	  int mydiff= (*il<*ir) ? (*ir)-(*il) : (*il)-(*ir);
	  if( mydiff < diff && myldiff <= ldiff) {
	    bank.tdcl = *il;
	    bank.tdcr = *ir;
	    diff = mydiff;
	    ldiff= myldiff;
	  }
	}
      }
    }
    else if( !tright.size() ) {
      for(vector<int>::iterator il = tleft.begin(); il != tleft.end(); il++) {
	int mydiff= (*il<3800) ? 3800-(*il) : (*il)-3800;
	if( mydiff < diff ) {
	  bank.tdcl = *il;
	  diff = mydiff;
	}
      }
    }
    else {
      for(vector<int>::iterator ir = tright.begin(); ir != tright.end(); ir++) {
	int mydiff= (*ir<3800) ? 3800-(*ir) : (*ir)-3800;
	if( mydiff < diff ) {
	  bank.tdcr = *ir;
	  diff = mydiff;
	}
      }
    }
    memcpy (ptr, &bank, sizeof(sc_t));
  }
};

class Jec {
  bool done;
public:
  ec_t bank;
  Jec() : done(false) {memset (&bank, 0, sizeof(bank));};
  void SetADC(ec_t* ptr) { 
    bank.id  = ptr->id;
    bank.adc = ptr->adc;
  }
  void SetPipelineTDC(ect_t* ptr) {
    if (!ptr->tdc) return;
    if (done)      return;
    bank.id  = ptr->id;
    bank.tdc = ptr->tdc;
    done     = true;
  }
  void FillBank(ec_t* ptr) {
    memcpy (ptr, &bank, sizeof(ec_t));
  }
};

class Jcc {
  bool done;
public:
  cc0_t bank;
  Jcc() : done(false) {memset (&bank, 0, sizeof(bank));};
  void SetADC(cc0_t* ptr) { 
    bank.id  = ptr->id;
    bank.adc = ptr->adc;
  }
  void SetPipelineTDC(cct_t* ptr) {
    if (!ptr->tdc) return;
    if (done)      return;
    bank.id  = ptr->id;
    bank.tdc = ptr->tdc;
    done     = true;
  }
  void FillBank(cc0_t* ptr) {
    memcpy (ptr, &bank, sizeof(cc0_t));
  }
};

void copy_pipeline_tdc() {
  map<int,Jsc> scmap[6];
  map<int,Jec> ecmap[6];
  map<int,Jcc> ccmap[6];


// copy classic banks (ADC)
  for (int sec = 0; sec<6; sec++) {
    clasSC_t*  SC  = NULL;
    clasEC_t*  EC  = NULL;
    clasCC0_t* CC  = NULL;

    scmap[sec].clear();
    SC = (clasSC_t*) getGroup(&bcs_, "SC  ", sec+1);
    if (SC) {
      for (int i=0; i<SC->bank.nrow; i++) {
	int id=SC->sc[i].id & 0xFF;
	//	cout<<sec+1<<" "<<SC->sc[i].id<<" "<<id<<" adcl "<<SC->sc[i].adcl<<" adcr "<<SC->sc[i].adcr<<endl;
	scmap[sec][id].SetADC(SC->sc+i);
      }
    }
    
    ecmap[sec].clear();
    EC = (clasEC_t*) getGroup(&bcs_, "EC  ", sec+1);
    if (EC) {
      for (int i=0; i<EC->bank.nrow; i++) {
	ecmap[sec][EC->ec[i].id].SetADC(EC->ec+i);
      }
    }
    
    ccmap[sec].clear();
    CC = (clasCC0_t*) getGroup(&bcs_, "CC  ", sec+1);
    if (CC) {
      for (int i=0; i<CC->bank.nrow; i++) {
	ccmap[sec][CC->cc0[i].id].SetADC(CC->cc0+i);
      }
    }
  }

  dropAllBanks(&bcs_, "SC  EC  CC  ");
  bankList(&bcs_, "E+", "SC  EC  CC  ");

  for (int sec = 0; sec<6; sec++) {
    clasSCT_t* SCT = NULL;
    clasECT_t* ECT = NULL;
    clasCCT_t* CCT = NULL;
  
    SCT = (clasSCT_t*) getGroup(&bcs_, "SCT ", sec+1);
    if (SCT) {
      for (int i=0; i<SCT->bank.nrow; i++) {
	int id = SCT->sct[i].id & 0xFF;
	//	cout<<sec+1<<" "<<SCT->sct[i].id<<" "<<id<<" tdc "<<SCT->sct[i].tdc<<endl;
	scmap[sec][id].SetPipelineTDC(SCT->sct+i);
      }
    }

    ECT = (clasECT_t*) getGroup(&bcs_, "ECT ", sec+1);
    if (ECT) {
      for (int i=0; i<ECT->bank.nrow; i++) {
	int id = ECT->ect[i].id;
	ecmap[sec][id].SetPipelineTDC(ECT->ect+i);
      }
    }
    
    CCT = (clasCCT_t*) getGroup(&bcs_, "CCT ", sec+1);
    if (CCT) {
      for (int i=0; i<CCT->bank.nrow; i++) {
	int id = CCT->cct[i].id;
	ccmap[sec][id].SetPipelineTDC(CCT->cct+i);
      }
    }
  }

  for (int sec = 0; sec<6; sec++) {
    clasSC_t*  SC  = NULL;
    if (scmap[sec].size()) {
      if(SC = (clasSC_t*) makeBank(&bcs_, "SC  ", sec+1, 
		       sizeof(sc_t)/2, scmap[sec].size())){
	int i = 0;
	for (map<int,Jsc>::iterator it = scmap[sec].begin();
	     it!=scmap[sec].end(); it++) {

	  it->second.FillBank( SC->sc + i );
	  //	  cout<<sec+1<<" "<<SC->sc[i].id<<" tdcl "<<SC->sc[i].tdcl<<" tdcr "<<SC->sc[i].tdcr<<" adcl "<<SC->sc[i].adcl<<" adcr "<<SC->sc[i].adcr<<endl;
	  i++;
	}
      }
      else throw "Can't create SC bank";
    }
  }

  for (int sec = 0; sec<6; sec++) {
    clasEC_t*  EC  = NULL;
    if (ecmap[sec].size()) {
      if(EC = (clasEC_t*) makeBank(&bcs_, "EC  ", sec+1, 
		      sizeof(ec_t)/2, ecmap[sec].size())){
	int i = 0;
	for (map<int,Jec>::iterator it = ecmap[sec].begin();
	     it!=ecmap[sec].end(); it++) {
	  it->second.FillBank( EC->ec + i++ );
	}
      }
      else throw "Can't create EC bank";
    }
  }

  for (int sec = 0; sec<6; sec++) {
    clasCC0_t*  CC  = NULL;
    if (ccmap[sec].size()) {
      if(CC = (clasCC0_t*) makeBank(&bcs_, "CC  ", sec+1, 
		       sizeof(cc0_t)/2, ccmap[sec].size())){
	int i = 0;
	for (map<int,Jcc>::iterator it = ccmap[sec].begin();
	     it!=ccmap[sec].end(); it++) {
	  it->second.FillBank( CC->cc0 + i++ );
	}
      }
      else throw "Can't create CC bank";
    }
  }
}

