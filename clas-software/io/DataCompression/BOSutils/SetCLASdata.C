#include "SetCLASdata.h"
#include <cmath>

void SetPhotons(BOS *bos,Photons *tag,float targoffset){
  // Fill the Photons object from the BOS object
  int ntagr,i,ntbid,j,Eid,Tid,sebptr=-1,nphot=0;
  bool timing;
  float vtime,E,Ttag,Tpho,vtime1,vtime2;

  tag->Reset(); // reset the Photons object

  // loop over the TAGR entries and keep all photons with stat = 7 or 15
  ntagr = bos->TAGRbank()->bank.nrow;
  ntbid = bos->TBIDbank(1)->bank.nrow;

  // now get the SEB photon from the TGPB bank
  //***************This is a Temporary g11 only Fix*********************
  if(bos->TGPBbank())
  {
    if(bos->TGPBbank()->bank.nrow > 0){
      sebptr = (int)(bos->TGPBbank()->tgpb[0].pointer/1000);
      tag->SetSEBdt(bos->TGPBbank()->tgpb[0].dt);
    }
  }
  else // A fix for frost MB 03/23/06
  {
      tag->SetSEBdt(-1000);
  }
  for(i = 0; i < ntagr; i++){
    timing = false;
    if(bos->TAGRbank()->tagr[i].stat == 7 || bos->TAGRbank()->tagr[i].stat == 15){
      // check if timing is good
      vtime = ((bos->MVRTbank()->mvrt->vert.z - targoffset)/29.9792458) + (bos->TAGRbank()->tagr[i].tpho);
      vtime1 = bos->TBIDbank(1)->tbid[0].vtime;
      vtime2 = 0.0;
      if(bos->TBIDbank(2)) vtime2 = bos->TBIDbank(2)->tbid[0].vtime;
      if(abs(vtime - bos->HEVTbank()->hevt[0].stt) < 10.0){
	timing = true;
      }
      else if(abs(vtime - vtime1) < 10.0){
	timing = true;
      }
      else if(abs(vtime - vtime2) < 10.0 && vtime2 != 0.0){
	timing = true;
      }
      if(!timing){
	for(j = 0; j < ntbid; j++){
	  if(abs(vtime - bos->TBIDbank(1)->tbid[j].sc_vtime) < 10.0 && bos->TBIDbank(1)->tbid[j].sc_id > 0){
	    timing = true;
	    j = ntbid + 1;
	  }
	  else if(abs(vtime - bos->TBIDbank(1)->tbid[j].ec_vtime) < 10.0 && bos->TBIDbank(1)->tbid[j].ec_id > 0){
	    timing = true;
	    j = ntbid + 1;
	  }
	  else if(abs(vtime - bos->TBIDbank(1)->tbid[j].st_vtime) < 10.0 && bos->TBIDbank(1)->tbid[j].st_id > 0){
	    timing = true;
	    j = ntbid + 1;
	  }
	}
      }
      if(timing){
	E = bos->TAGRbank()->tagr[i].erg;
	Ttag = bos->TAGRbank()->tagr[i].ttag;
	Tpho = bos->TAGRbank()->tagr[i].tpho;
	Eid = bos->TAGRbank()->tagr[i].e_id;
	Tid = bos->TAGRbank()->tagr[i].t_id;
	tag->AddPhoton(E,Ttag,Tpho,Tid,Eid);
	if(sebptr == i+1) tag->SetSEBphot(nphot);
	nphot++;
      }
    }
  }
}
//_____________________________________________________________________________

void SetEventHeader(BOS *bos,EventHeader *head){
  // Fill the EventHeader object from the BOS object  
  int id,sec,n,i;

  head->Reset();

  head->SetRunNumber(bos->HEADbank()->head[0].nrun);
  head->SetEventNumber(bos->HEADbank()->head[0].nevent);
  head->SetMode(bos->HEADbank()->head[0].type,bos->HEADbank()->head[0].evtclass);
  head->SetTimeProcessed(bos->HEADbank()->head[0].time);
  // Fix for frost MB 03/23/06
  if(bos->CL01bank())
  {
    head->SetRFtime1(bos->CL01bank()->cl01[0].rf1);
    head->SetRFtime2(bos->CL01bank()->cl01[0].rf2);
    head->SetRFtime(bos->CL01bank()->cl01[0].rf);
  }
  else
  {
    head->SetRFtime1(-1000);
    head->SetRFtime2(-1000);
    head->SetRFtime(-1000);
  }
  head->SetSEBtime(bos->HEVTbank()->hevt[0].stt);
  head->SetTrigBits(bos->HEADbank()->head[0].trigbits);

  // now set neutral particle info
  n = bos->EVNTbank()->bank.nrow;
  for(i = 0; i < n; i++){
    if(bos->EVNTbank()->evnt[i].status > 0 && bos->EVNTbank()->evnt[i].charge == 0 && bos->EVNTbank()->evnt[i].ecstat > 0){
      id = geant2pdg(bos->EVNTbank()->evnt[i].id);
      sec = bos->ECPBbank()->ecpb[bos->EVNTbank()->evnt[i].ecstat-1].scht/100;
      head->AddNeutral("seb",1,id,sec);
    }
  }
  n = bos->PARTbank(1)->bank.nrow;
  for(i = 0; i < n; i++){
    if(bos->PARTbank(1)->part[i].q == 0){
      id = bos->PARTbank(1)->part[i].pid;
      sec = bos->TBIDbank(1)->tbid[bos->PARTbank(1)->part[i].trkid-1].sec;
      head->AddNeutral("part",1,id,sec);
    }
  }
  if(bos->PARTbank(2)){
    n = bos->PARTbank(2)->bank.nrow;
    for(i = 0; i < n; i++){
      if(bos->PARTbank(2)->part[i].q == 0){
	id = bos->PARTbank(2)->part[i].pid;
	sec = bos->TBIDbank(2)->tbid[bos->PARTbank(2)->part[i].trkid-1].sec;
	head->AddNeutral("part",2,id,sec);
      }
    }
  }
}
//_____________________________________________________________________________

void SetTracks(BOS* bos, Tracks* tracks){
  //fill a track object for an event
  Int_t EVNTind, nevnt, dcstat, ecstat, charge, sctr, tberindEVNT,z,LayInfo;
  float cov[15], chi2, scix, sciy, sciz, scdx, scdy, scdz;
  TVector3 scinter,scdir;
  
  nevnt = bos->EVNTbank()->bank.nrow;    
  tracks->Reset(); // reset the tracks object for this event

  for(EVNTind = 0; EVNTind < nevnt; EVNTind++){
    dcstat = bos->EVNTbank()->evnt[EVNTind].dcstat;
    ecstat = bos->EVNTbank()->evnt[EVNTind].ecstat;
    charge = bos->EVNTbank()->evnt[EVNTind].charge;
    if(dcstat > 0){
      if(bos->DCPBbank()->dcpb[dcstat-1].status > 0 && charge != 0){
	tberindEVNT = bos->DCPBbank()->dcpb[dcstat-1].status;
	LayInfo = bos->TBERbank()->tber[tberindEVNT-1].layinfo1;
	cov[0] = bos->TBERbank()->tber[tberindEVNT-1].c11;
	cov[1] = bos->TBERbank()->tber[tberindEVNT-1].c12;
	cov[2] = bos->TBERbank()->tber[tberindEVNT-1].c13;
	cov[3] = bos->TBERbank()->tber[tberindEVNT-1].c14;
	cov[4] = bos->TBERbank()->tber[tberindEVNT-1].c15;
	cov[5] = bos->TBERbank()->tber[tberindEVNT-1].c22;
	cov[6] = bos->TBERbank()->tber[tberindEVNT-1].c23;
	cov[7] = bos->TBERbank()->tber[tberindEVNT-1].c24;
	cov[8] = bos->TBERbank()->tber[tberindEVNT-1].c25;
	cov[9] = bos->TBERbank()->tber[tberindEVNT-1].c33;
	cov[10]= bos->TBERbank()->tber[tberindEVNT-1].c34;
	cov[11]= bos->TBERbank()->tber[tberindEVNT-1].c35;
	cov[12]= bos->TBERbank()->tber[tberindEVNT-1].c44;
	cov[13]= bos->TBERbank()->tber[tberindEVNT-1].c45;
	cov[14]= bos->TBERbank()->tber[tberindEVNT-1].c55;
	chi2 = bos->TBERbank()->tber[tberindEVNT-1].chi2;
	
	scix = bos->DCPBbank()->dcpb[dcstat-1].x_sc;
	sciy = bos->DCPBbank()->dcpb[dcstat-1].y_sc;
	sciz = bos->DCPBbank()->dcpb[dcstat-1].z_sc;
	scinter.SetXYZ(scix,sciy,sciz);
	
	scdx = bos->DCPBbank()->dcpb[dcstat-1].cx_sc;
	scdy = bos->DCPBbank()->dcpb[dcstat-1].cy_sc;	
	scdz = bos->DCPBbank()->dcpb[dcstat-1].cz_sc;
	scdir.SetXYZ(scdx,scdy,scdz);

	tracks->AddTrack(cov,scinter,scdir,chi2);
      }
    }
  }
}

//_____________________________________________________________________________

void SetCharged(BOS *bos,Charged* charged) {
  // set charged 
  float vtime1,vtime2,sctime,scpath,scchi2,scedep,sttime,stvtime;
  float scptime,scppath,scpedep;
  int EVNTind,nevnt,dcstat,q,status,ccstat,scstat,ecstat,lcstat,ststat,TBIDind;
  int sctr,tberindEVNT,scst,scpdht,scid,PARTind,pidID1,pidID2,scpid;
  vector3_t vert;
  TVector3 v;
  AParticle part;

  charged->Reset();
  // Get vertex from MVRT and set vertex in Charged object
  vert = bos->MVRTbank()->mvrt[0].vert;
  v.SetXYZ(vert.x,vert.y,vert.z);
  charged->SetVertex(v);
  // Get vtimes from TBID banks and set them in Charged object
  vtime1 = 0.;
  vtime2 = 0.;
  if(bos->TBIDbank(1)) vtime1 = bos->TBIDbank(1)->tbid[0].vtime;
  if(bos->TBIDbank(2)) vtime2 = bos->TBIDbank(2)->tbid[0].vtime;
  charged->SetVtimes(vtime1,vtime2);
  nevnt = bos->EVNTbank()->bank.nrow;
  for(EVNTind = 0; EVNTind < nevnt; EVNTind++){
    dcstat = bos->EVNTbank()->evnt[EVNTind].dcstat;
    q = bos->EVNTbank()->evnt[EVNTind].charge;
    if(dcstat > 0){
      if(bos->DCPBbank()->dcpb[dcstat-1].status > 0 && q != 0){
	part.Zero();
	part.SetQ(q);
	part.SetSEBid(geant2pdg(bos->EVNTbank()->evnt[EVNTind].id));
	status = bos->EVNTbank()->evnt[EVNTind].status;
	dcstat = bos->EVNTbank()->evnt[EVNTind].dcstat;
	ccstat = bos->EVNTbank()->evnt[EVNTind].ccstat;
	scstat = bos->EVNTbank()->evnt[EVNTind].scstat;
	ecstat = bos->EVNTbank()->evnt[EVNTind].ecstat;
	lcstat = bos->EVNTbank()->evnt[EVNTind].lcstat;
	ststat = bos->EVNTbank()->evnt[EVNTind].ststat;

	part.SetStatFlags(status,dcstat,ccstat,scstat,ecstat,lcstat,ststat);
	// get particle's index in TBER bank
	sctr = bos->DCPBbank()->dcpb[dcstat - 1].sctr;
	tberindEVNT = sctr - 100*(sctr/100);

	part.SetP(abs(q/bos->TBERbank()->tber[tberindEVNT-1].q_over_p));
	part.SetPhi(bos->TBERbank()->tber[tberindEVNT-1].phi);
	part.SetLambda(bos->TBERbank()->tber[tberindEVNT-1].lambda);
	part.SetD0(bos->TBERbank()->tber[tberindEVNT-1].d0);
	part.SetZ0(bos->TBERbank()->tber[tberindEVNT-1].z0);
	part.SetSector(sctr/100);

	// get SC info
	if(scstat > 0){
	sctime = bos->SCPBbank()->scpb[scstat-1].time;
	scpath = bos->SCPBbank()->scpb[scstat-1].path;
	scchi2 = bos->SCPBbank()->scpb[scstat-1].chi2sc;
	scedep = bos->SCPBbank()->scpb[scstat-1].edep;
	scst = bos->SCPBbank()->scpb[scstat-1].status;
	scpdht = bos->SCPBbank()->scpb[scstat-1].scpdht;
	scid = (scpdht - (scpdht/10000)*10000)/100;
	
	part.SetSCinfo(scst,scchi2,sctime,scid,scpath,scedep);
	}
	else part.SetSCinfo(0,0.,0.,0,0.,0.);
	// Get particle's PART index
	PARTind = PARTindexFromEVNTindex(EVNTind,bos);
	if(PARTind >= 0){
	  pidID1 = bos->PARTbank(1)->part[PARTind].pid;
	  pidID2 = 0;
	  if(bos->PARTbank(2)) pidID2 = bos->PARTbank(2)->part[PARTind].pid;
	  part.SetPARTids(pidID1,pidID2);
	  if(scstat > 0){
	    TBIDind = bos->PARTbank(1)->part[PARTind].trkid - 1;
	    scptime = bos->TBIDbank(1)->tbid[TBIDind].sc_time;
	    scppath = (scptime - bos->TBIDbank(1)->tbid[TBIDind].vtime)*(bos->TBIDbank(1)->tbid[TBIDind].beta)*29.9792458;
	    scpid = bos->SCRCbank(part.Sector())->scrc[bos->TBIDbank(1)->tbid[TBIDind].sc_id - 1].id;
	    scpedep = bos->SCRCbank(part.Sector())->scrc[bos->TBIDbank(1)->tbid[TBIDind].sc_id - 1].energy;

	    part.SetSCpartInfo(scpid,scptime,scppath,scpedep);	
	  }
	  // get ST info
	  sttime = bos->TBIDbank(1)->tbid[bos->PARTbank(1)->part[PARTind].trkid -1].st_time;
	  stvtime = bos->TBIDbank(1)->tbid[bos->PARTbank(1)->part[PARTind].trkid -1].st_vtime;
	
	  part.SetSTinfo(sttime,stvtime);
	}
	else{
	  part.SetPARTids(0,0);
	  part.SetSCpartInfo(0,0.,0.,0.);
	  part.SetSTinfo(0.,0.);
	}
	// Add this particle to Charged
	charged->AddParticle(part);
      }
    }
  }
}
//_____________________________________________________________________________

int PARTindexFromEVNTindex(int EVNTindex, BOS *bos) {
  // converts an index to the EVNT bank to an index to the PART bank
  // both are C indicies (they start from 0)

  int tberindEVNT,tberindPART,i,sctr,dcstat = 0,ecstat = 0,charge;
  int PARTindex = -1,tbidind;
  int ECindexEVNT,ECindexPART;

  if(bos->EVNTbank()){
    ecstat = bos->EVNTbank()->evnt[EVNTindex].ecstat;
    dcstat = bos->EVNTbank()->evnt[EVNTindex].dcstat;
    charge = bos->EVNTbank()->evnt[EVNTindex].charge;
    if(dcstat > 0 && charge != 0){
      sctr = bos->DCPBbank()->dcpb[dcstat - 1].sctr;
      tberindEVNT = sctr - 100*(sctr/100);
      
      for(i = 0; i < bos->PARTbank(1)->bank.nrow; i++){    
	tbidind = bos->PARTbank(1)->part[i].trkid;
	tberindPART = (bos->TBIDbank(1)->tbid[tbidind -1].track);
	if(tberindPART == tberindEVNT){
	  PARTindex = i;
	  i = 1000;	  
	}
      }
    }
    else if(ecstat > 0 && charge == 0){
      ECindexEVNT = bos->ECPBbank()->ecpb[ecstat -1].scht;
      for(i = 0; i < bos->PARTbank(1)->bank.nrow; i++){
	tbidind = bos->PARTbank(1)->part[i].trkid;
	ECindexPART = 100*(bos->TBIDbank(1)->tbid[tbidind-1].sec)+bos->TBIDbank(1)->tbid[tbidind-1].ec_id;
	if(ECindexEVNT == ECindexPART){
	  PARTindex = i;
	  i = 1000;
	}
      }
    }
  }
  return PARTindex;
}
//_____________________________________________________________________________

unsigned long int GetTrigBits(BOS *bos){
  // Return what clasHEAD_t.trigbits should be (for monte carlo files)  
  int nsec = 0,i,j,nsc[6],nst[6],IDby4,sec;
  bitset<32> bits;

  for(sec = 1; sec <= 6; sec++){    
    nsc[sec-1] = 0;
    nst[sec-1] = 0;
    if(bos->SCRCbank(sec)){
      for(i = 0; i < bos->SCRCbank(sec)->bank.nrow; i++){
	if(i == 0) nsec++;
	if(bos->SCRCbank(sec)->scrc[i].time < 150.) nsc[sec-1]++;
      }
    }
  }
  if(bos->STN0bank()){
    for(i = 0; i < bos->STN0bank()->bank.nrow; i++){
      IDby4 = (bos->STN0bank()->stn0[i].id - 1)/4;
      nst[IDby4] = nst[IDby4] + 1;
    }
  }
  for(i = 1; i <= 6; i++){ 
    if(nst[i-1] > 0 && nsc[i-1] > 0) bits.set(i-1,1);
  }
  
  if(nsec >= 2) bits.set(6,1);

  return bits.to_ulong();
}
