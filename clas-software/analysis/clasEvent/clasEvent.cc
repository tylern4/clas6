using namespace std;
#include<list>
#include<vector>
#include <stdlib.h>
#include <errno.h>
#include <clasEvent.h>
#include <mwKfit.h>



extern "C" {
  int initProcess(char *name,int dispatchertrkidPipelinePrefill);
#include <unistd.h>
#include <sc.h>
#include <ec.h>
#include <utility.h>
#include <makebanks.h>
#include <tag_cpp_wrapper.h>
#include <st_cpp_wrapper.h>
#include <pid.h>
#include <trk.h>
//#include <g1cPcor.h>
#include <Pcor.h>
#include <g10pcor.h>
#include <PartUtil.h>

  void dc_set_def_();
  void trk_set_def_();
  void targcell_(int &icell,float &vert,float &cdir,float &dist,float &dist1);
  float TAGRcor(float epho, float E0, int runID);
}

int comp_tagr(const void* a,const void* b)
{
  int ret;
  tagr_t *at = (tagr_t *)a;
  tagr_t *bt = (tagr_t *)b;

  if(at->erg > bt->erg) {
    ret = 1;
  }
  else if (at->erg < bt->erg) {
    ret = -1;
  }
  else
    ret = 0;

  return(ret);
}

void clasEvent::copy(clasEvent &evt)
{

  this->_status = evt.status();
  this->_verbose = evt.verbose();
  this->_configSC = evt.configSCStatus();
  this->_configTGEO = evt.configTGEOStatus();
  this->_configSWIM = evt.configSwimStatus();
  this->_configTBID = evt.configTBIDStatus();
  this->_configPcorr = evt.configPcorrStatus();
  this->_configEloss = evt.configElossStatus();
//  this->_configg1cPcorrection = evt.configg1cPcorrectionStatus();
  this->_configPcor = evt.configPcorStatus();
  this->_configg10pcor = evt.configg10pcorStatus();
  this->_configMomentumCorrection = evt.configMomentumCorrectionStatus();
  this->_configEC = evt.configECStatus();
  this->_configSC = evt.configSCStatus();
  this->_configCC = evt.configCCStatus();
  this->_configTAGGER = evt.configTAGGERStatus();
  this->_configRunControl = evt.configRunControlStatus();

  this->_TAGR = evt.TAGR();
  this->_TAGM = evt.TAGM();

}

int clasEvent::tagr_id()
{
  return(this->_tagrIndex);
}

fourVec clasEvent::getBeam(double E0,double E1) {
  tagr_t *tagr = get_photon_tagrEcut(this->TAGR(),(clasBID_t *)this->TBID(),E0,E1);
  if (tagr) {
    this->setTagr(tagr);
    return(fourVec(tagr->erg,threeVec(0.0,0.0,tagr->erg)));
  }
  else
    return(fourVec(0.0,threeVec(0.0,0.0,0.0)));


}

clasGPID_t * clasEvent::MakeGPID(int partbank)
{
  clasGPID_t *ret = NULL;
  clasTDPL_t *TDPL = (clasTDPL_t *)this->getEvtBank("TDPL");

  bankList(this->_bcs, "E+", "GPID");

  if (this->_configSC != this->run()) {
    this->ConfigSC();
  }
  if (this->_configGPID != this->run()) {
    initGPID(this->run());
    this->_configGPID = this->run();
  }
  if (!TDPL)
    this->buildSwim();
  if (TDPL = (clasTDPL_t *)this->getEvtBank("TDPL"))
    ret = makeGPID(partbank,0);
  else
    if (this->verbose())
      std::cerr << "unable to make swim banks...unable to make GPID bank " << this->run() << " " << this->event() << std::endl;
  return(ret);

}



int clasEvent::_load(int partbank)
{

  int ret = 1;
  clasHEAD_t *HEAD = (clasHEAD_t *)getBank(this->_bcs,"HEAD");
  clasTAGR_t *TAGR = (clasTAGR_t *)getBank(this->_bcs,"TAGR");
  clasCC01_t *CC01 = (clasCC01_t *)getBank(this->_bcs,"CC01");
  clasCC0_t *CC=(clasCC0_t *)getBank(this->_bcs,"CC");
  clasMVRT_t *MVRT = (clasMVRT_t *)getBank(this->_bcs,"MVRT");
  clasTBTR_t *TBTR = (clasTBTR_t *)getBank(this->_bcs,"TBTR");
  clasTBER_t *TBER = (clasTBER_t *)getBank(this->_bcs,"TBER");
  clasVERT_t *VERT;

  unsigned long tagr0,tagrx;
  double diff = 1000.0;
  double beamp = 0.0;
  int usePartBeam = 0;
  particle target = particle(PDGtable.get("p"),1);
  particle beampart = particle(PDGtable.get("gamma"),0);

  this->_TBID = NULL;

  if (!_noLoad) {
    this->_mc = (partbank == 0);
    if (!this->_mc) {
      if (TBTR)
    this->_TBID  =  (clasTBID_t *)getBank(this->_bcs,"TBID");
      this->_TBTR = TBTR;
      this->_TBER = TBER;
    }
    else {
      this->_TBID = 0;
      this->_TBTR = 0;
      this->_TBER = 0;
    }

    if ((HEAD->head[0].type == DATA_TYPE) ||(HEAD->head[0].type == MONTE_CARLO_TYPE) ) {

      this->_TAGR = TAGR;
      this->_CC01 = CC01;
      this->_CC=CC;
      this->_initial.clear();
      this->_final.clear();
      this->_cp.clear();
      this->_tag.clear();
      this->_newBanks.clear();
      this->_targetZ = target_z();
      this->_The_tbid = NULL;

      // Set up the run period and the run class

      this->_runPeriod = getRunPeriod(this->run());
      this->_runClass = getRunClass(this->_runPeriod);

      this->ConfigRunControl();

      // set the prlink file:

      this->setPrlink(this->defaultPrlink().c_str());

      if (MVRT)
    this->_mvrt = MVRT->mvrt;
      else {
    // let's try to make the vertices
    this->makeVertex();
    if (MVRT = (clasMVRT_t *)getBank(this->_bcs,"MVRT"))
      this->_mvrt = MVRT->mvrt;
    else
      this->_mvrt = NULL;
      }

      VERT = (clasVERT_t *)getGroup(this->_bcs,"VERT",2);

      //   std::cerr << " _load CLASEVENT !!! " << std::endl;


      // go through PART bank and see if beam particle is there...


      this->_PART = (clasPART_t *)getGroup(this->_bcs,"PART",partbank);
      if (!this->_useGPID  && !this->_useSEB) {
    if (this->_PART) {
      for (int k = 0; k < this->_PART->bank.nrow; ++k) {
        part_t *part = &this->_PART->part[k];
        if (part->flags == BEAM_FLAG) {
          beamp = part->p.t;
          usePartBeam = 1;
        }
      }
    }
      }
      if (this->_useSEB) {
    clasTGPB_t *TGPB = (clasTGPB_t *)this->getEvtBank("TGPB");
    if (TGPB) {
      beamp = TGPB->tgpb[0].energy;
      _tagrIndex = abs(TGPB->tgpb[0].pointer/1000 - 1);
      if (TAGR)
        _tagr = &TAGR->tagr[_tagrIndex - 1];
    }
    else
      beamp = 0.0;
      }

      target.set3P(threeVec(0.0,0.0,0.0));
      beampart.set3P(threeVec(0.0,0.0,beamp));
      this->target(target);
      if (!this->_useSEB && !this->_mc && TAGR && this->_TBID) {
    if (this->runPeriod() == g7) {
      if (!this->_configSCG) {
        this->ConfigSCG();
      }
      this->_tagr = get_photon_tagrG7(TAGR,(clasBID_t *)this->_TBID);
      if (!usePartBeam)
        beamp = this->_tagr ? this->_tagr->erg : 0.0;
    }
    else {
      this->_tagr = get_photon_tagr(TAGR,(clasBID_t *)this->_TBID, TIME_BASED);
      if (!usePartBeam)
        beamp = get_photon_energy(TAGR,(clasBID_t *)this->_TBID);
    }

    tagr0 = (unsigned long) &this->_TAGR->tagr[0];
    tagrx = (unsigned long) this->_tagr;

    this->_tagrIndex = (tagrx - tagr0)/sizeof(tagr_t);

    // loop over TBID and find the time-defining charged track
    diff = 1000.0;

    for (int i = 0; i < this->_TBID->bank.nrow; ++i) {

      tbid_t *tbid = &this->_TBID->tbid[i];
      if (tbid->track && tbid->st_stat && tbid->sc_stat) {
        if (fabs(tbid->st_vtime - this->_TBID->tbid[0].vtime) < diff) {
          diff = fabs(tbid->st_vtime - this->_TBID->tbid[0].vtime);
          this->_The_tbid = tbid;
        }
      }
    }

    // kludge for g7
    if (!usePartBeam && beamp < .0001 && TAGR) {
      beamp = TAGR->tagr[0].erg;
    }
      }
      else if (TAGR) {
    // assume mc
    this->_tagr = &TAGR->tagr[0];
    if (this->_mc)
      beamp = TAGR->tagr[0].erg;
      }

      beampart.set3P(threeVec(0.0,0.0,beamp));
      this->beam(beampart);
      if (HEAD)
    this->_head = &HEAD->head[0];
      else
    this->_head = NULL;


      if (this->_useGPID) {
    clasGPID_t *GPID = (clasGPID_t *)getBank(this->_bcs,"GPID");
    if (!GPID) {
      bankList(&bcs_, "E+", "GPID");
      if (this->_configSC != this->run()) {
        this->ConfigSC();
      }
      if (this->_configGPID != this->run()) {
        initGPID(this->run());
        this->_configGPID = this->run();
      }
      GPID = this->MakeGPID(partbank);
    }
    if (GPID) {
      for (int k = 0; k < GPID->bank.nrow; ++k) {
        fourVec p1;
        fourVec p2;
        particle P;
        clasParticle cp;
        gpid_t *gpid = &GPID->gpid[k];
        p1.set(gpid->p.t,threeVec(gpid->p.space.x,gpid->p.space.y,gpid->p.space.z));
        cp.qtrk(0.0);
        cp.pidFlag(GPID_FLAG);
        cp.setTBID(&this->_TBID->tbid[gpid->trkid - 1]);
        cp.setTAGRindex(gpid->tagrid - 1);
        cp.gpidIndex(k);
        cp.setEvent(this);


        switch((Particle_t) gpid->pid) {
        case Unknown:
          if (this->_useUnknown) {
        P = particle(PDGtable.get("p"),1);
        P.set4P(p1);
        this->addfinal(P);
          cp.pid(Unknown);
          cp.tbid(gpid->trkid);
          //std::cerr << " TEST cp.tdid PROTON " <<  cp.tbid(gpid->trkid) <<std::endl;
          cp.setTBID(&this->_TBID->tbid[gpid->trkid - 1]);
          cp.bcs(this->_bcs);
          cp.p(p1);
          cp.sec(_TBID ? _TBID->tbid[gpid->trkid - 1].sec : -1);
          this->addParticle(cp);

          }
          break;
        case Gamma:
          P = particle(PDGtable.get("gamma"),0);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(Gamma);
          if (partbank) {
        cp.tbid(gpid->trkid);
        cp.setTBID(&this->_TBID->tbid[gpid->trkid - 1]);
        cp.sec(_TBID ? _TBID->tbid[gpid->trkid - 1].sec : -1);
          }
          cp.bcs(this->_bcs);
          cp.p(p1);

          this->addParticle(cp);
          break;
        case Proton:
          P = particle(PDGtable.get("p"),1);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(Proton);
          cp.tbid(gpid->trkid);
          //std::cerr << " TEST cp.tdid PROTON " <<  cp.tbid(gpid->trkid) <<std::endl;
          cp.setTBID(&this->_TBID->tbid[gpid->trkid - 1]);
          cp.bcs(this->_bcs);
          cp.p(p1);
          cp.sec(_TBID ? _TBID->tbid[gpid->trkid - 1].sec : -1);
          this->addParticle(cp);
          break;
        case Deuteron:
          P = particle(PDGtable.get("d"),1);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(Deuteron);
          cp.tbid(gpid->trkid);
          //std::cerr << " TEST cp.tdid PROTON " <<  cp.tbid(gpid->trkid) <<std::endl;
          cp.setTBID(&this->_TBID->tbid[gpid->trkid - 1]);
          cp.bcs(this->_bcs);
          cp.p(p1);
          cp.sec(_TBID ? _TBID->tbid[gpid->trkid - 1].sec : -1);
          this->addParticle(cp);
          break;
        case AntiProton:
          P = particle(PDGtable.get("pbar"),-1);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(AntiProton);
          if (partbank) {
        cp.tbid(gpid->trkid);
        cp.setTBID(&this->_TBID->tbid[gpid->trkid - 1]);
        cp.sec(_TBID ? _TBID->tbid[gpid->trkid - 1].sec : -1);
          }
          cp.bcs(this->_bcs);
          cp.p(p1);

          this->addParticle(cp);
          break;
        case Neutron:
          P = particle(PDGtable.get("n"),0);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(Neutron);
          cp.bcs(this->_bcs);
          if (partbank) {
        cp.tbid(gpid->trkid);
        cp.setTBID(&this->_TBID->tbid[gpid->trkid - 1]);
        cp.sec(_TBID ? _TBID->tbid[gpid->trkid - 1].sec : -1);
          }
          cp.p(p1);

          this->addParticle(cp);
          break;
        case PiPlus:
          P = particle(PDGtable.get("pi"),1);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(PiPlus);
          if (partbank) {
        cp.tbid(gpid->trkid);
        cp.setTBID(&this->_TBID->tbid[gpid->trkid - 1]);
        cp.ccid(this->_TBID ? this->_TBID->tbid[gpid->trkid -1].cc_id : -1);
        cp.nPhe(cp.ccid() ? ((this->_CC01) ? this->_CC01->cc01[cp.ccid() - 1].n_pe : 0) : 0);
        //cp.nPhe(cp.ccadc() ? ((this->_CC) ? this->_CC->cc0[cp.ccid() - 1].adc : 0) : 0);
        cp.sec(_TBID ? _TBID->tbid[gpid->trkid - 1].sec : -1);
          }
          cp.bcs(this->_bcs);
          cp.p(p1);

          this->addParticle(cp);
          break;
        case Electron:
          P = particle(PDGtable.get("e"),-1);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(Electron);
          if (partbank) {
        cp.tbid(gpid->trkid);
        cp.setTBID(&this->_TBID->tbid[gpid->trkid - 1]);
        cp.ccid(this->_TBID ? this->_TBID->tbid[gpid->trkid -1].cc_id : -1);
        cp.nPhe(cp.ccid() ? ((this->_CC01) ? this->_CC01->cc01[cp.ccid() - 1].n_pe : 0) : 0);
        cp.sec(_TBID ? _TBID->tbid[gpid->trkid - 1].sec : -1);
          }
          cp.bcs(this->_bcs);
          cp.p(p1);

          this->addParticle(cp);
          break;
        case Positron:
          P = particle(PDGtable.get("e"),1);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(Positron);
          if (partbank) {
        cp.tbid(gpid->trkid);
        cp.setTBID(&this->_TBID->tbid[gpid->trkid - 1]);
        cp.ccid(this->_TBID ? this->_TBID->tbid[gpid->trkid -1].cc_id : -1);
        cp.nPhe(cp.ccid() ? ((this->_CC01) ? this->_CC01->cc01[cp.ccid() - 1].n_pe : 0) : 0);
        cp.sec(_TBID ? _TBID->tbid[gpid->trkid - 1].sec : -1);
          }
          cp.bcs(this->_bcs);
          cp.p(p1);

          this->addParticle(cp);
          break;
        case PiMinus:
          P = particle(PDGtable.get("pi"),-1);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(PiMinus);
          if (partbank) {
        cp.tbid(gpid->trkid);
        cp.setTBID(&this->_TBID->tbid[gpid->trkid - 1]);
        cp.ccid(this->_TBID ? this->_TBID->tbid[gpid->trkid -1].cc_id : -1);
        cp.nPhe(cp.ccid() ? ((this->_CC01) ? this->_CC01->cc01[cp.ccid() - 1].n_pe : 0) : 0);
        cp.sec(_TBID ? _TBID->tbid[gpid->trkid - 1].sec : -1);
          }
          cp.bcs(this->_bcs);
          cp.p(p1);

          this->addParticle(cp);
          break;
        case Pi0:
          P = particle(PDGtable.get("pi0"),0);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(Pi0);
          if (partbank) {
        cp.tbid(gpid->trkid);
        cp.setTBID(&this->_TBID->tbid[gpid->trkid - 1]);
        cp.sec(_TBID ? _TBID->tbid[gpid->trkid - 1].sec : -1);
          }
          cp.bcs(this->_bcs);
          cp.p(p1);

          this->addParticle(cp);
          break;
        case KPlus:
          P = particle(PDGtable.get("K"),1);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(KPlus);
          if (partbank) {
        cp.tbid(gpid->trkid);
        cp.setTBID(&this->_TBID->tbid[gpid->trkid - 1]);
        cp.ccid(this->_TBID ? this->_TBID->tbid[gpid->trkid -1].cc_id : -1);
        cp.nPhe(cp.ccid() ? ((this->_CC01) ? this->_CC01->cc01[cp.ccid() - 1].n_pe : 0) : 0);
        cp.sec(_TBID ? _TBID->tbid[gpid->trkid - 1].sec : -1);
          }
          cp.bcs(this->_bcs);
          cp.p(p1);

          this->addParticle(cp);
          break;
        case KMinus:
          P = particle(PDGtable.get("K"),-1);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(KMinus);
          if (partbank) {
        cp.tbid(gpid->trkid);
        cp.setTBID(&this->_TBID->tbid[gpid->trkid - 1]);
        cp.ccid(this->_TBID ? this->_TBID->tbid[gpid->trkid -1].cc_id : -1);
        cp.nPhe(cp.ccid() ? ((this->_CC01) ? this->_CC01->cc01[cp.ccid() - 1].n_pe : 0) : 0);
        cp.sec(_TBID ? _TBID->tbid[gpid->trkid - 1].sec : -1);
          }
          cp.bcs(this->_bcs);
          cp.p(p1);

          this->addParticle(cp);
          break;
        }
      }
    }
      }
      else if (this->_useSEB) {
    clasEVNT_t *EVNT = (clasEVNT_t *) getBank(this->_bcs,"EVNT");
    if (EVNT) {
      for (int k = 0; k < EVNT->bank.nrow; ++k) {
        fourVec p1;
        fourVec p2;
        particle P;
        clasParticle cp;
        evnt_t *evnt = &EVNT->evnt[k];
        Particle_t sebID = sebPID(evnt->id);
        double t = sqrt(evnt->pmom * evnt->pmom + M(sebID) * M(sebID));

        if (evnt->status > 0) {

        p1.set(t,threeVec(evnt->pmom * evnt->dir_cos.x,evnt->pmom * evnt->dir_cos.y,evnt->pmom * evnt->dir_cos.z));
        cp.qtrk((double) Q(sebID));
        cp.pidFlag(SEB_FLAG);
        //        cp.setTAGRindex(gpid->tagrid - 1);
        cp.setEvent(this);


        switch((Particle_t) sebID) {
        case Gamma:
          P = particle(PDGtable.get("gamma"),0);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(Gamma);
          cp.bcs(this->_bcs);
          cp.p(p1);

          this->addParticle(cp);
          break;

        case Proton:
          P = particle(PDGtable.get("p"),1);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(Proton);
          cp.bcs(this->_bcs);
          cp.p(p1);
          this->addParticle(cp);
          break;
        case Deuteron:
          P = particle(PDGtable.get("d"),1);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(Deuteron);
          cp.bcs(this->_bcs);
          cp.p(p1);
          this->addParticle(cp);
          break;
        case AntiProton:
          P = particle(PDGtable.get("pbar"),1);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(AntiProton);
          cp.bcs(this->_bcs);
          cp.p(p1);
          this->addParticle(cp);
          break;
        case Neutron:
          P = particle(PDGtable.get("n"),0);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(Neutron);
          cp.bcs(this->_bcs);
          cp.p(p1);
          this->addParticle(cp);
          break;
        case PiPlus:
          P = particle(PDGtable.get("pi"),1);

          P.set4P(p1);
          this->addfinal(P);
          cp.pid(PiPlus);
          cp.bcs(this->_bcs);
          cp.p(p1);
          this->addParticle(cp);
          break;
        case Electron:
          P = particle(PDGtable.get("e"),-1);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(Electron);
          cp.bcs(this->_bcs);
          cp.p(p1);
          this->addParticle(cp);
          break;
        case Positron:
          P = particle(PDGtable.get("e"),1);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(Positron);
          cp.bcs(this->_bcs);
          cp.p(p1);
          this->addParticle(cp);
          break;
        case PiMinus:
          P = particle(PDGtable.get("pi"),-1);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(PiMinus);
          cp.bcs(this->_bcs);
          cp.p(p1);
          this->addParticle(cp);
          break;

        case Pi0:
          P = particle(PDGtable.get("pi0"),0);

          P.set4P(p1);
          this->addfinal(P);
          cp.pid(Pi0);
          cp.bcs(this->_bcs);
          cp.p(p1);
          this->addParticle(cp);
          break;
        case KPlus:
          P = particle(PDGtable.get("K"),1);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(KPlus);
          cp.bcs(this->_bcs);
          cp.p(p1);
          this->addParticle(cp);
          break;

        case KMinus:
          P = particle(PDGtable.get("K"),-1);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(KMinus);
          cp.bcs(this->_bcs);
          cp.p(p1);
          this->addParticle(cp);
          break;

        }


        }
      }
    }
      }
      else {
    if (this->_PART) {
      threeVec Pos;
      for (int k = 0; k < this->_PART->bank.nrow; ++k) {
        int sc_stat;
        fourVec p1;
        fourVec p2;
        particle P;
        clasParticle cp;
        part_t *part = &this->_PART->part[k];
        p1.set(part->p.t,threeVec(part->p.space.x,part->p.space.y,part->p.space.z));
        cp.qtrk(part->qtrk);
        cp.quality(part->qpid);
        cp.pidFlag(PART_FLAG);
        cp.setEvent(this);
        if (!this->_mc && this->_TBID) {
          cp.setTBID(&this->_TBID->tbid[part->trkid - 1]);
          if (part->trkid - 1 < this->_TBID->bank.nrow && part->trkid > 0)
        sc_stat = this->_TBID->tbid[part->trkid - 1].sc_stat;
          else
        sc_stat = -1000;
        }
        else {
          cp.setTBID(0);
        }
        switch((Particle_t) part->pid) {
        case Unknown:
          if (this->_useUnknown) {
        P = particle(PDGtable.get("p"),1);
        P.set4P(p1);
        Pos.set(part->vert.x,part->vert.y,part->vert.z);
        cp.pos(Pos);
        this->addfinal(P);
        cp.pid(Unknown);
        cp.tbid(part->trkid);
        if (!this->_mc) {
          cp.setTBID(&this->_TBID->tbid[part->trkid - 1]);
          cp.setTAGRindex(this->tagr_id());
        }
        cp.bcs(this->_bcs);
        cp.p(p1);
        cp.sec(_TBID ? _TBID->tbid[part->trkid - 1].sec : -1);
        if (sc_stat || this->_mc)
          this->addParticle(cp);
          }
          break;
        case Gamma:
          P = particle(PDGtable.get("gamma"),0);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(Gamma);
          if (!this->_mc) {
        cp.tbid(part->trkid);
        cp.setTBID(&this->_TBID->tbid[part->trkid - 1]);
        cp.setTAGRindex(this->tagr_id());
        cp.sec(_TBID ? _TBID->tbid[part->trkid - 1].sec : -1);
          }
          cp.bcs(this->_bcs);
          cp.p(p1);

          this->addParticle(cp);
          break;
        case Proton:
          P = particle(PDGtable.get("p"),1);
          P.set4P(p1);
          Pos.set(part->vert.x,part->vert.y,part->vert.z);
          cp.pos(Pos);
          this->addfinal(P);
          cp.pid(Proton);
          cp.tbid(part->trkid);
          //std::cerr << " TEST cp.tdid PROTON " <<  cp.tbid(part->trkid) <<std::endl;
          if (!this->_mc) {
        cp.setTBID(&this->_TBID->tbid[part->trkid - 1]);
        cp.setTAGRindex(this->tagr_id());
          }
          cp.bcs(this->_bcs);
          cp.p(p1);

          cp.sec(_TBID ? _TBID->tbid[part->trkid - 1].sec : -1);
          if (sc_stat || this->_mc)
        this->addParticle(cp);
          break;
        case Deuteron:
          P = particle(PDGtable.get("d"),1);
          P.set4P(p1);
          Pos.set(part->vert.x,part->vert.y,part->vert.z);
          cp.pos(Pos);
          this->addfinal(P);
          cp.pid(Deuteron);
          cp.tbid(part->trkid);
          if (!this->_mc) {
        cp.setTBID(&this->_TBID->tbid[part->trkid - 1]);
        cp.setTAGRindex(this->tagr_id());
          }
          cp.bcs(this->_bcs);
          cp.p(p1);
          cp.sec(_TBID ? _TBID->tbid[part->trkid - 1].sec : -1);
          if (sc_stat || this->_mc)
        this->addParticle(cp);
          break;

        case AntiProton:
          P = particle(PDGtable.get("pbar"),-1);
          P.set4P(p1);
          Pos.set(part->vert.x,part->vert.y,part->vert.z);
          cp.pos(Pos);
          this->addfinal(P);
          cp.pid(AntiProton);
          if (partbank) {
        cp.tbid(part->trkid);
        cp.setTBID(&this->_TBID->tbid[part->trkid - 1]);
        cp.setTAGRindex(this->tagr_id());
        cp.sec(_TBID ? _TBID->tbid[part->trkid - 1].sec : -1);
          }
          cp.bcs(this->_bcs);
          cp.p(p1);
          //          if (sc_stat || this->_mc)
          this->addParticle(cp);
          break;
        case Neutron:
          P = particle(PDGtable.get("n"),0);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(Neutron);
          cp.bcs(this->_bcs);
          if (partbank) {
        cp.tbid(part->trkid);
        cp.setTBID(&this->_TBID->tbid[part->trkid - 1]);
        cp.setTAGRindex(this->tagr_id());
        cp.sec(_TBID ? _TBID->tbid[part->trkid - 1].sec : -1);
          }
          cp.p(p1);

          this->addParticle(cp);
          break;
        case PiPlus:
          P = particle(PDGtable.get("pi"),1);
          P.set4P(p1);
          Pos.set(part->vert.x,part->vert.y,part->vert.z);
          cp.pos(Pos);
          this->addfinal(P);
          cp.pid(PiPlus);
          if (partbank) {
        cp.tbid(part->trkid);
        cp.setTBID(&this->_TBID->tbid[part->trkid - 1]);
        cp.setTAGRindex(this->tagr_id());
        cp.ccid(this->_TBID ? this->_TBID->tbid[part->trkid -1].cc_id : -1);
        cp.nPhe(cp.ccid() ? ((this->_CC01) ? this->_CC01->cc01[cp.ccid() - 1].n_pe : 0) : 0);
        //cp.nPhe(cp.ccadc() ? ((this->_CC) ? this->_CC->cc0[cp.ccid() - 1].adc : 0) : 0);
        cp.sec(_TBID ? _TBID->tbid[part->trkid - 1].sec : -1);
          }
          cp.bcs(this->_bcs);
          cp.p(p1);
          //          if (sc_stat || this->_mc)
          this->addParticle(cp);
          break;
        case Electron:
          P = particle(PDGtable.get("e"),-1);
          P.set4P(p1);
          Pos.set(part->vert.x,part->vert.y,part->vert.z);
          cp.pos(Pos);
          this->addfinal(P);
          cp.pid(Electron);
          if (partbank) {
        cp.tbid(part->trkid);
        cp.setTBID(&this->_TBID->tbid[part->trkid - 1]);
        cp.setTAGRindex(this->tagr_id());
        cp.ccid(this->_TBID ? this->_TBID->tbid[part->trkid -1].cc_id : -1);
        cp.nPhe(cp.ccid() ? ((this->_CC01) ? this->_CC01->cc01[cp.ccid() - 1].n_pe : 0) : 0);
        cp.sec(_TBID ? _TBID->tbid[part->trkid - 1].sec : -1);
          }
          cp.bcs(this->_bcs);
          cp.p(p1);


          //          if (sc_stat || this->_mc)
          this->addParticle(cp);
          break;
        case Positron:
          P = particle(PDGtable.get("e"),1);
          P.set4P(p1);
          Pos.set(part->vert.x,part->vert.y,part->vert.z);
          cp.pos(Pos);
          this->addfinal(P);
          cp.pid(Positron);
          if (partbank) {
        cp.tbid(part->trkid);
        cp.setTBID(&this->_TBID->tbid[part->trkid - 1]);
        cp.setTAGRindex(this->tagr_id());
        cp.ccid(this->_TBID ? this->_TBID->tbid[part->trkid -1].cc_id : -1);
        cp.nPhe(cp.ccid() ? ((this->_CC01) ? this->_CC01->cc01[cp.ccid() - 1].n_pe : 0) : 0);
        cp.sec(_TBID ? _TBID->tbid[part->trkid - 1].sec : -1);
          }
          cp.bcs(this->_bcs);
          cp.p(p1);
          //          if (sc_stat || this->_mc)
          this->addParticle(cp);
          break;
        case PiMinus:
          P = particle(PDGtable.get("pi"),-1);
          P.set4P(p1);
          Pos.set(part->vert.x,part->vert.y,part->vert.z);
          cp.pos(Pos);
          this->addfinal(P);
          cp.pid(PiMinus);
          if (partbank) {
        cp.tbid(part->trkid);
        cp.setTBID(&this->_TBID->tbid[part->trkid - 1]);
        cp.setTAGRindex(this->tagr_id());
        cp.ccid(this->_TBID ? this->_TBID->tbid[part->trkid -1].cc_id : -1);
        cp.nPhe(cp.ccid() ? ((this->_CC01) ? this->_CC01->cc01[cp.ccid() - 1].n_pe : 0) : 0);
        cp.sec(_TBID ? _TBID->tbid[part->trkid - 1].sec : -1);
          }
          cp.bcs(this->_bcs);
          cp.p(p1);
          //   if (sc_stat || this->_mc)
          this->addParticle(cp);
          break;
        case Pi0:
          P = particle(PDGtable.get("pi0"),0);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(Pi0);
          if (partbank) {
        cp.tbid(part->trkid);
        cp.setTBID(&this->_TBID->tbid[part->trkid - 1]);
        cp.setTAGRindex(this->tagr_id());
        cp.sec(_TBID ? _TBID->tbid[part->trkid - 1].sec : -1);
          }
          cp.bcs(this->_bcs);
          cp.p(p1);

          this->addParticle(cp);
          break;
        case KPlus:
          P = particle(PDGtable.get("K"),1);
          P.set4P(p1);
          this->addfinal(P);
          cp.pid(KPlus);
          if (partbank) {
        cp.tbid(part->trkid);
        cp.setTBID(&this->_TBID->tbid[part->trkid - 1]);
        cp.setTAGRindex(this->tagr_id());
        cp.ccid(this->_TBID ? this->_TBID->tbid[part->trkid -1].cc_id : -1);
        cp.nPhe(cp.ccid() ? ((this->_CC01) ? this->_CC01->cc01[cp.ccid() - 1].n_pe : 0) : 0);
        cp.sec(_TBID ? _TBID->tbid[part->trkid - 1].sec : -1);
          }
          cp.bcs(this->_bcs);
          cp.p(p1);
          //          if (sc_stat || this->_mc)
          this->addParticle(cp);
          break;
        case KMinus:
          P = particle(PDGtable.get("K"),-1);
          P.set4P(p1);
          Pos.set(part->vert.x,part->vert.y,part->vert.z);
          cp.pos(Pos);
          this->addfinal(P);
          cp.pid(KMinus);
          if (partbank) {
        cp.tbid(part->trkid);
        cp.setTBID(&this->_TBID->tbid[part->trkid - 1]);
        cp.setTAGRindex(this->tagr_id());
        cp.ccid(this->_TBID ? this->_TBID->tbid[part->trkid -1].cc_id : -1);
        cp.nPhe(cp.ccid() ? ((this->_CC01) ? this->_CC01->cc01[cp.ccid() - 1].n_pe : 0) : 0);
        cp.sec(_TBID ? _TBID->tbid[part->trkid - 1].sec : -1);
          }
          cp.bcs(this->_bcs);
          cp.p(p1);
          //          if(sc_stat || this->_mc)
          this->addParticle(cp);
          break;
        }
      }
    }
      }
      // Now create the tagged photon list

      //  tagr_t *tbase = this->_tagr = get_photon_tagr(TAGR,(clasBID_t *)this->_TBID, TIME_BASED);
      if(TAGR){
    tagr_t *tbase = this->_tagr;
    if (tbase) {
      for(int j=0;j<TAGR->bank.nrow;j++){
        if (fabs(tbase->tpho - TAGR->tagr[j].tpho) < 1.0) {
          clasTaggedPhoton tag(&TAGR->tagr[j]);
          this->addTaggedPhoton(tag);
        }
      }
    }
      }
    }

    else{
      ret =0;
    }
  }

  return(ret);

}


list<clasTaggedPhoton> clasEvent::createTaggedPhoton(double t1,double t2)
{
  this->_tagWindow.clear();

  if(this->_TAGR){
    tagr_t *tbase = this->_tagr;
    if (tbase) {
      for(int j=0;j<this->_TAGR->bank.nrow;j++){
    if (this->_TAGR->tagr[j].tpho > t1 && this->_TAGR->tagr[j].tpho < t2) {
      clasTaggedPhoton tag(&this->_TAGR->tagr[j]);
      this->addTaggedPhotonWindow(tag);
    }
      }
    }
  }
  return(this->_tagWindow);
}


list<clasTaggedPhoton> clasEvent::createTaggedPhoton(double t)
{
  this->_tagWindow.clear();
  if(this->_TAGR){
    clasTAGR_t *TAGR = this->_TAGR;
    tagr_t *tbase = this->_tagr;
    if (tbase) {
      for(int j=0;j<TAGR->bank.nrow;j++){
    if (fabs(tbase->tpho - TAGR->tagr[j].tpho) < t) {
      clasTaggedPhoton tag(&TAGR->tagr[j]);
      this->addTaggedPhotonWindow(tag);
    }
      }
    }
  }
  return(this->_tagWindow);
}



// write



void clasTaggedPhoton::write(ostream& os)
{
  if (this->_tagr) {
    os << "Tagged photon:\tE: " << this->E() << "\tttag: " << this->ttag() << "\ttpho: " << this->tpho() << "\tstatus: " << this->stat() << "\ttID: " << this->t_id() << "\teID: " << this->e_id() << endl;
  }

}

void clasParticle::write(ostream& os)
{
  os << "\tParticle: " << particleName(this->pid()) << " (" << this->pid() << ") " << endl;
  os << "\t\tMomentum (E,px,py,pz):\t" << this->p().t() << " " << this->p().x() << " " << this->p().y() << " " << this->p().z() << endl;
  os << "\t\tPosition (x,y,z):\t" << this->x() << " " << this->y() << " " << this->z() << endl;
  os << "\t\tBeta (p/E):\t" << this->Beta() << " beta (tof):\t" << this->beta() << endl;
  os << "\t\tST->SC\t" << this->pathlenST_SC() << " cm\t" << "SC len: " << this->sc_len() << " ST len: " << this->st_len() << endl;


}



void clasEvent::write(ostream& os)
{
  clasTaggedPhoton tag(this->_tagr);
  os << "Dump of event " << this->run() << " " << this->event() << " ";
  if (this->gpid())
    os << "\tGPID ";
  else if (this->seb())
    os << "\tSEB ";
  else
    os << "\tPID ";
  if (this->mc())
    os << "\tMonte Carlo ";

  os << "\tweight: " << this->weight() << " ";
  os <<  "\tcode: " << this->eventCode() << " ";
  os << endl;

  tag.write(os);


  os << "Vertex time: " << this->vtime();
  os << endl;

  os << "\n" << this->Ncp() << " particles" << endl;
  for (int i = 0; i < this->Ncp(); ++i) {
    clasParticle cp = this->cp(i + 1);
    cp.write(os);
  }

  os.flush();

}



void clasEvent::fitPart(int bankNo,clasKineFit &Fit)
{

  // Get the output list of clasParticles

  std::list<clasParticle> newCP = Fit.cpOut();
  std::list<clasParticle>::iterator p = newCP.begin();
  int N = newCP.size();
  int i = 0;
  part_t *part;

  clasPART_t *newPART = (clasPART_t *)makeBank(this->_bcs,"PART",bankNo,sizeof(part_t)/sizeof(int),N + 1);

  part = &newPART->part[0];
  part->vert.x = this->x();
  part->vert.y = this->y();
  part->vert.z = this->z();
  part->p.space.x = 0.0;
  part->p.space.y = 0.0;
  part->p.space.z = Fit.beamEnergy();
  part->p.t = Fit.beamEnergy();
  part->qpid = Fit.prob();
  part->flags = BEAM_FLAG;
  part->pid = Gamma;
  i++;

  while (p != newCP.end() ) {
   part_t *part = &newPART->part[i];
   part->vert.x = p->x();
   part->vert.y = p->y();
   part->vert.z = p->z();
   part->p.space.x = p->p().V().x();
   part->p.space.y = p->p().V().y();
   part->p.space.z = p->p().V().z();
   part->p.t = p->p().t();
   part->q = (float) p->Q();
   part->pid = p->pid();
   part->trkid = p->tbidIndex();
   part->qtrk = p->qtrk();
   part->qpid = p->quality();
   i++;
    p++;
  }



}

clasParticle clasParticle::reAssign(Particle_t pid)
{

  threeVec p = this->_p.V();
  double e,m;
  this->_pid = pid;
  m = this->pdgMass();
  e = sqrt(m*m + p.lenSq());
  this->_p = fourVec(e,p);
  return(*this);
}



threeVec clasParticle::CCdir()
{
  threeVec ret(-1000.,-1000.,-1000.);
  clasTDPL_t *TDPL = (clasTDPL_t *) getGroup(this->_bcs,"TDPL",this->sec());

  if (this->tbidptr() && TDPL) {
    int track = this->tbidptr()->track;
    for (int j = 0; j < TDPL->bank.nrow; ++j) {
      tdpl_t *tdpl = &TDPL->tdpl[j];
      for (int i = NCC_MIN; i <= NCC_MAX; ++i) {
    if ((track == tdpl->trk_pln/100) && (i + 1) == (tdpl->trk_pln % 100)) {
      ret.set(tdpl->dir.x,tdpl->dir.y,tdpl->dir.z);
    }
      }
    }

  }
  return(ret);
}


threeVec clasParticle::CCpos()
{
  threeVec ret(-1000.,-1000.,-1000.);
  clasTDPL_t *TDPL = (clasTDPL_t *) getGroup(this->_bcs,"TDPL",this->sec());

  if (this->tbidptr() && TDPL) {
    int track = this->tbidptr()->track;
    for (int j = 0; j < TDPL->bank.nrow; ++j) {
      tdpl_t *tdpl = &TDPL->tdpl[j];
      for (int i = NCC_MIN; i <= NCC_MAX; ++i) {
    if ((track == tdpl->trk_pln/100) && (i + 1) == (tdpl->trk_pln % 100)) {
      ret.set(tdpl->pos.x,tdpl->pos.y,tdpl->pos.z);
    }
      }
    }

  }
  return(ret);
}



double clasParticle::tpho()
{
 if (this->_pidflag == GPID_FLAG) {
   clasGPID_t *GPID = (clasGPID_t *) getBank(this->_bcs,"GPID");
   return(GPID ? GPID->gpid[this->_gpidIndex].tpho : -1000);
 }
 else {
   double ret = -1000.0;
   clasTAGR_t *TAGR = (clasTAGR_t *) getBank(this->_bcs,"TAGR");
   if (TAGR) {
     ret  = TAGR->tagr[this->tagr_id() - 1].tpho;
   }
   return (ret);
 }


}
int clasParticle::st_stat()
{
  if (this->_pidflag == GPID_FLAG) {
    clasGPID_t *GPID = (clasGPID_t *) getBank(this->_bcs,"GPID");
    return(GPID ? GPID->gpid[this->_gpidIndex].st_stat : -1000);
  }
  else {

    clasTBID_t *TBID = (clasTBID_t *)getBank(this->_bcs,"TBID");
    if (TBID) {
      tbid_t *tbid = &TBID->tbid[this->_tbidIndex -1];
      return(tbid->st_stat);
    }
    else
      return (-1000);
  }
}

double clasParticle::st_len()
{
  if (this->_pidflag == GPID_FLAG) {
    clasGPID_t *GPID = (clasGPID_t *) getBank(this->_bcs,"GPID");
    return(GPID ? GPID->gpid[this->_gpidIndex].st_len : -1000.);
  }
  else {

    clasTBID_t *TBID = (clasTBID_t *)getBank(this->_bcs,"TBID");
    if (TBID) {
      tbid_t *tbid = &TBID->tbid[this->_tbidIndex -1];
      return(-1000.0);
    }
    else
      return (-1000.);
  }
}
int clasParticle::sc_stat()
{
  if (this->_pidflag == GPID_FLAG) {
    clasGPID_t *GPID = (clasGPID_t *) getBank(this->_bcs,"GPID");
    return(GPID ? GPID->gpid[this->_gpidIndex].sc_stat : -1000);
  }
  else {

    clasTBID_t *TBID = (clasTBID_t *)getBank(this->_bcs,"TBID");
    if (TBID) {
      tbid_t *tbid = &TBID->tbid[this->_tbidIndex -1];
      return(tbid->sc_stat);
    }
    else
      return (-1000);
  }
}

double clasParticle::ttag()
{
  double ret = -1000.0;
  clasTAGR_t *TAGR = (clasTAGR_t *) getBank(this->_bcs,"TAGR");
  if (TAGR) {
    ret  = TAGR->tagr[this->tagr_id() - 1].ttag;
  }
  return (ret);
}




int clasParticle::nGammaRF()
{
 if (this->_pidflag == GPID_FLAG) {
   clasGPID_t *GPID = (clasGPID_t *) getBank(this->_bcs,"GPID");
   return(GPID ? GPID->gpid[this->_gpidIndex].ngrf : -1000);
 }
 else {
   int ngamma = 0;
   clasTAGR_t *TAGR = (clasTAGR_t *) getBank(this->_bcs,"TAGR");
   if (TAGR  && this->_ev->tagr()) {
     double tpho = TAGR->tagr[this->tagr_id() - 1].tpho;
     for (int i = 0; i < TAGR->bank.nrow; ++i) {
       if (fabs(TAGR->tagr[i].tpho - tpho) < 0.3)
     ngamma++;
     }
   }
   return (ngamma);
 }


}

int clasParticle::tagr_id() const
{
 if (this->_pidflag == GPID_FLAG) {
   clasGPID_t *GPID = (clasGPID_t *) getBank(this->_bcs,"GPID");
   return(GPID ? GPID->gpid[this->_gpidIndex].tagrid : -1000);
 }
 else {
   return (this->_tagrIndex + 1);
 }


}


// ------------------------------ timing ----------------------------------------


double clasEvent::vtime()
{
  double ret = -1000.0;
  if (this->seb()) {
    clasTGPB_t *TGPB = (clasTGPB_t *)this->getEvtBank("TGPB");
    if (TGPB)
      ret = TGPB->tgpb[0].time;
  }
  else
    ret = this->_TBID ? this->_TBID->tbid[0].vtime: -1000;
  return(ret);
}

double clasEvent::stVtime()
{
  double ret = -1000.0;
  if (this->seb()) {
    clasTGPB_t *TGPB = (clasTGPB_t *)this->getEvtBank("TGPB");
    if (TGPB)
      ret = TGPB->tgpb[0].dt + TGPB->tgpb[0].time;
  }
  else
    ret = this->_The_tbid ? this->_The_tbid->st_vtime : -1000.0;
  return(ret);
}


// ---------------------------------------------------------------------------------


  // building banks

void clasEvent::ConfigTAGGER()
{
  int run = this->run();
  tagtcl_set_def_();
  tag_init_();
  tag_brun_(&run);
  this->_configTAGGER = run;
  tagM_init();
  tagM_brun(run);

}

void clasEvent::ConfigTGEO()
{
  make_TGEO(this->run());
  this->_configTGEO = this->run();

}

void clasEvent::makeTAGM()
{
  string s("TAGM");
  bankList(&bcs_, "E+", "TAGM");
  this->_newBanks.push_back(s);
  if (!this->configTAGGERStatus()) {
    this->ConfigTAGGER();
  }
  tagM_evt();
  this->_TAGM= (clasTAGM_t *) getBank(this->_bcs,"TAGM");


}

void clasEvent::ConfigSwim()
{
  dc_set_def_();
  trk_set_def_();
  setPrlinkx(this->prlink());
  dc_begin_run(this->run());
  this->_configSWIM = this->run();
}

string clasEvent::defaultPrlink()
{

  double torus = this->torusCurrent();
  double minitorus = this->minitorusCurrent();

  switch (this->_runPeriod) {
  case g11a:
    if (torus > 1900.0 && torus < 2100.0) {
      return(string("prlink_g11_1920.bos"));
    }
    else
      return(string("prlink_g11_2250"));
    break;
  case g12:
    return(string("prlink_g12_1930"));
  default:
    return(string(prlinkFile((float) torus,(float)minitorus)));
  }



}


void clasEvent::ConfigSC()
{
  if (this->run() != this->_configSC) {
    sc_begin_run(this->run());
    this->_configSC = this->run();
  }

}
void clasEvent::ConfigSCG()
{
  if (this->run() != this->_configSCG) {
    make_SCG_banks(this->run());
    this->_configSCG = this->run();
  }

}

void clasEvent::ConfigCC()
{
  int run = this->run();
  cc_brun_(&run);
  cc_begin_run(this->run());
  this->_configCC = this->run();

}


void clasEvent::ConfigEC()
{
  ec_set_def_();
  ec_begin_run(this->run());
  this->_configEC = this->run();

}

void clasEvent::ConfigRunControl()
{
  int run = this->run();
  if (this->configRunControlStatus() != run) {
    make_RUNC_bank(run);
    this->_configRunControl = run;
  }
}

double clasEvent::torusCurrent()
{
  double ret;
  clasRUNC_t *RUNC;
  if (this->configRunControlStatus() != this->run()) {
    make_RUNC_bank(this->run());
    this->_configRunControl = this->run();
  }
 if (RUNC = (clasRUNC_t *)getBank(&wcs_, "RUNC")) {
   ret = RUNC->runc.torus.val.f[0];
 }
 else
   ret = 0.0;
 return(ret);
}

double clasEvent::minitorusCurrent()
{
  double ret;
  clasRUNC_t *RUNC;
  if (this->configRunControlStatus() != this->run()) {
    make_RUNC_bank(this->run());
    this->_configRunControl = this->run();
  }
 if (RUNC = (clasRUNC_t *)getBank(&wcs_, "RUNC")) {
   ret = RUNC->runc.minitorus.val.f[0];
 }
 else
   ret = 0.0;
 return(ret);
}

void clasEvent::buildSwim(int Sector1)
{
  clasRUNC_t *RUNC = (clasRUNC_t *)getBank(&wcs_, "RUNC");
  if (!RUNC) {
    make_RUNC_bank(this->run());
  }
  if (!(this->_configSC)) {
    this->ConfigSC();
  }

  if (!this->_configSWIM ) {
     this->ConfigSwim();
  }

  trk_remake_swim_banks(Sector1,.005,.1,10,0,1,1,1);
}

int clasEvent::RUNC_BeamType()
{

  int ret = -99; // initial value set to nonsense

  clasRUNC_t *RUNC = (clasRUNC_t *)getBank(&wcs_, "RUNC");
  if (!RUNC) {
    make_RUNC_bank(this->run());
    RUNC = (clasRUNC_t *)getBank(&wcs_, "RUNC");
  }
  if (RUNC){
    ret = RUNC->runc.beam.type.val.i[0];
  }
  return(ret);
}

void clasEvent::buildTBID(int bankNo) // M.K. "=1" is removed
{
  int trk_level=TIME_BASED;
  clasRUNC_t *RUNC = (clasRUNC_t *)getBank(&wcs_, "RUNC");
  if (!RUNC) {
    make_RUNC_bank(this->run());
    RUNC = (clasRUNC_t *)getBank(&wcs_, "RUNC");
  }

  if (!this->_configTBID ) {
    ConfigEvent(this->run(),1);
    this->_configTBID = this->run();
  }

  dropAllBanks(&bcs_, "SC1 SCR SCRCCC01");
  for(int sec=1; sec <= 6; sec++) {
    make_SC1_bank(sec);
    make_SCR_bank(sec,"TDPL");
    make_SCRC_bank(sec);
    make_CC01_bank(sec);
  }
  dropAllBanks(&bcs_,"ECHBECPI");
  ec_evnt_();
  /* large angle calorimeter stuff */
  dropAllBanks(&bcs_,"EC1R");
  ec1_evnt_();

  if (RUNC){
    if (RUNC->runc.beam.type.val.i[0] == 1){
      //      dropAllBanks(&bcs_, "TAGITAGR");
      //      tag_evnt_();
      dropAllBanks(&bcs_, "ST1 STR ");
      st_bevt_();
      st_evnt_(&trk_level);
    }
  }

  //  make_BID_banks(bankNo);
  dropBank(this->_bcs, "TBID", bankNo);
  make_TBID_group(bankNo);
  make_PART_group(bankNo);
  this->reload(bankNo);
}

int clasEvent::_init(char *file,BOSbank *b,int partbank,int RawMode)
{
  int ret;

  // Initialization flags
  this->_configTGEO = 0;
  this->_configTBID = 0;
  this->_configPcorr = 0;
  this->_configEloss = 0;
//  this->_configg1cPcorrection = 0;
  this->_configPcor = 0;
  this->_configg10pcor = 0;
  this->_configMomentumCorrection = 0;
  this->_configEC = 0;
  this->_configSC = 0;
  this->_configSCG = 0;
  this->_configCC = 0;
  this->_configTAGGER = 0;
  this->_configSWIM = 0;
  this->_configRunControl = 0;
  this->_useGPID = this->_useSEB = 0;
  this->_mc = 0;

  this->_TAGR = NULL;
  this->_TAGM = NULL;

  this->_initial.clear();
  this->_final.clear();
  this->_partbank = partbank;
  this->_cp.clear();
  this->_bcs = b;
  this->_RawMode = RawMode;
  ret = RawMode ? initProcess(file,10) : initDisplay(file,10);
  this->_status = ret ? 1 : 0;
  this->_verbose = 0;
  this->_file = ret;
  return(ret);
}
int clasEvent::_init(BOSbank *b,int partbank,int RawMode)
{
  int ret = 1;
  // Initialization flags
  this->_configTGEO = 0;
  this->_configTBID = 0;
  this->_configPcorr = 0;
  this->_configEloss = 0;
//  this->_configg1cPcorrection = 0;
  this->_configPcor = 0;
  this->_configg10pcor = 0;
  this->_configMomentumCorrection = 0;
  this->_configEC = 0;
  this->_configSC = 0;
  this->_configSCG = 0;
  this->_configCC = 0;
  this->_configTAGGER = 0;
  this->_configSWIM = 0;
  this->_configRunControl = 0;
  this->_useGPID = this->_useSEB = 0;
  this->_mc = 0;

  this->_TAGR = NULL;
  this->_TAGM = NULL;



  this->_file = 0;
  this->_useGPID = 0;
  this->_useSEB = 0;
  this->_mc = (partbank == 0);
  this->_verbose = 0;
  this->_partbank = partbank;
  this->_initial.clear();
  this->_final.clear();
  this->_cp.clear();
  this->_tag.clear();
  this->_bcs = b;
  this->_RawMode = RawMode;
  return(ret);
}
int clasEvent::_init(BOSbank *b,int RawMode)
{
  int ret = 1;


  // Initialization flags
  this->_configTGEO = 0;
  this->_configTBID = 0;
  this->_configPcorr = 0;
  this->_configEloss = 0;
//  this->_configg1cPcorrection = 0;
  this->_configPcor = 0;
  this->_configg10pcor = 0;
  this->_configMomentumCorrection = 0;
  this->_configEC = 0;
  this->_configSC = 0;
  this->_configSCG = 0;
  this->_configCC = 0;
  this->_configTAGGER = 0;
  this->_configSWIM = 0;
  this->_configRunControl = 0;
  this->_useGPID = this->_useSEB = 0;
  this->_mc = 0;


  this->_file = 0;
  this->_useGPID = 0;
  this->_useSEB = 0;
  this->_partbank = 0;
  this->_initial.clear();
  this->_verbose = 0;
  this->_final.clear();
  this->_cp.clear();
  this->_tag.clear();
  this->_bcs = b;
  this->_RawMode = RawMode;
  return(ret);
}


int clasEvent::read()
{
  int ret = getData(this->_bcs,"E");
  clasHEAD_t *HEAD = (clasHEAD_t *)getBank(this->_bcs,"HEAD");
  if (HEAD)
    this->_head = &HEAD->head[0];
  else
    this->_head = NULL;
  return(1);
}

int clasEvent::read(int partbank)
{
  int ret = getData(this->_bcs,"E");
  clasHEAD_t *HEAD = (clasHEAD_t *)getBank(this->_bcs,"HEAD");
  if (HEAD)
    this->_head = &HEAD->head[0];
  else
    this->_head = NULL;
  if (ret){
    if (this->load(partbank)==0) ret=-1;
  }
  return(ret);
}

int clasEvent::evtClass()
{
  int ret = 0;
  clasHEAD_t *HEAD = (clasHEAD_t *)getBank(this->_bcs,"HEAD");
  if (HEAD) {
    ret = HEAD->head[0].evtclass;
  }
  return(ret);
}
int clasEvent::run()
{
  int ret = 0;
  clasHEAD_t *HEAD = (clasHEAD_t *)getBank(this->_bcs,"HEAD");
  if (HEAD) {
    ret = HEAD->head[0].nrun;
  }
  return(ret);
}
int clasEvent::event()
{
  int ret = 0;
  clasHEAD_t *HEAD = (clasHEAD_t *)getBank(this->_bcs,"HEAD");
  if (HEAD) {
    ret = HEAD->head[0].nevent;
  }
  return(ret);
}
int clasEvent::type()
{
  int ret = 0;
  clasHEAD_t *HEAD = (clasHEAD_t *)getBank(this->_bcs,"HEAD");
  if (HEAD) {
    ret = HEAD->head[0].type;
  }
  return(ret);
}
int clasEvent::latch()
{
  int ret = 0;
  clasTGBI_t *TGBI = (clasTGBI_t *)getBank(this->_bcs,"TGBI");
  if (TGBI) {
    ret = TGBI->tgbi[0].latch1;
  }
  return(ret);
}


int clasEvent::trig()
{
  int ret = 0;
  clasHEAD_t *HEAD = (clasHEAD_t *)getBank(this->_bcs,"HEAD");
  if (HEAD) {
    ret = HEAD->head[0].trigbits & 0xff;
  }
  return(ret);
}

uint32 clasEvent::latch2()
{
  int ret = 0;
  clasTGBI_t *TGBI = (clasTGBI_t *)getBank(this->_bcs,"TGBI");
  if (TGBI) {
    ret = TGBI->tgbi[0].latch2;
  }
  return(ret);
}
uint32 clasEvent::latch1()
{
  int ret = 0;
  clasTGBI_t *TGBI = (clasTGBI_t *)getBank(this->_bcs,"TGBI");
  if (TGBI) {
    ret = TGBI->tgbi[0].latch1;
  }
  return(ret);
}

double clasEvent::weight()
{
  double ret = -1000.0;
  clasPART_t *PART0 = (clasPART_t *)this->getEvtBank("PART",0);
  if (PART0) {
    ret = PART0->part[0].qtrk;
  }
  return(ret);
}

int clasEvent::eventCode()
{
  int ret = -1000;
  clasPART_t *PART0 = (clasPART_t *)this->getEvtBank("PART",0);
  if (PART0) {
    ret = PART0->part[0].flags;
  }
  return(ret);
}

Particle_t clasEvent::partID(int partIndex)
{
  Particle_t  ret = Unknown;
  if (this->_PART) {
    return((Particle_t) this->_PART->part[partIndex].pid);
  }
    return(ret);
}

int clasEvent::nTagged(double t0,double t1,double E0,double E1)
{
  int nTag = 0;

  if (this->_TAGR) {
    for (int i = 0; i < this->_TAGR->bank.nrow; ++i) {
      tagr_t *tagr = &this->_TAGR->tagr[i];
      if (tagr->tpho < t1 && tagr->tpho > t0 && tagr->erg < E1 && tagr->erg > E0)
    nTag++;
    }
  }
  return(nTag);

}

int clasEvent::nTracks()
{
  int nTrks = 0;
  if (this->_TBTR) {
    nTrks = this->_TBTR->bank.nrow;
  }
  return(nTrks);
}

int clasEvent::N()
{
  return(this->_cp.size());
}

nuclear_t clasEvent::g7Target()
{
  nuclear_t ret = UNKNOWN;
  threeVec v = this->V();
  double r = sqrt(v.x()*v.x() + v.y() * v.y());
  if (r < G7A_R_LIM) {
    if (v.z() < -14.0  && v.z() > -22.0) {
      ret = D2;
    }
    else if (v.z() < -8.5 && v.z() > -11.0) {
      ret = IRON;
    }

    else if (v.z() < 1.5 && v.z() > -1.0) {
      ret = TITANIUM;
    }

    else if (v.z() < -3.5 && v.z() > -6.0) {
      ret = LEAD;
    }

    else if (( v.z() < -11.0 && v.z() > -14.0) || ( v.z() < -6.0 && v.z() > -8.5) || ( v.z() < -1.0 && v.z() > -3.5) || ( v.z() < 4.0 && v.z() > 1.5) ) {
      ret = CARBON;
    }
    else
      ret = UNKNOWN;

  }


  return(ret);

}

int clasEvent::nIsLepton()
{
  int ret = 0;
  std::list<clasParticle>::iterator p = this->_cp.begin();
  while (p != this->_cp.end() ) {
    if (p->IsLepton())
      ret++;
    p++;
  }
  return(ret);
}

int clasEvent::nIsLepton(int q)
{
  int ret = 0;
  std::list<clasParticle>::iterator p = this->_cp.begin();
  while (p != this->_cp.end() ) {
    if (p->Q() == q) {
      if (p->IsLepton())
    ret++;
    }
    p++;
  }
  return(ret);
}

int clasEvent::IndexIsLepton()
{
  int i;
  int ret = 0;
  int NLep, iLepEC, iLepCC, iLepTOF;

  NLep = this->nIsLepton();
  if(NLep){
    for(i=0;i<NLep;i++){
      iLepEC = this->cpLepton(i+1).IsLepton_ECtot();
      iLepCC = this->cpLepton(i+1).IsLepton_CC();
      iLepTOF = this->cpLepton(i+1).IsLepton_MM2();
      // if(!(iLepEC && iLepCC) && iLepTOF){
      if(iLepEC && iLepCC){
    ret = i+1;
    break;
      }
    }
  }
  return(ret);
}

int clasEvent::IndexIsLepton(int q)
{
  int i;
  int ret = 0;
  int NLep, iLepEC, iLepCC, iLepTOF;

  NLep = this->nIsLepton(q);
  if(NLep){
    for(i=0;i<NLep;i++){
      iLepEC = this->cpLepton(q,i+1).IsLepton_ECtot();
      iLepCC = this->cpLepton(q,i+1).IsLepton_CC();
      iLepTOF = this->cpLepton(q,i+1).IsLepton_MM2();
      // if(!(iLepEC && iLepCC) && iLepTOF){
      if(iLepEC && iLepCC){
    ret = i+1;
    break;
      }
    }
  }
  return(ret);
}

clasParticle clasEvent::cpLepton(int index)
{
  clasParticle ret;
  int n = 0;
  std::list<clasParticle>::iterator p = this->_cp.begin();
  while ( p != this->_cp.end() ) {
    if (p->IsLepton()) {
      n++; // we count from 1
      if (n == index) {
    threeVec mom = p->p().V();
    double energy = sqrt(mom.lenSq()+ ELECTRON_MASS*ELECTRON_MASS);
    ret = *p;
    ret._p.t(energy);
    return(ret);
      }
    }
    p++;
  }
  throw((char *)"ParticleNotFound");
  return(ret);
}

clasParticle clasEvent::cpLepton(int q,int index)
{
  clasParticle ret;
  int n = 0;
  std::list<clasParticle>::iterator p = this->_cp.begin();
  while ( p != this->_cp.end() ) {
    if (p->Q() == q) {
      if (p->IsLepton()) {
    n++; // we count from 1
    if (n == index) {
      threeVec mom = p->p().V();
      double energy = sqrt(mom.lenSq()+ ELECTRON_MASS*ELECTRON_MASS);
      ret = *p;
      ret._p.t(energy);
      return(ret);
    }
      }
    }
    p++;
  }
  throw((char *)"ParticleNotFound");
  return(ret);
}

int clasEvent::nMaybeLepton()
{
  int ret = 0;
  std::list<clasParticle>::iterator p = this->_cp.begin();
  while (p != this->_cp.end() ) {
    if (p->MaybeLepton()) ret++;
    p++;
  }
  return(ret);
}
int clasEvent::nMaybeLepton(int q)
{
  int ret = 0;
  std::list<clasParticle>::iterator p = this->_cp.begin();
  while (p != this->_cp.end() ) {
    if ((p->Q() == q) && (p->MaybeLepton())) ret++;
    p++;
  }
  return(ret);
}

clasParticle clasEvent::cpMaybeLepton(int index)
{
  clasParticle ret;
  int n = 0;
  std::list<clasParticle>::iterator p = this->_cp.begin();
  while ( p != this->_cp.end() ) {
    if (p->MaybeLepton()) {
      n++; // we count from 1
      if (n == index) {
    ret = *p;
    return(ret);
      }
    }
    p++;
  }
  throw((char *)"ParticleNotFound");
  return(ret);
}

clasParticle clasEvent::cpMaybeLepton(int q,int index)
{
  clasParticle ret;
  int n = 0;
  std::list<clasParticle>::iterator p = this->_cp.begin();
  while ( p != this->_cp.end() ) {
    if (p->Q() == q) {
      if (p->MaybeLepton()) {
    n++; // we count from 1
    if (n == index) {
      ret = *p;
      return(ret);
    }
      }
    }
    p++;
  }
  throw((char *)"ParticleNotFound");
  return(ret);
}

int clasEvent::N(Particle_t pid)
{
  int ret = 0;
  std::list<clasParticle>::iterator p = this->_cp.begin();
  while (p != this->_cp.end() ) {
    if (p->pid() == pid)
      ret++;
    p++;
  }
  return(ret);
}

int clasEvent::triggerMask(unsigned int t)
{
  int ret = 0;
  if (this->_head) {
    ret = t ? (t & this->_head->trigbits) : 1;
  }
  return(ret);
}

fourVec clasEvent::getPfinal(Particle_t pid,int index)
{
  return(this->getPartPFinal(id2name((Geant_ID) pid),Q(pid),index));
}

double clasEvent::MMsq()
{
  fourVec total(0.0,threeVec(0.0,0.0,0.0));
  fourVec initial(0.0,threeVec(0.0,0.0,0.0));

  Particle_t all[] = {Proton,Neutron,PiPlus,PiMinus,Pi0,KPlus,KMinus};

  int nP = 6;

  for (int i = 0; i < nP; ++i) {
    int k = this->N(all[i]);
    for (int j = 0; j < k; ++j) {
      fourVec P = getPfinal(all[i],j + 1);
      total += P;
    }
  }

  initial = this->beam().get4P() + this->target().get4P();
  return((initial - total).lenSq());



}


void clasEvent::convertNeutrons(int partbank0)
{
  clasPART_t *PART = (clasPART_t *)getGroup(this->_bcs,"PART",partbank0);
  clasECHB_t *ECHB = (clasECHB_t *)getBank(this->_bcs,"ECHB");
  threeVec v(0.0,0.0,0.0);
  fourVec p;
  part_t *part;
  echb_t *echb;
  tbid_t *tbid;
  if (PART && this->_TBID && ECHB) {
    for (int k = 0; k < PART->bank.nrow; ++k) {
      part = &PART->part[k];
      switch ((Particle_t) part->pid) {
      case Neutron:
    tbid = &this->_TBID->tbid[part->trkid - 1];
    if ((tbid->ec_id > 0) && tbid->ec_stat) {
      echb = &ECHB->echb[tbid->ec_id - 1];
      part->pid = (Particle_t) Gamma;
      p = echb2partMVRT(echb,NEUTRON_MASS,v);
      part->p.t = p.t();
      part->p.space.x = p.x();
      part->p.space.y = p.y();
      part->p.space.z = p.z();
    }
    break;
      case Gamma:
    break;
      }
    }
  }
}

double clasEvent::vert2Separation(clasParticle &p1,clasParticle &p2)
{
  clasVERT_t *VERT = (clasVERT_t *) getGroup(this->_bcs,"VERT",2);
  if (VERT) {
    for (int v = 0; v < VERT->bank.nrow; ++v) {
      vert_t *vert = &VERT->vert[v];
      if ((vert->trk1 == p1.trackNo() && vert->trk2 == p2.trackNo()) ||
      (vert->trk1 == p2.trackNo() && vert->trk2 == p1.trackNo())) {
    double  ret(vert->sepd);
    return(ret);
    break;
      }
    }
  }
  return(-1000.0);
}

double clasEvent::vert1Separation(clasParticle &p1)
{
  clasVERT_t *VERT = (clasVERT_t *) getGroup(this->_bcs,"VERT",1);
  if (VERT) {
    for (int v = 0; v < VERT->bank.nrow; ++v) {
      vert_t *vert = &VERT->vert[v];
      if (vert->trk2 == p1.trackNo()) {
    double ret(vert->sepd);
    return(ret);
    break;
      }
    }
  }
  return(-1000.0);
}


threeVec clasEvent::vert2(clasParticle &p1,clasParticle &p2)
{
  threeVec zero(-1000.0,-1000.0,-1000.0);
  clasVERT_t *VERT = (clasVERT_t *) getGroup(this->_bcs,"VERT",2);
  if (!VERT) {
    this->makeVertex();
    VERT = (clasVERT_t *) getGroup(this->_bcs,"VERT",2);
  }
  if (VERT) {
    for (int v = 0; v < VERT->bank.nrow; ++v) {
      vert_t *vert = &VERT->vert[v];
      if ((vert->trk1 == p1.trackNo() && vert->trk2 == p2.trackNo()) ||
      (vert->trk1 == p2.trackNo() && vert->trk2 == p1.trackNo())) {
        threeVec ret(vert->vert.x,vert->vert.y,vert->vert.z);
        return(ret);
        break;
      }
    }
  }
  return(zero);
}

threeVec clasEvent::vert1(clasParticle &p1)
{
  threeVec zero(-1000.0,-1000.0,-1000.0);
  clasVERT_t *VERT = (clasVERT_t *) getGroup(this->_bcs,"VERT",1);
  if (!VERT) {
    this->makeVertex();
    VERT = (clasVERT_t *) getGroup(this->_bcs,"VERT",1);
  }
  if (VERT) {
    for (int v = 0; v < VERT->bank.nrow; ++v) {
      vert_t *vert = &VERT->vert[v];
      if (vert->trk2 == p1.trackNo()) {
        threeVec ret(vert->vert.x,vert->vert.y,vert->vert.z);
        return(ret);
        break;
      }
    }
  }
  return(zero);
}

void clasEvent::makePartPi0(int partbank,int addbank)
{
  clasPART_t *PART = (clasPART_t *)getGroup(this->_bcs,"PART",partbank);
  int igamma1,igamma2;
  if (addbank >= 0 && PART) {
    fourVec pi0;
    int addpi0 = getPi0(partbank,pi0,&igamma1,&igamma2);
    int npart = addpi0 ? PART->bank.nrow - 1: PART->bank.nrow;
    clasPART_t *newPART = (clasPART_t *)makeBank(this->_bcs,"PART",addbank,sizeof(part_t)/sizeof(int),npart);
      for (int k = 0,j = 0; k < PART->bank.nrow; ++k) {
    if (addpi0 && (k == igamma1)) {
      newPART->part[j] = PART->part[k];
      newPART->part[j].p.t = pi0.t();
      newPART->part[j].p.space.x = pi0.x();
      newPART->part[j].p.space.y = pi0.y();
      newPART->part[j].p.space.z = pi0.z();
      newPART->part[j].pid = Pi0;
      newPART->part[j].q = 0.0;
      j++;
    }
    else if (addpi0 && (k == igamma2)) {
      ;
    }
    else {
      newPART->part[j++] = PART->part[k];
    }
      }

  }

}



void clasEvent::remakeNeutrals(int partbank0,double neutronBetaCut) {
  {
    clasPART_t *PART = (clasPART_t *)getGroup(this->_bcs,"PART",partbank0);
    clasMVRT_t *MVRT = (clasMVRT_t *)getBank(this->_bcs,"MVRT");
    clasECHB_t *ECHB = (clasECHB_t *)getBank(this->_bcs,"ECHB");
    clasTGPB_t *TGPB = (clasTGPB_t *) getBank(this->_bcs,"TGPB");
#define LIGHTSPEED 30 // cm/nanosec
    fourVec p;
    part_t *part;
    echb_t *echb;
    tbid_t *tbid;
    double mass = 0.0;
    if (this->_TBID && MVRT && PART && ECHB && TGPB) {
      threeVec p1;
      fourVec p2;
      threeVec v(MVRT->mvrt[0].vert.x,MVRT->mvrt[0].vert.y,MVRT->mvrt[0].vert.z);
      threeVec r;
      double beta,p;
      for (int k = 0; k < PART->bank.nrow; ++k) {
    part = &PART->part[k];
    p1.set(part->p.space.x,part->p.space.y,part->p.space.z);
    p2.set(0.0,p1);
    switch ((Particle_t) part->pid) {
    case Gamma:
    case Neutron:
      // Get the corresponding echb bank
      tbid = &this->_TBID->tbid[part->trkid - 1];
      if ((tbid->ec_id > 0) && tbid->ec_stat) {
        echb = &ECHB->echb[tbid->ec_id - 1];
        r.set(echb->x_hit - v.x(),echb->y_hit - v.y(),echb->z_hit - v.z());
        beta = r.r()/((echb->t_hit - TGPB->tgpb[0].time) * LIGHT_SPEED);
        //      std::cout << "BETA " << beta << std::endl;
        if (beta < neutronBetaCut) {
          part->pid = Neutron;
          mass = NEUTRON_MASS;
          p = NEUTRON_MASS*tbid->ec_beta*beta2gamma(tbid->ec_beta);
          p2 = echb2partMVRT(echb,0.0,v);
          p1.set(p2.x(),p2.y(),p2.z());
          p1 *= p/p1.r();
          p2.set(sqrt(mass * mass + p1.r() * p1.r()),p1);
        }
        else {
          part->pid = Gamma;
          mass = 0.0;
          p2 = echb2partMVRT(echb,mass,v);
        }
        part->p.space.x = p2.x();
        part->p.space.y = p2.y();
        part->p.space.z = p2.z();
        part->p.t = p2.t();
        part->vert.x = v.x();
        part->vert.y = v.y();
        part->vert.z = v.z();
      }
      break;
    }
      }
    }
  }
}

threeVec clasEvent::v()
{
  if (this->_mvrt)
   return(threeVec(this->_mvrt->vert.x,this->_mvrt->vert.y,this->_mvrt->vert.z));
  else {
   this->makeVertex();
   if (this->_mvrt) {
     return(threeVec(this->_mvrt->vert.x,this->_mvrt->vert.y,this->_mvrt->vert.z));
   }
   else
     return(threeVec(-1000.0,-1000.0,-1000.0));
  }
}



threeVec clasEvent::V()
{
  if ((this->type() == MONTE_CARLO_TYPE) && this->_mc){
    if (this->PART()) {
      return(threeVec(this->PART()->part[0].vert.x,this->PART()->part[0].vert.y,this->PART()->part[0].vert.z));
    }
    else
      return(threeVec(-1000.0,-1000.0,-1000.0));
  }

  else if (this->_mvrt)
    return(threeVec(this->x(),this->y(),this->z()));
  else {
   this->makeVertex();
   if (this->_mvrt) {
     return(threeVec(this->_mvrt->vert.x,this->_mvrt->vert.y,this->_mvrt->vert.z));
   }
   else
     return(threeVec(-1000.0,-1000.0,-1000.0));
  }
}

//ut number of tracks used to make the vertex
int clasEvent::V_ntrk()
{
   if (this->_mvrt)
    return( (this->_mvrt->ntrk) );
  else {
   this->makeVertex();
   if (this->_mvrt) {
     return( (this->_mvrt->ntrk) );
   }
   else
     return(-1000);
  }
}

//ut chi2 <-> vertex
float clasEvent::V_chi2()
{
   if (this->_mvrt)
    return( (this->_mvrt->chi2) );
  else {
   this->makeVertex();
   if (this->_mvrt) {
     return( (this->_mvrt->chi2) );
   }
   else
     return(-1000.);
  }
}


//ut end





double clasEvent::x()
{
  double ret = -1000.0;

  if ((this->type() == MONTE_CARLO_TYPE) && this->_mc) {
    if (this->PART()) {
      ret = this->PART()->part[0].vert.x;
    }
  }
  else if (this->_mvrt)
    ret = this->_mvrt->chi2 > 0 ? this->_mvrt->vert.x : -1000;
  else {
    this->makeVertex();
    if (this->_mvrt) {
      ret = this->_mvrt->chi2 > 0 ? this->_mvrt->vert.x : -1000;
    }
    else
      ret = -1000.0;
  }
  return(ret);
}

double clasEvent::y()
{
  double ret = -1000.0;

  if ((this->type() == MONTE_CARLO_TYPE) && this->_mc) {
    if (this->PART()) {
      ret = this->PART()->part[0].vert.y;
    }
  }
  else if (this->_mvrt)
    ret = this->_mvrt->chi2 > 0 ? this->_mvrt->vert.y : -1000;
  else {
    this->makeVertex();
    if (this->_mvrt) {
      ret = this->_mvrt->chi2 > 0 ? this->_mvrt->vert.y : -1000;
    }
    else
      ret = -1000.0;
  }
  return(ret);
}


double clasEvent::z()
{
  double ret = -1000.0;
  if ((this->type() == MONTE_CARLO_TYPE) && this->_mc) {
    if (this->PART()) {
      ret = this->PART()->part[0].vert.z;
    }
  }
  else if (this->_mvrt)
    ret = this->_mvrt->chi2 > 0 ? this->_mvrt->vert.z : -1000;
  else {
    this->makeVertex();
    if (this->_mvrt) {
      ret = this->_mvrt->chi2 > 0 ? this->_mvrt->vert.z : -1000;
    }
    else
      ret = -1000.0;
  }
  return(ret);
}


void clasEvent::newBanks()
{
  std::list<string>::const_iterator p = this->_newBanks.begin();
  cout << "NEW " << this->run() << " " << this->event() << " ";
  while (p != this->_newBanks.end()) {
    cout << *p++ << " ";
  }
  cout << endl;

}

void clasEvent::dropNewBanks()
{
  char dum[10];
  std::list<string>::const_iterator p = this->_newBanks.begin();
  while (p != this->_newBanks.end()) {
    bdrop_(this->bcs()->iw, p->c_str(), 4);
    strcpy(dum,p->c_str());
    bankList(this->bcs(),"E-",dum);
    p++;
  }


}

void clasEvent::makeVertex()
{
  string s("MVRT");
  string sv("VERT");
  bankList(&bcs_, "E+", "MVRTVERT");
  this->_newBanks.push_back(s);
  this->_newBanks.push_back(sv);
  make_vert();
  make_mvrt();
  {
    clasMVRT_t *MVRT = (clasMVRT_t *)getBank(this->_bcs,"MVRT");

    if (MVRT)
      this->_mvrt = MVRT->mvrt;
    else
      this->_mvrt = NULL;
  }
}

// destructor

clasEvent::~clasEvent() {
  char mess[1024];
  if (this->_file) {
    sprintf(mess,"CLOSE BOSINPUT UNIT=%d",this->_file);
    fparm_c(mess);
    this->_file = 0;
    cerr << "clasEvent file closed " << endl;
  }
}




// clasOutput


int clasOutput::_init(char *name,int unit)
{
  char out[2048];
  int status = 1;
  this->_nwrite = 0;
  if (strlen(name)) {
    unlink(name);
    sprintf(out, "OPEN UNIT%d UNIT=%d FILE=\"%s\" WRITE  STATUS=NEW RECL=3600",unit,unit,name);
    if (!fparm_c(out)) {
      std::cerr << "Unable to open file: " << name << "\tError:\t" << strerror(errno) << std::endl;
      status = 0;
    }
    else {
      this->_name = new char[strlen(name) + 1];
      strcpy(this->_name,name);
      this->_unit = unit;
    }
  }
  else
    status = 0;
  this->_status = status;
  return(status);
}

int clasOutput::_append(char *name,int unit,char *mode)
{
  char out[2048];
  int status = 1;
  this->_nwrite = 0;
  if (strlen(name)) {
    unlink(name);
    sprintf(out, "OPEN UNIT%d UNIT=%d FILE=\"%s\" ACTION=READWRITE FORM=BINARY  STATUS=OLD ACCESS=SEQ RECL=3600",unit,unit,name);
    if (!fparm_c(out)) {
      std::cerr << "Unable to open file: " << name << "\tError:\t" << strerror(errno) << std::endl;
      status = 0;
    }
    else {
      this->_name = new char[strlen(name) + 1];
      strcpy(this->_name,name);
      this->_unit = unit;
    }
  }
  else
    status = 0;
  this->_status = status;
  return(status);
}
int clasOutput::close()
{
  char out[1024];
  if (this->_status && this->_nwrite) {
    putBOS(this->_bcs,this->_unit,"0");
    sprintf(out, "CLOSE UNIT%d UNIT=%d",this->_unit,this->_unit);
    fparm_c(out);
    return(1);
  }
  else
    return(0);
}

// momentum correction (g6c)

void clasEvent::momentumCorrection()
{
  if (this->_runPeriod == g6c) {

    if (this->_configMomentumCorrection != this->run()) {
      init_momentum_correction(this->run());
      this->_configMomentumCorrection = this->run();
      if (this->verbose()) {
    cerr << "Configure momentum corrections for g6c run " << this->run() << endl;
      }
    }
    {
      particle beampart = this->beam();
      fourVec beamP = this->beam().get4P();
      fourVec newBeam = photon_beam_correction(beamP);
      std::list<clasParticle>::iterator p = this->_cp.begin();
      std::list<clasParticle> _newparts;
      std::list<clasParticle>::iterator first,last;

      first = this->_cp.begin();
      last = this->_cp.end();

      beampart.set4P(newBeam);
      delete this->_beam;
      this->_beam = new particle(beampart);



      while (p != this->_cp.end() ) {
    clasParticle x;
    p->_momCorr();
    x = *p;
    _newparts.push_back(x);
    p++;
      }

      this->_cp.erase(first,last);
      p = _newparts.begin();
      while (p != _newparts.end()) {
    this->addParticle(*p);
    p++;
      }


    }
  }
  else if (this->_runPeriod == g1c) {
   if (this->_configMomentumCorrection != this->run()) {
      init_mom_corr(this->run());
      this->_configMomentumCorrection = this->run();
      if (this->verbose()) {
    cerr << "Configure momentum corrections for g1c run " << this->run() << endl;
      }
    }
    {
      particle beampart = this->beam();
      fourVec beamP = this->beam().get4P();
      fourVec newBeam = beamP;
      std::list<clasParticle>::iterator p = this->_cp.begin();
      std::list<clasParticle> _newparts;
      std::list<clasParticle>::iterator first,last;

      first = this->_cp.begin();
      last = this->_cp.end();

      //      beampart.set4P(newBeam);
      //   delete this->_beam;
      //    this->_beam = new particle(beampart);



      while (p != this->_cp.end() ) {
    clasParticle x;
    p->_momCorrg1c();
    x = *p;
    _newparts.push_back(x);
    p++;
      }

      this->_cp.erase(first,last);
      p = _newparts.begin();
      while (p != _newparts.end()) {
    this->addParticle(*p);
    p++;
      }


    }



  }
}
// energy loss

void clasEvent::eLoss()
{
  switch (this->runPeriod()) {
  case g10:
    this->eLoss("g10",2);
    break;
  case g1c:
    this->eLoss("g1c",1);
    break;
  case g6c:
  case g8a:
    this->eLoss("g6c",1);
    break;
  case g11a:
  case g12:
    this->eLoss("g11a",1);
    break;
  default:
    this->eLoss("g1c",1);
    break;
  }
}


void clasEvent::eLoss(char *cell,int iflag)
{

  int icell = 0;

  if (this->_configEloss == 0) {
    InitEloss(this->run());
    this->_configEloss = this->run();
  }

  if (strcmp(cell,"none") == 0 || !strlen(cell))
    icell = 0;
  else if (!strcmp(cell,"g1a")  || !strcmp(cell,"g1b") || !strcmp(cell,"g6"))
    icell = 1;
  else if (!strcmp(cell,"g2a") )
    icell = 2;

  else if (!strcmp(cell,"g1c"))
    icell = 3;
  else if (!strcmp(cell,"g3") )
    icell = 4;
  else if (!strcmp(cell,"g6c") || !strcmp(cell,"g8a") )
    icell = 5;
  else if (!strcmp(cell,"g10"))
    icell = 6;
  else if (!strcmp(cell,"g11a") )
    icell = 7;
  else if (!strcmp(cell,"g12"))
    icell = 7;
  else
    icell = 0;

  {
    std::list<clasParticle>::iterator p = this->_cp.begin();
    std::list<clasParticle> _newparts;
    std::list<clasParticle>::iterator first,last;

    first = this->_cp.begin();
    last = this->_cp.end();

    //  std::cout << "Particle list\n" << this->_cp.size() << std::endl;
    while (p != this->_cp.end() ) {
      clasParticle x;
      threeVec VV = this->V();
      if (VV.z() < -999.0)
    VV = this->v();
      p->_eLoss(VV,icell,iflag);
      x = *p;
      _newparts.push_back(x);
      p++;
    }

    this->_cp.erase(first,last);
    p = _newparts.begin();
    while (p != _newparts.end()) {
      this->addParticle(*p);
      p++;
    }

  }
}


// p_correction and energy loss
// should only be used for data

void clasEvent::pcorr_eLoss(char *cell,int iflag)
{
  this->eLoss(cell,iflag);
  this->momentumCorrection();
}



// General utilities

double deltaBeta(double l,double t)
{
#define DELTA_T .3
  return((l * DELTA_T)/(LIGHT_SPEED * t * t));
}


double idBeta(const threeVec &p,double m)
{
  double E = sqrt(p.r() * p.r() + m * m);
  return(p.r()/E);
}

double pidChisq(const threeVec &p,double mass,double len,double t)
{
  double Beta = len/(LIGHT_SPEED * t);
  return(pow( (Beta - idBeta(p,mass))/deltaBeta(len,t),2.0));
}

double M(Particle_t pid)
{
  double ret = -1000.0;
  switch (pid) {
  case PiPlus:
  case PiMinus:
    ret = PI_CHARGED_MASS;
    break;
  case Deuteron:
    ret = DEUTERON_MASS;
    break;
  case Proton:
    ret = PROTON_MASS;
    break;
  case KPlus:
  case KMinus:
    ret = KAON_CHARGED_MASS;
    break;
  case Positron:
  case Electron:
    ret = ELECTRON_MASS;
    break;

  case Gamma:
    ret = 0.0;
    break;

  case Neutron:
    ret = NEUTRON_MASS;
    break;
  case KLong:
  case KShort:
    ret = KAON_ZERO_MASS;
    break;
  case Pi0:
    ret = PI_ZERO_MASS;
    break;
  default:
    ret =-1000;
    break;
  }
  return(ret);
}

int pid2PDG(Particle_t pid)
{
  int ret = -1000;
  switch (pid) {
  case PiPlus:
    ret = 211;
    break;
  case PiMinus:
    ret = -211;
    break;
  case Proton:
    ret = 2212;
    break;
  case KPlus:
    ret = 321;
    break;
  case KMinus:
    ret = -321;
    break;
  case Positron:
    ret = -11;
    break;
  case Electron:
    ret = 11;
    break;

  case Gamma:
    ret = 22;
    break;

  case Neutron:
    ret = 2112;
    break;
  case KLong:
    ret = 130;
    break;
  case KShort:
    ret = 310;
    break;
  case Pi0:
    ret = 111;
    break;
  default:
    ret =-1000;
    break;
  }
  return(ret);
}





//  string  id2name( Particle_t pid )
//  {
//    Geant_ID gpid = (Geant_ID) pid;
//    return(id2name(gpid));
//  }


int Q(Particle_t pid)
{
  int ret = 0;
  switch (pid) {
  case PiPlus:
  case Proton:
  case KPlus:
  case Positron:
  case Deuteron:
    ret = 1;
    break;
  case PiMinus:
  case Electron:
  case KMinus:
  case AntiProton:
    ret = -1;
    break;
  case Gamma:
  case Neutron:
  case KLong:
  case KShort:
  case Pi0:
    ret = 0;
    break;
  default:
    ret =-100;
    break;
  }
  return(ret);
}


int getPi0(int partbank,fourVec &pi0,int *igam1,int *igam2)
{
#define MPI0 .135
#define DELTA .075
  int ipi,jpi;
  int npi0 = 0;
  int ngamma = 0;
  fourVec gamma[3];
  int kpart[3];
  float m[] = {-1000.0,-1000.0,-1000.0};
  clasPART_t *PART = (clasPART_t *)getGroup(&bcs_,"PART",partbank);
  if (PART) {
    part_t *part;
    for (int i = 0; i < PART->bank.nrow; ++i) {
      part = &PART->part[i];
      switch((Particle_t) part->pid) {
      case Gamma:
    if (ngamma < 3) {
      kpart[ngamma] = i;
      gamma[ngamma].set(part->p.t,threeVec(part->p.space.x,part->p.space.y,part->p.space.z));
      ngamma++;
    }

    break;
      }
    }
    for (int i = 0,k = 0; i < ngamma -1; ++i) {
      for (int j = i + 1; j < ngamma; ++j) {
    m[k] = ~(gamma[i] + gamma[j]);
    if (cut(m[k],MPI0 - DELTA, MPI0 + DELTA)) {
      npi0++;
      ipi = i;
      jpi = j;
    }
    k++;
      }
    }
  }
  if (npi0) {
    pi0 = gamma[ipi] + gamma[jpi];
    *igam1 = kpart[ipi];
    *igam2 = kpart[jpi];

  }
  return(npi0);

}


fourVec echb2partMVRT(echb_t *echb, double mass,threeVec &v)
{
  threeVec p,r;
  fourVec pt;
  float energy = gamma_energy(echb);
  float momentum = sqrt(energy * energy - mass * mass);
  r.set(echb->x_hit - v.x(),echb->y_hit - v.y(),echb->z_hit - v.z());

  //  part->p.space.x = energy*(echb->x_hit)/mag;
  //  part->p.space.y = energy*(echb->y_hit)/mag;
  //  part->p.space.z = energy*(echb->z_hit)/mag;
  //  part->p.t = energy;
  //  part->q = 0;
  p.set(momentum*(r.x()/r.r()),momentum*(r.y()/r.r()),momentum*(r.z()/r.r()));
  pt.set(sqrt(p.lenSq() + mass * mass),p);
  return(pt);

}

int cut(float x,float low,float hi)
{
  return((x > low) && (x < hi));
}

void clasEvent::clean()
{
  this->dropNewBanks();
  dropAllBanks(this->_bcs,"E");
  cleanBanks(this->_bcs);
}

// clasParticle code for clasEvent

void clasEvent::printGampEvent(ostream& os)
{

  int n = this->Ncp();
  fourVec beam = this->beam().get4P();
  os << n + 1 << endl;
  //  os <<   os << name2id(this->beam().Name() << " " << this->beam().Charge();
  os << " " << beam.x() << " " << beam.y() << " " << beam.z() << " " << beam.t() << endl;

  for (int i = 0; i < n; ++i) {
    clasParticle cp = this->cp(i + 1);
    os << cp.pid() << " " << cp.Q() << " " << cp.p().x() << " " << cp.p().y() << " " << cp.p().z() << " " << cp.p().t() << endl;
  }

}

void clasEvent::Print() const
{
  this->beam().get4P().print();
  std::cout << "sqrt(s): " << sqrt(this->s()) << std::endl;
  this->printParticles();
}

void clasEvent::printParticles() const
{
  std::list<clasParticle>::const_iterator p = this->_cp.begin();
  std::cout << "Particle list\n" << this->_cp.size() << std::endl;
  while (p != this->_cp.end() ) {
    p->Print();
    p++;
  }
}
clasEvent& clasEvent::addTaggedPhoton(const clasTaggedPhoton& tag)
{
  this->_tag.push_back(tag);
  return *this;
}
clasEvent& clasEvent::addTaggedPhotonWindow(const clasTaggedPhoton& tag)
{
  this->_tagWindow.push_back(tag);
  return *this;
}

clasEvent& clasEvent::addParticle(const clasParticle& cp)
{
  this->_cp.push_back(cp);
  return *this;
}
clasEvent& clasEvent::deleteParticle(std::list<clasParticle>::iterator cp)
{
  this->_cp.erase(cp);
  return *this;
}

int clasParticle::g_stat()
{

  return(0);

}

double clasParticle::tofLength() const
{
  return(this->_tbidptr ? this->_tbidptr->beta * (this->_tbidptr->sc_time - this->_tbidptr->vtime) * LIGHT_SPEED : -1000.0);


}


double clasEvent::beta(Particle_t pid,int index)
{
  int _index = 0;
  if (this->_TBID) {
   std::list<clasParticle>::const_iterator p = this->_cp.begin();
   while (p != this->_cp.end() ) {
     if (p->pid() == pid) {
       _index++;
       if (_index == index) {
     return(this->_TBID->tbid[p->tbid()-1].beta);
       }
     }
     p++;
   }
  }
  return(-1000.0);
}

int clasEvent::N(int q) const
{
  int n = 0;
  std::list<clasParticle>::const_iterator p = this->_cp.begin();
  for (p = this->_cp.begin(); p !=   this->_cp.end(); ++p) {
    if (p->Q() == q)
      n++;
  }
  return(n);

}

// clasEvent bosDump -----------------------------------------------------------------------------------------

void clasEvent::bosDump(std::list<string> &banks)
{
  char **dummy;
  int i = 0;
  int n = banks.size();
  if (n) {
    char **groups = new char*[n];
    std::list<string>::const_iterator p = banks.begin();
    while (p != banks.end()) {
      groups[i++] = (char *) p->c_str();
      p++;
    }
    PrintEvent(n,(char **)groups,0,dummy);
  }
  else
    PrintEvent(0,dummy,0,dummy);
}

void clasEvent::bosDump()
{
  char **dummy;

    PrintEvent(0,dummy,0,dummy);
}


// clasEvent a1c Modules --------------------------------------------------------------------------------------

void clasEvent::doHitBasedTracking()
{
  if (this->run() != this->_configSWIM) {
    this->ConfigSwim();
  }
  ProcessHitBased();
}


void clasEvent::doTimeBasedTracking()
{
  if (this->run() != this->_configSWIM) {
    this->ConfigSwim();
  }
  dropAllBanks(this->_bcs,"TBTRTBLATBERTDPL");
  ProcessTimeBased();
}

void clasEvent::doLAC()
{
  dropAllBanks(this->_bcs,"EC1R");
  ec1_evnt_();
}

void clasEvent::doCC()
{

   dropAllBanks(this->_bcs,CC_BANKS);
    cc_bevt_();
    cc_evnt_(); /* make the CCRC bank */
    for(int sec=1; sec <= 6; sec++) {
      make_CC01_bank(sec);
    }
}

void clasEvent::doEC()
{
  if (this->_configEC != this->run()) {
    this->ConfigEC();
  }
  dropAllBanks(&bcs_,EC_BANKS);
  ec_evnt_();
  }


void clasEvent::doSC()
{
  if (this->_configSC != this->run()) {
    this->ConfigSC();
  }
  dropAllBanks(&bcs_, SC_BANKS);
  for(int sec=1; sec <= 6; sec++) {
    make_SC1_bank(sec);
    make_SCR_bank(sec, "HDPL");
    make_SCRC_bank(sec);
  }

}

void clasEvent::doTAGGER()
{
  if (this->_configTAGGER != this->run()) {
    this->ConfigTAGGER();
  }
  dropAllBanks(&bcs_, TAGGER_BANKS);
  bankList(this->_bcs, "E+", TAGGER_BANKS);
  tag_evnt_();
  this->makeTAGM();

}

void clasEvent::doHBTR()
{

  int fail;

  if (!this->configSwimStatus())
    this->ConfigSwim();

  setup_anapatt_();
  for (int sec=1; sec <= 6; sec++){
    ana_segm_(&sec, &fail);
    trk_link_(&sec, &fail);
  }
  ana_prfit_(&fail);

  if (fail && this->verbose() ) {
    cerr << "clasEvent:\tHit based tracking failed" << endl;
  }

}


clasParticle::clasParticle()
{
  //this->_pid = 0;
  this->_tbidIndex = 0;
  this->_ccidIndex = 0;
  this->_ccadc=0;
  this->_NPE = 0;
}

double clasEvent::zTarget()
{
  double ztarget = -1000.0;
  clasTGEO_t *TGEO =  (clasTGEO_t *) getBank(&wcs_, "TGEO");
  if (!TGEO) {
    this->ConfigTGEO();
    TGEO =  (clasTGEO_t *) getBank(&wcs_, "TGEO");
  }
  if (TGEO) {
    ztarget = TGEO->tgeo[0].z;
  }
  return(ztarget);
}

// time of a particle from CLAS center to the vertex
double clasParticle::tprop()
{
  double ztarget = 0.0;
  clasTGEO_t *TGEO =  (clasTGEO_t *) getBank(&wcs_, "TGEO");
  if (!TGEO) {
    this->ev()->ConfigTGEO();
    TGEO =  (clasTGEO_t *) getBank(&wcs_, "TGEO");
  }
  if (TGEO) {
    ztarget = TGEO->tgeo[0].z;
  }

  return((this->z() - ztarget)/LIGHT_SPEED);
}

// time of a particle from CLAS center to the vertex
double clasParticle::tprop(float vz)
{
  double ztarget = 0.0;
  clasTGEO_t *TGEO =  (clasTGEO_t *) getBank(&wcs_, "TGEO");
  if (!TGEO) {
    this->ev()->ConfigTGEO();
    TGEO =  (clasTGEO_t *) getBank(&wcs_, "TGEO");
  }
  if (TGEO) {
    ztarget = TGEO->tgeo[0].z;
  }

  return((vz - ztarget)/LIGHT_SPEED);
}

// time of a particle at the vertex
double clasParticle::tVertex()
{
  double ztarget = 0.0;
  clasTGEO_t *TGEO =  (clasTGEO_t *) getBank(&wcs_, "TGEO");
  if (!TGEO) {
    this->ev()->ConfigTGEO();
    TGEO =  (clasTGEO_t *) getBank(&wcs_, "TGEO");
  }
  if (TGEO) {
    ztarget = TGEO->tgeo[0].z;
  }

 if (this->_pidflag == GPID_FLAG) {
   clasGPID_t *GPID = (clasGPID_t *) getBank(this->_bcs,"GPID");
   return(GPID ? GPID->gpid[this->_gpidIndex].tpho + (this->z() - ztarget)/LIGHT_SPEED : -1000.0);
 }
 else {
   return (this->_tbidptr ? this->_tbidptr->vtime : -1000.0);
 }

}

double clasParticle::scVertexTime()
{
  double ret;
  if (this->_pidflag == GPID_FLAG) {
    clasGPID_t *GPID = (clasGPID_t *) getBank(this->_bcs,"GPID");
    if (GPID) {
      ret = GPID ? (GPID->gpid[this->_gpidIndex].sc_len - this->beta() * LIGHT_SPEED * GPID->gpid[this->_gpidIndex].sc_time)/(LIGHT_SPEED * this->beta()) : -1000;
    }
  }

  else {
    ret = this->_tbidptr ? (this->tbidptr()->sc_time - this->scPathLen()/(LIGHT_SPEED * this->beta())) : -1000;
  }

  return(ret);

}
double clasParticle::scVertexTimeBeta()
{
  double ret;
  if (this->_pidflag == GPID_FLAG) {
    clasGPID_t *GPID = (clasGPID_t *) getBank(this->_bcs,"GPID");
    if (GPID) {
      ret = GPID ? (GPID->gpid[this->_gpidIndex].sc_len - this->beta() * LIGHT_SPEED * GPID->gpid[this->_gpidIndex].sc_time)/(LIGHT_SPEED * this->beta()) : -1000;
    }
  }

  else {
    ret = this->_tbidptr ? (this->tbidptr()->sc_time - this->scPathLen()/(LIGHT_SPEED * this->Beta())) : -1000;
  }

  return(ret);

}

double clasParticle::x()
{
  double x = 0.0;

  clasTBTR_t *TBTR = (clasTBTR_t *)getBank(_bcs,"TBTR");
  if (TBTR)
    x = TBTR->tbtr[this->trackNo() - 1].vert.x;
 else {
    x = this->_pos.x();
  }
  return(x);
}


double clasParticle::y()
{
  double y = 0.0;

  clasTBTR_t *TBTR = (clasTBTR_t *)getBank(_bcs,"TBTR");
  if (TBTR)
    y = TBTR->tbtr[this->trackNo() - 1].vert.y;
  else {
    y = this->_pos.y();
  }
  return(y);
}



double clasParticle::z()
{
  double z = 0.0;

  clasTBTR_t *TBTR = (clasTBTR_t *)getBank(_bcs,"TBTR");
  if (TBTR)
    z = TBTR->tbtr[this->trackNo() - 1].vert.z;
  else {
    z = this->_pos.z();
  }
  return(z);
}

double clasParticle::idChisq(Particle_t pid)
{

  return(pidChisq(this->p().V(),M(pid),this->scPathLen(),this->scTOF()));


}

double clasParticle::scVertexTime(double mass)
{
  double ret = -1000.0;
  if (this->tbidptr()) {
    double pmom = this->p().V().r();
    double beta = pmom/sqrt(pmom*pmom + mass * mass);
    ret = this->tbidptr()->sc_time -  this->scPathLen()/(LIGHTSPEED*beta);
  }
  return(ret);

}

// EC stuff

echb_t *clasParticle::echbHit()
{
  clasECHB_t *ECHB = (clasECHB_t *) this->getEvtBank("ECHB");
  if (ECHB && this->_tbidptr) {
    return(&ECHB->echb[this->_tbidptr->ec_id - 1]);
  }
  else
    return(NULL);
}

echb_t *clasParticle::echbHitIn()
{
  echb_t *in = NULL,*out = NULL;
  echb_t *whole = this->echbHit();
  if (whole) {
    ec_Whole2InOut(whole,&in,&out);
  }
  return(in);

}
echb_t *clasParticle::echbHitOut()
{
  echb_t *in = NULL,*out = NULL;
  echb_t *whole = this->echbHit();
  if (whole) {
    ec_Whole2InOut(whole,&in,&out);
  }
  return(out);

}

int clasParticle::ecStatus()
{
  return(this->echbHit() != NULL);
}

double clasParticle::ecEnergy()
{
  if (this->echbHit())
    return(this->echbHit()->e__hit);
  else
    return(0.0);
}
double clasParticle::ecEnergyIn()
{
  echb_t *in = this->echbHitIn();
  if (in)
    return(in->e__hit);
  else
    return(0.0);
}
double clasParticle::ecEnergyOut()
{
  echb_t *out = this->echbHitOut();
  if (out)
    return(out->e__hit);
  else
    return(0.0);
}
double clasParticle::ecTime()
{
  if (this->echbHit())
    return(this->echbHit()->t_hit);
  else
    return(-1000.0);
}

double clasParticle::ecVtime()
{
  if (this->_tbidptr) {
    return(this->_tbidptr->ec_vtime);
  }
  else
    return(-1000.0);
}

threeVec clasParticle::ECdir()
{
  threeVec ret(-1000.,-1000.,-1000.);
  clasTDPL_t *TDPL = (clasTDPL_t *) getGroup(this->_bcs,"TDPL",this->sec());

  if (this->tbidptr() && TDPL) {
    int track = this->tbidptr()->track;
    for (int j = 0; j < TDPL->bank.nrow; ++j) {
      tdpl_t *tdpl = &TDPL->tdpl[j];
      if ((track == tdpl->trk_pln/100) && (NEC_MIN + 1) == (tdpl->trk_pln % 100)) {
    ret.set(tdpl->dir.x,tdpl->dir.y,tdpl->dir.z);
      }
    }

  }
  return(ret);
}
/*
 * ECpos() takes in data from TDPL, used for CHARGED particles.  Use ecPosition() for NEUTRAL particles.
 *
 */
threeVec clasParticle::ECpos()
{
  threeVec ret(-1000.,-1000.,-1000.);
  clasTDPL_t *TDPL = (clasTDPL_t *) getGroup(this->_bcs,"TDPL",this->sec());

  if (this->tbidptr() && TDPL) {
    int track = this->tbidptr()->track;
    for (int j = 0; j < TDPL->bank.nrow; ++j) {
      tdpl_t *tdpl = &TDPL->tdpl[j];
      if ((track == tdpl->trk_pln/100) && (NEC_MIN + 1) == (tdpl->trk_pln % 100)) {
    ret.set(tdpl->pos.x,tdpl->pos.y,tdpl->pos.z);
      }
    }
   }
  return(ret);
}

int clasParticle::ECfidcut()
{
  int ret = 0;
  threeVec uvw = ECxyz2uvw();

  int ucut = ((uvw.x()>=20.0) && (uvw.x()<=400.0));
  int vcut = (uvw.y()<=375.0);
  int wcut = (uvw.z()<=405.0);

  ret = (ucut && vcut && wcut);

  return(ret);
}

threeVec clasParticle::ECuvw()
{

   if (this->echbHit())
     return(threeVec(this->echbHit()->centr_u,this->echbHit()->centr_v,this->echbHit()->centr_w));
   else
     return(threeVec(-1000.0,-1000.0,-1000.0));
}

threeVec clasParticle::ECxyz2uvw()
{

  float ec_the = 0.4363323;
  float ylow = -182.974;
  float yhi = 189.956;
  float tgrho = 1.95325;
  float sinrho = 0.8901256;
  float cosrho = 0.455715;
  float u, v, w;
  float rot11, rot12, rot13;
  float rot21, rot22, rot23;
  float rot31, rot32, rot33;
  float x, y, z;
  float xi, yi, zi;
  float ec_phi;

  x = this->ecPosition().x();
  y = this->ecPosition().y();
  z = this->ecPosition().z();
  //  x = this->p().V().x();
  //  y = this->p().V().y();
  //  z = this->p().V().z();

  ec_phi = (this->sec()-1)*60.0*DEG_2_RAD_CONV;

  rot11 = cos(ec_the)*cos(ec_phi);
  rot12 = -sin(ec_phi);
  rot13 = sin(ec_the)*cos(ec_phi);
  rot21 = cos(ec_the)*sin(ec_phi);
  rot22 = cos(ec_phi);
  rot23 = sin(ec_the)*sin(ec_phi);
  rot31 = -sin(ec_the);
  rot32 = 0.0;
  rot33 = cos(ec_the);

  yi = x*rot11 + y*rot21 + z*rot31;
  xi = x*rot12 + y*rot22 + z*rot32;
  zi = x*rot13 + y*rot23 + z*rot33;
  zi-=510.32;

  u = (yi-ylow)/sinrho;
  v = (2.0*yhi-ylow-yi)/tgrho - xi;
  w = 0.5*((2.0*yhi-ylow-yi)/tgrho - xi)/cosrho;
  return(threeVec(u,v,w));
}
//  ecPosition should only be used for NEUTRAL particles.  Use ecPos for CHARGED particles.
threeVec clasParticle::ecPosition()
{
   if (this->echbHit())
     return(threeVec(this->echbHit()->x_hit,this->echbHit()->y_hit,this->echbHit()->z_hit));
   else
     return(threeVec(-1000.0,-1000.0,-1000.0));
}

int clasParticle::ecSector()
{
    if (this->echbHit())
    return(this->echbHit()->sect/100);
  else
    return(0);
}

int clasParticle::ecLayer()
{
    if (this->echbHit())
    return(this->echbHit()->sect % 100);
  else
    return(0);
}

int clasParticle::ccStatus()
{
  if (this->_tbidptr) {
    return(this->_tbidptr->cc_stat);
  }
  else
    return(0);
}

double clasParticle::ccTime()
{
  if (this->_tbidptr) {
    return(this->_tbidptr->cc_time);
  }
  else
    return(0);
}


double clasParticle::ccVtime()
{
  if (this->_tbidptr) {
    return(this->_tbidptr->cc_vtime);
  }
  else
    return(0);
}

double clasParticle::ccNphe()
{
  double ret = -1000.0;
   if (this->_tbidptr) {
     clasCC01_t *CC01 = (clasCC01_t *)this->getEvtBank("CC01",this->sec());
     if (CC01) {
       int id = this->_tbidptr->cc_id;
       if (id  && id <= CC01->bank.nrow) {
     ret = CC01->cc01[id - 1].n_pe;
       }
     }
   }
   return(ret);
}

// LAC stuff - M H Wood
int clasParticle::lacStatus()
{
  if (this->_tbidptr) {
    return(this->_tbidptr->lac_stat);
  }
  else
    return(0);
}

double clasParticle::lacTime()
{
  if (this->_tbidptr) {
    return(this->_tbidptr->lac_time);
  }
  else
    return(0);
}


double clasParticle::lacVtime()
{
  if (this->_tbidptr) {
    return(this->_tbidptr->lac_vtime);
  }
  else
    return(0);
}

double clasParticle::lacEtot()
{
  double ret = -1000.0;
   if (this->_tbidptr) {
     clasEC1R_t *EC1R = (clasEC1R_t *)this->getEvtBank("EC1R",this->sec());
     if (EC1R) {
       int id = this->_tbidptr->lac_id;
       if (id  && id <= EC1R->bank.nrow) {
     ret = EC1R->ec1r[id - 1].e_tot;
       }
     }
   }
   return(ret);
}

double clasParticle::lacEin()
{
  double ret = -1000.0;
   if (this->_tbidptr) {
     clasEC1R_t *EC1R = (clasEC1R_t *)this->getEvtBank("EC1R",this->sec());
     if (EC1R) {
       int id = this->_tbidptr->lac_id;
       if (id  && id <= EC1R->bank.nrow) {
     ret = EC1R->ec1r[id - 1].e_in;
       }
     }
   }
   return(ret);
}

threeVec clasParticle::LACdir()
{
  threeVec ret(-1000.,-1000.,-1000.);
  clasTDPL_t *TDPL = (clasTDPL_t *) getGroup(this->_bcs,"TDPL",this->sec());

  if (this->tbidptr() && TDPL) {
    int track = this->tbidptr()->track;
    for (int j = 0; j < TDPL->bank.nrow; ++j) {
      tdpl_t *tdpl = &TDPL->tdpl[j];
      if ((track == tdpl->trk_pln/100) && (NEC_MAX + 1) == (tdpl->trk_pln % 100)) {
    ret.set(tdpl->dir.x,tdpl->dir.y,tdpl->dir.z);
      }
    }

  }
  return(ret);
}

threeVec clasParticle::LACpos()
{
  threeVec ret(-1000.,-1000.,-1000.);
  clasTDPL_t *TDPL = (clasTDPL_t *) getGroup(this->_bcs,"TDPL",this->sec());

  if (this->tbidptr() && TDPL) {
    int track = this->tbidptr()->track;
    for (int j = 0; j < TDPL->bank.nrow; ++j) {
      tdpl_t *tdpl = &TDPL->tdpl[j];
      if ((track == tdpl->trk_pln/100) && (NEC_MAX + 1) == (tdpl->trk_pln % 100)) {
    ret.set(tdpl->pos.x,tdpl->pos.y,tdpl->pos.z);
      }
    }

  }
  return(ret);
}

// Trigger particle finder - M H Wood
// This function use the BIT(n) function to check for trigger bits.
// The index of bit numbers starts with 0 for bit 1, 1 for bit 2, ...
// following the C programming numbering scheme
int clasParticle::triggerParticle(unsigned int t)
{

  int ret = 0;
  int ECcheck = 0;
  int CCcheck = 0;
  int SCcheck = 0;

  if(this->ev()->runPeriod() == g7){
    ECcheck = (this->ecEnergy()>0.0);
    CCcheck = (this->isCChit());
    SCcheck = (this->isSChit());
    if(t & BIT(2)){
      if(SCcheck) ret=2;
    }
    if(t & BIT(4)){
      if(ECcheck && CCcheck) ret=5;
    }
    if((t & BIT(5)) && (ret==0)){
      if(ECcheck || CCcheck) ret=6;
    }
  }
  return(ret);
}

// Fiducial cuts routine based on $CLAS_PACK/pid/pseudo_spa.F code
int clasParticle::FidCut()
{
  int ret = 1;
  int runNo = 0;
  float TorusCurrent;

  double ThetaMin,pnorm,exp,DeltaPhi,acc;

  // set constant values
  double Tmax = 3375.;
  double Phi0_el = 30.;
  double Phi0_nh = 25.;
  double Phi0_ph = 25.;
  double Theta0_el = 15.5;
  double Theta0_nh = 10.;
  double Theta0_ph = 10.;
  double Thetas_el = 15.;
  double Thetas_nh = 15.;
  double Thetas_ph = 25.;
  double p_shift = 0.05;
  double pel_ex = 0.333;
  double pim_ex = 0.5;
  double cel_ex = 0.35;
  double ThetaCut = 75.;
  double ch_ex = 0.1;
  double Theta_nip = 2.;
  double cel_ex_nip = 0.5;
  double Phi0_el_nip = 20.;
  double u_acc = 0.20944;

  clasHEAD_t *HEAD = (clasHEAD_t *)getBank(this->_bcs,"HEAD");
  clasRUNC_t *RUNC= (clasRUNC_t *)getBank(&wcs_,"RUNC");

  if (HEAD) {
    runNo = HEAD->head[0].nrun;
  }

  if (!RUNC) {
    make_RUNC_bank(runNo);
    RUNC = (clasRUNC_t *)getBank(&wcs_, "RUNC");
  }

  if(RUNC){
    TorusCurrent = RUNC->runc.torus.val.f[0];
    //    printf("Torus current %f, run %i\n",RUNC->runc.torus.val.f[0],runNo);
  }

  double pmom = this->p().V().r();
  double Theta = (this->p().V().theta())*RAD_2_DEG_CONV;
  double Phi = (this->p().V().phi())*RAD_2_DEG_CONV;
  if(Phi<0.0){
    Phi = 360.0 + Phi;
  }

  // calc Phi in sector range from -30.0 to 30.0
  Phi += 30.0;
  Phi -= (int)((Phi/60.0))*60.0;
  Phi -= 30.0;

  pnorm = pmom*Tmax/TorusCurrent;

  switch (this->pid()) {
  case Electron:
    ThetaMin = Theta0_el + Thetas_el/(pnorm + p_shift);
    if((Theta > ThetaMin) && (Theta<50.)){
      exp = cel_ex*pow(pnorm,pel_ex);
      DeltaPhi = Phi0_el*pow(sin((Theta-ThetaMin)*DEG_2_RAD_CONV),exp);
    }
    if((Theta > (ThetaMin-Theta_nip)) && (Theta<50.)){
      exp = cel_ex_nip*pow(pnorm,pel_ex);
      DeltaPhi = Phi0_el_nip*pow(sin((Theta-ThetaMin+Theta_nip)*DEG_2_RAD_CONV),exp);
    }
    if(abs(Phi)<DeltaPhi){
      acc = sin(Theta*DEG_2_RAD_CONV)*DeltaPhi*u_acc;
    }
    break;
  case Proton:
  case PiPlus:
  case KPlus:
    ThetaMin = Theta0_ph + Thetas_ph*pow((1.-pnorm/5.),24.);
    if(Theta > ThetaMin){
      exp = pow((pnorm/5.),(1./8.));
      DeltaPhi = Phi0_ph*pow(cos((Theta-ThetaCut)*DEG_2_RAD_CONV),exp);
    }
    if(abs(Phi)<DeltaPhi){
      acc = sin(Theta*DEG_2_RAD_CONV)*DeltaPhi*u_acc;
    }
    break;
  case PiMinus:
  case KMinus:
    ThetaMin = Theta0_nh + Thetas_nh/(pnorm+p_shift);
    if((Theta > ThetaMin) && (Theta < 130.)){
      exp = ch_ex*pow(pnorm,pim_ex);
      DeltaPhi = Phi0_nh*pow(sin((Theta-ThetaMin)*DEG_2_RAD_CONV),exp);
    }
    if(abs(Phi)<DeltaPhi){
      acc = sin(Theta*DEG_2_RAD_CONV)*DeltaPhi*u_acc;
    }
    break;
  default:
    acc = 0.;
    break;
  }

  if(acc>0.0){
    ret = 1;
  }
  return(ret);
}

int clasParticle::IsLeptonOld()
{
  // in this method, the Lepton could only be electron or positron

  bool bIsLepton = false;   // assumes we start without an lepton
  int nReturnValue = 0;

  int ntab = 0;

  const float c_fMassSqLow = - 0.0025; // lower limit for e mass square
  const float c_fMassSqUp = 0.0025;    // upper limit for e mass square
  const float c_fEcaloThreshold = 0.1; // Energy  threshold for the calorimeter
  const float c_fEoverPlow = 0.22;     // lower limit for Ecalo/p
  const float c_fEoverPup = 0.44;      // upper limit for Ecalo/p

  clasTBTR_t * TBTR = (clasTBTR_t*) getBank(&bcs_, "TBTR");
  clasTBID_t * TBID = (clasTBID_t*) getBank(&bcs_, "TBID");
  clasCC01_t * CC01 = (clasCC01_t*) getBank(&bcs_, "CC01");
  clasECHB_t * ECHB = (clasECHB_t*) getBank(&bcs_, "ECHB");

  if(TBTR && TBID){

    // index
    int nTBID_Index = this->tbid();

    if (nTBID_Index && TBID->tbid[nTBID_Index - 1].sc_stat){

      if(TBTR->bank.nrow < nTBID_Index){
    cerr<<"clasEvent:IsLepton: bad pointer to TBTR bank"<<endl;
    return(0);
      }
      int nTBTR_Index = TBID->tbid[nTBID_Index-1].track;

      // gets kinematics
      float fMoment = v3mag(TBTR->tbtr[nTBTR_Index-1].p);
      float fBeta = TBID->tbid[nTBID_Index-1].beta;
      float fBeta2 = fBeta*fBeta;
      float fMassSq = fMoment*fMoment*(1-fBeta2)/fBeta2;

      if (this->ev()->verbose()) {
    cerr << "ISLEP Q/momentum: " << this->Q() << "/" << fMoment << " beta: " << fBeta << " Mass^2: " << fMassSq << endl;
    ntab++;
      }

      if(ECHB){
    if(CC01 && (TBID->tbid[nTBID_Index-1].cc_stat) && (TBID->tbid[nTBID_Index-1].ec_stat)){
      // There is hits in the CC and the EC so we can use them.
      int nECHB_Index = TBID->tbid[nTBID_Index-1].ec_id;
      float fE_calo = ECHB->echb[nECHB_Index-1].e__hit;
      float fPhotoElect = this->isCChit() ? this->CChit().npe() : 0.0;
      double  nCCphoto ;
      float fRatio = fE_calo/fMoment;

      if (this->ev()->verbose() ) {
        for (int i = 0; i < ntab; ++i) cerr << "\t";
        if (this->isCChit()) nCCphoto = this->CChit().npe();
        cerr << "Ecal/threshold: " << fE_calo << ">" << c_fEcaloThreshold << "\tratio/low:high " << fRatio << "/" << c_fEoverPlow << ":" << c_fEoverPup <<  " # CC photo " << nCCphoto << endl;
        cerr << "# photoelectrons/threshold " << fPhotoElect << "/" << CC_NPE << endl;
        ntab++;
      }

      if((fPhotoElect >= CC_NPE) && (fE_calo > c_fEcaloThreshold) && (fRatio > c_fEoverPlow) &&  (fRatio < c_fEoverPup)){
        // EUREKA, we have a lepton
        bIsLepton = true;
        if (this->ev()->verbose() ) {
          for (int i = 0; i < ntab; ++i)
        cerr << "\t";
          cerr << "Lepton! by EC & CC" << endl;
        }
      }
      else {
        // not a lepton
        bIsLepton = false;
        if (this->ev()->verbose() ) {
          for (int i = 0; i < ntab; ++i)
        cerr << "\t";
          cerr << "not a Lepton by EC & CC" << endl;
        }
      }
    }
      }

      if (!bIsLepton && this->ev()->verbose() ) {
    ntab--;
    for (int i = 0; i < ntab; ++i) cerr << "\t";
    cerr << "Momentum/threshold: " << fMoment << "/" << ISLEP_MOM_THRESHOLD << " Mass^2/low:high " << fMassSq << "/" << c_fMassSqLow << ":" << c_fMassSqUp << endl;
    ntab++;
      }

      if (!bIsLepton && (fMoment < ISLEP_MOM_THRESHOLD)){
    // case TOF gives good identification
    if ((fMassSq > c_fMassSqLow) && (fMassSq < c_fMassSqUp)) {
      // EUREKA, we have a lepton
      bIsLepton = true;
      if (this->ev()->verbose() ) {
        for (int i = 0; i < ntab; ++i) cerr << "\t";
        cerr << "Lepton! by TOF" << endl;
      }
    }
    else {
      // not a lepton
      bIsLepton = false;
      if (this->ev()->verbose() ) {
        for (int i = 0; i < ntab; ++i)
          cerr << "\t";
        cerr << "not a Lepton by TOF" << endl;
      }
    }
      }

      if(bIsLepton){
    // return 1 if the particle is a positron and -1 if the particle is an electron
    nReturnValue= (int) TBTR->tbtr[nTBTR_Index-1].q;
      }
      else{
    //return 0 if the particle in neither an electron or a positron
    nReturnValue=0;
      }
    }
  }
  return(nReturnValue);
}

int clasParticle::IsLepton_ECtot()
{
  // Test the EC cut of IsLepton
  int nReturnValue = 0;
  float ECthrLow; // Lower energy  threshold for the calorimeter
  float ECthrHigh; // Higher energy  threshold for the calorimeter

  clasTBTR_t * TBTR = (clasTBTR_t*) getBank(&bcs_, "TBTR");
  clasTBID_t * TBID = (clasTBID_t*) getBank(&bcs_, "TBID");

  if(TBTR && TBID && this->isEChit()){
    int nTBID_Index = this->tbid();
    if (nTBID_Index && TBID->tbid[nTBID_Index - 1].sc_stat){
      if(TBTR->bank.nrow < nTBID_Index){
    cerr<<"clasEvent:IsLepton_ECtot: bad pointer to TBTR bank"<<endl;
    return(0);
      }
      int nTBTR_Index = TBID->tbid[nTBID_Index-1].track;
      float fMoment = v3mag(TBTR->tbtr[nTBTR_Index-1].p); // momentum
      float fE_calo = this->ecEnergy();

      // calculate EC total energy cut based on momentum
      ECthrLow = predict_e(TBTR->tbtr[nTBTR_Index-1].p)*(fMoment+ISLEP_EC_P_LO)/fMoment;
      ECthrHigh = predict_e(TBTR->tbtr[nTBTR_Index-1].p)*(fMoment+ISLEP_EC_P_HI)/fMoment;

      if((fE_calo>=ECthrLow) && (fE_calo<=ECthrHigh)){
    nReturnValue= this->Q(); // return 1(-1) for positron(electron)
      }
    }
  }
  return(nReturnValue);
}

int clasParticle::IsLepton_ECin2EC(float ecin_ec_lo, float ecin_ec_hi)
{
  // Test the ECin/EC cut of IsLepton
  int nReturnValue = 0;
  float ecin, ectot, ecratio;

  if(this->isSChit()){
    ecin = this->ecEnergyIn();
    ectot = this->ecEnergy();
    ecratio = ecin/ectot;
    if(cut(ecratio,ecin_ec_lo,ecin_ec_hi)){
      nReturnValue= this->Q(); // return 1(-1) for positron(electron)
    }
  }
  return(nReturnValue);
}

int clasParticle::IsLepton_ECio()
{
  // Test the ECin vs ECout cuts of IsLepton
  // It is a combined cut of ECin/P vs ECout/P and ECin vs ECout

  int nReturnValue = 0;

  int ecio_cut1 = 0;
  int ecio_cut2 = 0;
  int ecio_p_cut = 0;

  float ecin, ecout, ecin_p, ecout_p, mom;

  if(this->isSChit()){
    mom  = this->p().r();       // particle momentum
    ecin = this->ecEnergyIn();  // EC inner energy
    ecin_p = ecin/mom;          // ratio of EC inner energy and momentum
    ecout = this->ecEnergyOut();// EC outer energy
    ecout_p = ecout/mom;        // ratio of EC outer energy and momentum

    // cut ECin/P vs ECout/P
    ecio_p_cut = (ecin_p<=ISLEP_ECIN_P && ecout_p<=ISLEP_ECOUT_P);

    // diagonal cut on EC in vs ECout
    ecio_cut1 = ((ecout<ISLEP_ECOUT) && ((ecout-ISLEP_ECIO_SLOPE*ecin)<=0.0));

    // constant cut on EC in vs ECout
    ecio_cut2 = ((ecout>=ISLEP_ECOUT) && (ecin>=ISLEP_ECIN));

    if((ecio_cut1 || ecio_cut2) && !ecio_p_cut){ // final cut
      nReturnValue= this->Q(); // return 1(-1) for positron(electron)
    }
  }
  return(nReturnValue);
}

int clasParticle::IsLepton_EC()
{
  // Combined ECtot and ECio cut for IsLepton
  int nReturnValue = 0;

  if(this->IsLepton_ECtot() && this->IsLepton_ECio()){
    nReturnValue= this->Q(); // return 1(-1) for positron(electron)
  }else{
    nReturnValue= 0;
  }
  return(nReturnValue);
}

int clasParticle::IsLepton_CC()
{
  // Test the CC cut of IsLepton with the following cuts
  // 1) Number of photoelectrons >= CC_NPE
  // 2) ccPhiMatch - match phi with side of sector given by CCRC bank
  // 3) Timing cut between SC and CC (not implemented as of 2009)

  int nReturnValue = 0;
  int timing_cut = 0;
  float dt_offset;

  dt_offset = SC_CC_TIME_OFFSET;

  if(this->isSChit()){
    // set the number of photoelectrons
    float fPhotoElect = this->isCChit() ?  this->CChit().npe() : 0.0;

    // SC to CC time difference
    timing_cut = (fabs(this->sc_time() - this->ccTime() - dt_offset)<=SC_CC_TIME_CUT);

    //    if((fPhotoElect>=CC_NPE) && (this->ccPhiMatch()==1) && timing_cut){
    if((fPhotoElect>=CC_NPE) && (this->ccPhiMatch()==1)){
      nReturnValue= this->Q(); // return 1(-1) for positron(electron)
    }
  }
  return(nReturnValue);
}

int clasParticle::IsLepton_MM2()
{
  // Test the TOF cut of IsLepton
  int nReturnValue = 0;
  float fMassSqThr; // M2 threshold for good e using TOF

  if(this->isSChit()){
    // gets kinematics
    float fMoment = this->p().r();
    float fMassSq = this->scMassSquared();

    fMassSqThr = (fMoment*fMoment)/ISLEP_MSQ_SLOPE + ISLEP_MSQ_OFFSET;

    if((fMoment < ISLEP_MOM_THRESHOLD) && (abs(fMassSq)<=fMassSqThr)){
      nReturnValue= this->Q(); // return 1(-1) for positron(electron)
    }
  }
  return(nReturnValue);
}

int clasParticle::MaybeLepton()
{
  // in this method, the Lepton could only be electron or positron
  // with a different EC total energy cut

  bool bMaybe = false;   // assumes we start without an lepton
  int nReturnValue = 0;
  int ecTest = 0;

  // test EC,CC cut
  ecTest = (this->IsLepton_ECtot() || this->IsLepton_ECin2EC(0.95,1.05));
  if(ecTest && this->isCChit()){
    bMaybe = true;
  }

  // if EC,CC cut fails, try TOF mass squared
  if (!bMaybe && this->IsLepton_MM2()) {
    bMaybe = true;
  }

  // return 1(-1) if the particle is a positron(electron)
  if(bMaybe){
    nReturnValue= this->Q();
  }

  return(nReturnValue);
}

int clasParticle::IsLepton(int tryTOF)
{
  // in this method, the Lepton could only be electron or positron
  // with a different EC total energy cut

  bool bIsLepton = false;   // assumes we start without an lepton
  int nReturnValue = 0;

  // test EC,CC cut
  if(this->IsLepton_ECtot() && this->IsLepton_CC()){
    bIsLepton = true;
  }

  // if EC,CC cut fails, try TOF mass squared
  // if tryTOF=0, do not attempt the TOF MM2 id
  if (!bIsLepton && tryTOF && this->IsLepton_MM2()) {
    bIsLepton = true;
  }

  // return 1(-1) if the particle is a positron(electron)
  if(bIsLepton){
    nReturnValue= this->Q();
  }

  return(nReturnValue);
}

double clasEvent::g7EpEmMass()
{
  double PairMass = -1000.0;

  threeVec vtx;
  fourVec pair;
  clasParticle eplus, eminus, eplusOld, eminusOld;

  int emIndex, epIndex;
  int NnegLep = 0;
  int NposLep = 0;

  double r_pair, dz;
  double epdt, emdt;

  int rCut = 0;
  int dzCut = 0;
  int dtCut = 0;
  int SectorCut = 0;
  int IsLeptonCut = 0;
  int epIsLepton = 0;
  int emIsLepton = 0;

  //this->eLoss("g7a"); //energy loss correction, this may be redundant

  if(this->nTracks()>=2){ // need at least two charged tracks

    NnegLep = this->nIsLepton(Q(Electron)); // number of IsLepton e-'s
    NposLep = this->nIsLepton(Q(Positron)); // number of IsLepton e+'s

    if(NnegLep && NposLep){ // need both an e+ and e- to proceed
      emIndex = this->IndexIsLepton(Q(Electron)); // e- index
      eminusOld = this->cpLepton(Q(Electron),emIndex); // e- clasParticle

      epIndex = this->IndexIsLepton(Q(Positron)); // e+ index
      eplusOld = this->cpLepton(Q(Positron),epIndex); // e+ clasParticle

      // copy new clasParticles from old and reassign with lepton mass and id
      eplus = eplusOld;
      eminus = eminusOld;
      eplus.reAssign(Positron);
      eminus.reAssign(Electron);

      // set the e+e- vertex
      this->makeVertex();
      vtx = this->vert2(eplus,eminus);
      r_pair = sqrt(vtx.x()*vtx.x()+vtx.y()*vtx.y());
      rCut = (fabs(r_pair)<=G7A_R_LIM); // r cut

      dz =  eplus.z() - eminus.z();  // z difference between e+ and e-
      dzCut = (fabs(dz)<=G7A_DZ_LIM); // z difference cut

      SectorCut = (eplus.sec()!=eminus.sec()); // Different sector cut

      // check the IsLepton cuts for the e+ and e-
      epIsLepton = (eplus.IsLepton_ECtot() && eplus.IsLepton_CC());
      emIsLepton = (eminus.IsLepton_ECtot() && eminus.IsLepton_CC());
      IsLeptonCut = (epIsLepton && emIsLepton); // final IsLepton cut

      epdt = eplus.sc_time();
      emdt = eminus.sc_time();
      dtCut = (fabs(epdt-emdt)<=G7A_DT_LIM);

      // calculate the e+e- invariant mass
      if(rCut && dzCut && SectorCut && IsLeptonCut && dtCut){
    pair = eplus.p() + eminus.p(); // e+,e- 4V
    PairMass = ~pair; // calculate invariant mass for e+e-
      }
    }
  }
  return(PairMass);
}

int clasEvent::EpEm_EC_CC()
{
  clasParticle eplus, eminus, eplusOld, eminusOld;

  int emIndex, epIndex;
  int NnegLep = 0;
  int NposLep = 0;

  int ret = 0;
  int epIsLepton = 0;
  int emIsLepton = 0;

  //this->eLoss("g7a"); //energy loss correction, this may be redundant

  if(this->nTracks()>=2){ // need at least two charged tracks

    NnegLep = this->nIsLepton(Q(Electron)); // number of IsLepton e-'s
    NposLep = this->nIsLepton(Q(Positron)); // number of IsLepton e+'s

    if(NnegLep && NposLep){ // need both an e+ and e- to proceed
      emIndex = this->IndexIsLepton(Q(Electron)); // e- index
      eminusOld = this->cpLepton(Q(Electron),emIndex); // e- clasParticle

      epIndex = this->IndexIsLepton(Q(Positron)); // e+ index
      eplusOld = this->cpLepton(Q(Positron),epIndex); // e+ clasParticle

      // copy new clasParticles from old and reassign with lepton mass and id
      eplus = eplusOld;
      eminus = eminusOld;
      eplus.reAssign(Positron);
      eminus.reAssign(Electron);

      // check the IsLepton cuts for the e+ and e-
      epIsLepton = (eplus.IsLepton_ECtot() && eplus.IsLepton_CC());
      emIsLepton = (eminus.IsLepton_ECtot() && eminus.IsLepton_CC());
      ret = (epIsLepton && emIsLepton); // final IsLepton cut
    }
  }
  return(ret);
}

//------------------------------------------------------------------------
// The PID electron/positron id reproduced...
#define SIGMA_ETRK  0.01; /* momentum resolution for eletron ID*/
#define ETOT_EL_CUT  3.0; /* number of sigmas for EC total electron cut */
#define EIO_EL_CUT  3.0; /* number of sigmas for EC (in-out) electron cut */


int clasParticle::IsLeptonPID()
{
  int ret = 0;

  int tbtr_index = this->tbidptr()->track-1;
  int echb_index = this->tbidptr()->ec_id -1;
  int etot_cut = 0;
  int eio_cut = 0;
  int e_p_cut=0;
  float ECEnergy, Mo;
  float predicted_ECEnergy, predicted_EC_in_out;
  float sigma_e_whole, sigma_e_in_out;
  float sigma_e_tot, sigma_e_io;
  float diff_e_in_out;
  float factor, sigma_mo,sigma_etot_cut, sigma_eio_cut;
  clasECHB_t * ECHB = (clasECHB_t *)getBank(&bcs_, "ECHB");
  clasTBTR_t * TBTR = (clasTBTR_t *)getBank(&bcs_, "TBTR");
  echb_t * ECIn  = NULL;
  echb_t * ECOut = NULL;

  int ntab = 0;

  if (this->ev()->verbose()) {
    cerr << "ISLEPPID Q/momentum: " << this->Q() << "/" << this->p().V().r() <<
      "ec match " << this->tbidptr()->ec_stat << " cc match " <<  this->tbidptr()->cc_stat << endl;
    ntab++;
  }

  /* require  - charged track, match to ec & cc, ECHB & TBTR banks */
  if (ECHB && TBTR  && this->tbidptr()->ec_stat && this->tbidptr()->cc_stat){
    /* magnitude of momentum from tracking */
    Mo = v3mag(TBTR->tbtr[tbtr_index].p);
    /* create banks for EC inner and outer out of ECHB bank */
    ec_Whole2InOut(&ECHB->echb[echb_index], &ECIn, &ECOut);
    /* energy of EC total */
    ECEnergy = ECHB->echb[echb_index].e__hit;

    if(ECIn || ECOut){
      /* energy between EC inner and EC outer deposited energies */
      if(ECIn && !ECOut){
    diff_e_in_out = ECIn->e__hit;
      }
      if(!ECIn && ECOut){
    diff_e_in_out = -ECOut->e__hit;
      }
      if(ECIn && ECOut){
    diff_e_in_out = (ECIn->e__hit - ECOut->e__hit);
      }


      /* calculate predicted values for total energy and (inner-outer) */
      predicted_ECEnergy = predict_e(TBTR->tbtr[tbtr_index].p);
      predicted_EC_in_out = predict_e_in_out(TBTR->tbtr[tbtr_index].p);
      /* calculate predicted values for sigmas of the above quantities */
      sigma_e_whole = predict_sigma_e(TBTR->tbtr[tbtr_index].p);
      sigma_e_in_out = predict_sigma_e_in_out(TBTR->tbtr[tbtr_index].p);

      //    sigma_e_tot = sqrt(pow(SIGMA_ETRK*Mo,2) + pow(sigma_e_whole,2));
      sigma_mo = Mo*SIGMA_ETRK;
      sigma_e_tot = sqrt(pow(factor,2) + pow(sigma_e_whole,2));
      sigma_e_io = sqrt(pow(factor,2) + pow(sigma_e_in_out,2));

      sigma_etot_cut = sigma_e_tot*ETOT_EL_CUT;
      sigma_eio_cut = sigma_e_io*EIO_EL_CUT;
      /* diff of measured and predicted tot. energy must be <= sigma_e_tot */
      etot_cut = (fabs(ECEnergy-predicted_ECEnergy) <= sigma_etot_cut);
      /* diff of measured and predicted (in-out) energy must be <= sigma_e_io */
      eio_cut = (fabs(diff_e_in_out-predicted_EC_in_out) <= sigma_eio_cut);
      /* electron cut based on EC energies */
      if (this->ev()->verbose()) {
    for (int i = 0; i < ntab; ++i)
      cerr << "\t";
    cerr << "etot_cut " << fabs(ECEnergy-predicted_ECEnergy) << " <= " << sigma_etot_cut << " eio_cut "
         << fabs(diff_e_in_out-predicted_EC_in_out) << " <= " <<  sigma_eio_cut << endl;
    ntab++;
      }
      e_p_cut = (etot_cut && eio_cut);
    }

    /* if ec match was fiducial (GOOD_MATCH), use ec energy for cut */
    if (this->ev()->verbose()) {

      for (int i = 0; i < ntab; ++i) cerr << "\t";
    }
    if (e_p_cut) {
      if (this->ev()->verbose()) {
    cerr << "PID: Lepton!" << endl;
      }
      ret = 1;
    }
    else {
      if (this->ev()->verbose()) {
    cerr << "PID: not a Lepton!" << endl;
      }
    }
  }
  return(ret);
}
//-----------------------------------------------------------------------

int clasParticle::IsSCkaonG7()
{
  double kp, kSCen, kb;
  int ret = 0;
  if((this->isSChit() == 1)){
    int boolK = 0;
    switch(this->pid()){
    case KPlus:
      boolK = 1;
    case KMinus:
      boolK = 1;
    }
    if(boolK ==1){
      kp = this->p().V().r();
      kSCen = this->SChit().energy();
      kb = this->beta();
      bool scEfunc = (kSCen > 10.5 + 1/(kp - 0.1));
      if((scEfunc == 1) || kb > 0.9){
    ret =1;
      }
    }
  }
  return(ret);
}


//------------------------------------------------------------------------
clasParticle clasEvent::cp(int q,int index) const
{
    clasParticle ret;
  int n = 0;
  if (this->_TBID || 1) {
   std::list<clasParticle>::const_iterator p = this->_cp.begin();
       while ( p != this->_cp.end() ) {
      if (p->Q() == q)
        n++; // we count from 1
      if (n == index) {
        ret = *p;
        return(ret);
      }
      p++;
    }
  }
  throw((char *)"ParticleNotFound");
  return(ret);
}


std::list<clasParticle>::iterator clasEvent::xcp(Particle_t pid,int i)
{
  std::list<clasParticle>::iterator ret;
  int n = 0;
  if (this->_TBID || 1) {
    ret = this->_cp.begin();
       while ( ret != this->_cp.end() ) {
      if (ret->pid() == pid)
        n++;
      if (n == i) {
        return(ret);
      }
      ++ret;
    }
  }
  ret = this->_cp.end();
  throw((char *)"ParticleNotFound");
  return(ret);
}

clasParticle clasEvent::cp(Particle_t pid,int i) const
{
  clasParticle ret;
  int n = 0;
  if (this->_TBID || 1) {
   std::list<clasParticle>::const_iterator p = this->_cp.begin();
       while ( p != this->_cp.end() ) {
      if (p->pid() == pid)
        n++;
      if (n == i) {
        ret = *p;
        return(ret);
      }
      p++;
    }
  }
  throw((char *)"ParticleNotFound");
  return(ret);
}
clasParticle clasEvent::cp(int i) const
{
  clasParticle ret;
  i--;
  if (this->_TBID || 1) {
   std::list<clasParticle>::const_iterator p = this->_cp.begin();
   for (int j = 0; j < i; j++,p++)
     ;
   ret = *p;

  }
  return(ret);
}
// clasTaggedPhoton

clasTaggedPhoton clasEvent::ntag(int i) const
{
  clasTaggedPhoton ret;


     std::list<clasTaggedPhoton>::const_iterator p = this->_tag.begin();
   for (int j = 0; j < i; j++,p++)
     ;
   ret = *p;


  return(ret);
}

double clasEvent::ssTAGRcorrection(double epho, int Eid)
{
  double newE = epho;
  double correction[767] = {
1.00064, 1.00074, 1.00106, 1.00094, 1.00099, 1.00098, 1.00104, 1.00116, 1.00121, 1.00139, 1.00129, 1.00163, 1.00132, 1.00152, 1.00162, 1.00156, 1.00189, 1.00207, 1.00246, 1.00245, 1.00190, 1.00205, 1.00202, 1.00233, 1.00271, 1.00227, 1.00232, 1.00239, 1.00253, 1.00281, 1.00263, 1.00305, 1.00269, 1.00284, 1.00281, 1.00307, 1.00291, 1.00280, 1.00301, 1.00277, 1.00261, 1.00284, 1.00360, 1.00323, 1.00273, 1.00280, 1.00276, 1.00329, 1.00295, 1.00306, 1.00295, 1.00292, 1.00261, 1.00323, 1.00332, 1.00318, 1.00304, 1.00318, 1.00329, 1.00344, 1.00323, 1.00364, 1.00350, 1.00326, 1.00362, 1.00298, 1.00338, 1.00346, 1.00317, 1.00353, 1.00347, 1.00378, 1.00394, 1.00368, 1.00341, 1.00365, 1.00331, 1.00336, 1.00368, 1.00366, 1.00320, 1.00236, 1.00393, 1.00318, 1.00317, 0.00000, 0.00000, 0.00000, 1.00365, 1.00377, 1.00380, 1.00375, 1.00371, 1.00373, 1.00387, 1.00359, 1.00378, 1.00369, 1.00382, 1.00340, 1.00389, 1.00377, 1.00374, 1.00403, 1.00421, 1.00396, 1.00368, 1.00416, 1.00454, 1.00431, 1.00400, 1.00388, 1.00342, 1.00379, 1.00395, 1.00424, 1.00410, 1.00387, 1.00407, 1.00249, 0.99929, 1.00201, 1.00393, 1.00611, 1.00824, 1.00279, 1.00419, 1.00408, 1.00476, 1.00425, 1.00432, 1.00433, 1.00435, 1.00435, 1.00439, 1.00438, 1.00428, 1.00413, 1.00416, 1.00466, 1.00464, 1.00455, 1.00459, 1.00409, 1.00405, 1.00416, 1.00454, 1.00425, 1.00454, 1.00478, 1.00454, 1.00467, 1.00437, 1.00451, 1.00420, 1.00419, 1.00470, 1.00426, 1.00441, 1.00470, 1.00471, 1.00436, 1.00440, 1.00435, 1.00448, 1.00410, 1.00438, 1.00471, 1.00510, 1.00451, 1.00444, 1.00420, 1.00455, 1.00442, 1.00427, 1.00423, 1.00422, 1.00450, 1.00437, 1.00449, 1.00485, 1.00454, 1.00459, 1.00477, 1.00442, 1.00488, 1.00481, 1.00500, 1.00480, 1.00477, 1.00465, 1.00442, 1.00437, 1.00440, 1.00487, 1.00450, 1.00458, 1.00442, 1.00453, 1.00473, 1.00498, 1.00506, 1.00512, 1.00477, 1.00520, 1.00423, 1.00450, 1.00472, 1.00448, 1.00502, 1.00472, 1.00468, 1.00483, 1.00457, 1.00506, 1.00486, 1.00474, 1.00481, 1.00440, 1.00474, 1.00446, 1.00476, 1.00462, 1.00432, 1.00390, 1.00445, 1.00441, 1.00460, 1.00478, 1.00440, 1.00447, 1.00451, 1.00429, 1.00461, 1.00456, 1.00476, 1.00423, 1.00503, 1.00493, 1.00472, 1.00433, 1.00390, 1.00414, 1.00431, 1.00418, 1.00426, 1.00442, 1.00420, 1.00426, 1.00431, 1.00434, 1.00422, 1.00392, 1.00420, 1.00414, 1.00464, 1.00377, 1.00401, 1.00393, 1.00392, 1.00424, 1.00409, 1.00371, 1.00407, 1.00379, 1.00370, 1.00388, 1.00306, 1.00298, 1.00306, 1.00318, 1.00298, 1.00298, 1.00292, 1.00334, 1.00291, 1.00296, 1.00335, 1.00303, 1.00313, 1.00307, 1.00349, 1.00352, 1.00358, 1.00407, 1.00397, 1.00335, 1.00388, 1.00346, 1.00396, 1.00363, 1.00399, 1.00366, 1.00387, 1.00383, 1.00385, 1.00331, 1.00355, 1.00378, 1.00399, 1.00397, 1.00396, 1.00370, 1.00350, 1.00403, 1.00357, 1.00411, 1.00383, 1.00409, 1.00411, 1.00391, 1.00357, 1.00397, 1.00408, 1.00359, 1.00396, 1.00340, 1.00383, 1.00361, 1.00372, 1.00341, 1.00407, 1.00352, 1.00436, 1.00425, 1.00413, 1.00369, 1.00397, 1.00421, 1.00441, 1.00429, 1.00383, 1.00436, 1.00419, 1.00424, 1.00428, 1.00405, 1.00415, 1.00422, 1.00430, 1.00429, 1.00461, 1.00408, 1.00385, 1.00429, 1.00408, 1.00422, 1.00469, 1.00457, 1.00463, 1.00456, 1.00461, 1.00430, 1.00456, 1.00459, 1.00449, 1.00446, 1.00445, 1.00434, 1.00419, 1.00449, 1.00445, 1.00487, 1.00483, 1.00505, 1.00447, 1.00446, 1.00483, 1.00473, 1.00495, 1.00500, 1.00528, 1.00370, 1.00371, 1.00377, 1.00392, 1.00432, 1.00337, 1.00351, 1.00365, 1.00449, 1.00357, 1.00342, 1.00396, 1.00392, 1.00405, 1.00470, 1.00450, 1.00470, 1.00430, 1.00493, 1.00440, 1.00488, 1.00476, 1.00420, 1.00449, 1.00465, 1.00463, 1.00434, 1.00434, 1.00469, 1.00488, 1.00467, 1.00422, 1.00463, 1.00432, 1.00467, 1.00440, 1.00351, 1.00372, 1.00396, 1.00374, 1.00414, 1.00390, 1.00416, 1.00453, 1.00445, 1.00371, 1.00366, 1.00358, 1.00384, 1.00377, 1.00355, 1.00307, 1.00305, 1.00326, 1.00288, 1.00370, 1.00383, 1.00335, 1.00312, 1.00274, 1.00316, 1.00308, 1.00332, 1.00377, 1.00348, 1.00245, 1.00260, 1.00273, 1.00254, 1.00334, 1.00324, 0.00000, 0.99602, 0.99955, 1.00300, 1.00635, 1.01059, 0.00000, 1.00328, 1.00337, 1.00322, 1.00334, 1.00332, 1.00320, 1.00347, 1.00364, 1.00341, 1.00305, 1.00360, 1.00340, 1.00339, 1.00383, 1.00220, 1.00221, 1.00304, 1.00299, 1.00298, 1.00225, 1.00234, 1.00243, 1.00251, 1.00277, 1.00250, 1.00298, 1.00269, 1.00254, 1.00265, 1.00312, 1.00324, 1.00339, 1.00235, 1.00328, 1.00372, 1.00280, 1.00301, 1.00230, 1.00102, 1.00087, 1.00134, 1.00189, 1.00196, 1.00204, 1.00245, 1.00166, 1.00173, 1.00124, 1.00058, 1.00108, 1.00115, 1.00179, 1.00203, 1.00220, 1.00192, 1.00148, 1.00149, 1.00128, 1.00113, 1.00168, 1.00179, 1.00103, 1.00125, 1.00151, 1.00120, 1.00138, 1.00179, 1.00173, 1.00208, 1.00170, 1.00210, 1.00217, 1.00235, 1.00282, 1.00202, 1.00162, 1.00148, 1.00176, 1.00145, 1.00206, 1.00164, 1.00139, 1.00171, 1.00140, 1.00139, 1.00070, 1.00069, 1.00133, 1.00152, 1.00131, 1.00148, 1.00162, 1.00157, 1.00123, 1.00083, 1.00022, 1.00034, 1.00108, 1.00033, 1.00066, 1.00083, 1.00121, 1.00187, 1.00151, 1.00124, 1.00138, 1.00121, 1.00187, 1.00193, 1.00204, 1.00194, 1.00236, 1.00237, 1.00338, 1.00019, 0.99939, 1.00001, 0.99996, 0.99764, 0.99699, 0.99763, 0.99730, 0.99777, 0.99735, 0.99847, 0.99828, 0.99869, 0.99958, 0.99954, 1.00001, 1.00076, 1.00022, 1.00025, 0.99980, 1.00179, 1.00030, 1.00118, 1.00195, 1.00201, 1.00138, 1.00157, 1.00107, 1.00160, 1.00151, 1.00129, 1.00157, 1.00149, 1.00189, 1.00210, 1.00261, 1.00278, 1.00119, 1.00141, 1.00172, 1.00129, 1.00169, 1.00141, 1.00126, 1.00188, 1.00163, 1.00120, 1.00186, 1.00155, 1.00183, 1.00141, 1.00128, 1.00114, 1.00102, 1.00039, 1.00081, 1.00105, 1.00087, 1.00102, 1.00051, 1.00097, 1.00081, 1.00150, 1.00116, 1.00146, 1.00185, 1.00198, 1.00286, 1.00270, 1.00211, 1.00223, 1.00221, 1.00217, 1.00192, 1.00221, 1.00242, 1.00202, 1.00226, 1.00220, 1.00091, 1.00108, 1.00154, 1.00107, 1.00074, 1.00140, 1.00070, 1.00043, 1.00081, 1.00092, 1.00079, 1.00072, 1.00036, 1.00005, 1.00077, 1.00122, 1.00072, 1.00121, 1.00089, 1.00066, 1.00047, 1.00010, 1.00064, 1.00040, 1.00026, 1.00027, 1.00096, 1.00048, 0.99972, 1.00003, 1.00074, 1.00110, 1.00170, 1.00150, 1.00196, 1.00237, 1.00230, 1.00140, 1.00204, 1.00181, 1.00304, 1.00290, 1.00240, 1.00137, 1.00197, 1.00233, 1.00196, 1.00130, 1.00118, 1.00048, 0.99974, 0.99922, 0.99875, 0.99836, 0.99845, 0.99828, 0.99985, 0.99908, 0.99960, 0.99959, 0.99974, 1.00015, 1.00029, 1.00014, 0.99953, 1.00070, 0.99994, 1.00023, 1.00007, 0.99998, 0.99993, 1.00005, 0.99994, 1.00015, 1.00045, 1.00065, 1.00118, 1.00163, 1.00228, 1.00289, 1.00386, 1.00506, 1.00497, 1.00725, 1.00787, 1.00934, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000, 1.00000
  };
  newE = epho*correction[Eid-1];
  return newE;
}

double clasEvent::vkTAGRcorrection(float epho)
{
  double newE = 0.0;
//  cerr << "clasEvent: " << epho << endl;
  newE = c_taggerenergy(this->run(), epho);
  return newE;
}

double clasEvent::lgTAGRcorrection(double epho, int Eid)
{
  double correction[730] = {
0.999641274898174, 0.999830817984, 0.999481267887484, 0.999896584374392, 0.999840131216698, 1.00002861422892, 0.99981393105743, 0.999802369983202, 0.999755484500896, 0.999989876023443, 0.999618790602173, 1.00024104816674, 0.99987971680348, 1.00028772655589, 0.999950191191486, 1.00028101520189, 1.00015864207253, 1.0005822845877, 1.00033537012653, 1.00089146591762, 1.00022029892858, 1.00075221149229, 1.0005786381311, 1.000793893761, 1.00090213956517, 1.00074190891072, 1.00069584821157, 1.00085049116408, 1.00098015124109, 1.00109553955547, 1.00112097550862, 1.00105972777493, 1.00105591563437, 1.00115312724208, 1.00117076475435, 1.00135129793796, 1.00137607070817, 1.00132807873845, 1.00123614016835, 1.00150065246828, 1.00104744111881, 1.00131561068325, 1.00157322704166, 1.00118737725681, 1.00153678145304, 1.00143204276426, 1.00138193088654, 1.00193812854717, 1.00153477543311, 1.0015356067766, 1.00129634879942, 1.00122634956425, 1.00145569779834, 1.00156690083765, 1.0017152819096, 1.00177099262762, 1.00144381810936, 1.00154732684385, 1.00142354102094, 1.00173587971582, 1.00126058928961, 1.00165934358672, 1.00145590822282, 1.00152166296161, 1.0016512875352, 1.00158930633475, 1.00175663238933, 1.00173197321879, 1.00154849559581, 1.00163650414548, 1.00173597753255, 1.00188868001114, 1.00168135243705, 1.00176171311683, 1.00183838119491, 1.00185404450701, 1.00174780579804, 1.00185089017299, 1.00188157492601, 1.00177059541264, 1.00190783204485, 1.00182695494152, 1.0019722967706, 1.00177978691756, 1.00142484069026, 1.00169683240959, 1.00173410479916, 1.00176340220185, 1.00151057879155, 1.00188013021027, 1.00193649821708, 1.00185310416206, 1.00199100271754, 1.00188382016423, 1.00172523821953, 1.00186685051624, 1.00172739868413, 1.00200630087683, 1.00183909150693, 1.001863337191, 1.00207213086984, 1.00218673509816, 1.00253784402641, 1.00218804261433, 1.00212152616746, 1.00206236174821, 1.00146516544491, 1.00178113442591, 1.00217322089274, 1.00208533220267, 1.00202092516321, 1.00217939743502, 1.00237798688689, 1.00199069286199, 1.00239704785759, 1.00212029498631, 1.00168632902282, 1.00206937882767, 1.00138912893146, 1.002556485143, 0.997038343048062, 0.999985681303091, 1.00209226333791, 1.00438921760459, 1.00725788076018, 1.00844024992764, 1.00266217374831, 1.00233929157037, 1.00188920420059, 1.00194675104644, 1.00215143874352, 1.0020746291126, 1.00248744629328, 1.00244335417642, 1.00277610978233, 1.0023140625651, 1.00309101676408, 1.00218294732938, 1.00184090993412, 1.00207539749345, 1.00238900816942, 1.0023390096752, 1.00252100085315, 1.00234640014115, 1.00270299258048, 1.00243249327612, 1.00246918785125, 1.00223910934487, 1.0022253213011, 1.00208164264986, 1.00246505324001, 1.00235458770445, 1.0025041804465, 1.00257805367009, 1.00231590180088, 1.00200642211389, 1.00242977184288, 1.00233896268606, 1.00261061515066, 1.00254052502611, 1.00300466154996, 1.00235010879749, 1.00225799762122, 1.00229319698412, 1.00219200659726, 1.00213310002432, 1.00253535512089, 1.0025364035848, 1.00276865686503, 1.00258104703812, 1.00355654696005, 1.00262953899363, 1.00092057515793, 1.0013241688215, 1.00187552085365, 1.00160667462389, 1.0023154841747, 1.00223292926915, 1.00213221332597, 1.00194464017232, 1.00228288107956, 1.00205531803337, 1.00242056130802, 1.00231883636413, 1.00298733332673, 1.00321471813739, 1.00339872110182, 1.00346009251901, 1.00366228135306, 1.00292222331662, 1.00298285379561, 1.0025446458403, 1.0027592620114, 1.00281071316829, 1.00265717750814, 1.00272197110678, 1.00284438934087, 1.00282552483432, 1.0025337394289, 1.00271384498025, 1.00246086315304, 1.00278914402347, 1.00262501574106, 1.00238848510773, 1.00245691155543, 1.0025434181122, 1.00269793601942, 1.00270352216732, 1.00268188523343, 1.00261000587009, 1.00244769897881, 1.00246584720463, 1.00250266967154, 1.00244361047454, 1.00272157317643, 1.0027537109293, 1.0025572845762, 1.00289113876448, 1.0027539966763, 1.00296460964494, 1.00258365015187, 1.00249060370561, 1.00272961734937, 1.00246580487673, 1.00223810779109, 1.00278230954186, 1.00256838953865, 1.00256665570396, 1.00268625245203, 1.00256838574757, 1.00265080143575, 1.00239664507843, 1.00249751818129, 1.0024625010744, 1.00276476525386, 1.00290816708898, 1.00276542712357, 1.00307846366309, 1.00310000208052, 1.00280991131461, 1.00235176030787, 1.00236692460593, 1.00253881570576, 1.00233761405665, 1.00267137717981, 1.00236790355178, 1.00226882298317, 1.00235967990973, 1.00235058604209, 1.00222195405789, 1.00232750280213, 1.00239926723282, 1.00223576113324, 1.00243776823109, 1.00241832472536, 1.00248566421521, 1.00241760395362, 1.00236860232099, 1.00243058331585, 1.00215765853909, 1.00225892067088, 1.00230637711839, 1.00206097882732, 1.00192227341064, 1.00160678504155, 1.00168247371403, 1.00183682264746, 1.00143090587667, 1.00133884385289, 1.00145885884217, 1.00146767033724, 1.00146054702793, 1.00160566866382, 1.00158795447569, 1.00161378568048, 1.00192023762938, 1.00175500165037, 1.0017838036465, 1.00126866612499, 1.00163159234806, 1.00128788639951, 1.0015566757643, 1.00145136912216, 1.00178542190849, 1.00157150699198, 1.00148428537688, 1.00132506790698, 1.00152765967223, 1.00154598492292, 1.00132938151052, 1.00156567100615, 1.00145421421087, 1.00130754994605, 1.00141252159486, 1.00191709078117, 1.00198757664145, 1.00198620577258, 1.0017809006568, 1.00178717133582, 1.00199936355792, 1.00190644008723, 1.00238580348467, 1.00194957363049, 1.00176694345133, 1.00197263413742, 1.00215126813157, 1.00217433627706, 1.00247900849958, 1.00230330077272, 1.00222035599417, 1.0018998191087, 1.00181928088052, 1.00227683954939, 1.002186048064, 1.00198099349966, 1.00216841614581, 1.00226132197492, 1.00243249012278, 1.00262639180763, 1.00211965349232, 1.00201668103105, 1.00232477311487, 1.00196345884682, 1.00228287428044, 1.00278468044232, 1.00247950162958, 1.00219571785539, 1.00225251166577, 1.00266392870375, 1.00252427433714, 1.00283519870919, 1.00234958203714, 1.0027762632807, 1.00262709410647, 1.00259459858655, 1.00249622163671, 1.00229820706578, 1.00238248288198, 1.00278719218773, 1.00220128425082, 1.0026094835054, 1.0025748514271, 1.00225066890238, 1.00240172411992, 1.00242714368122, 1.00236852488486, 1.00238749744431, 1.00277951840867, 1.00294974811514, 1.00257919977129, 1.00307502201993, 1.00295297456866, 1.00269140649524, 1.0026699253096, 1.002993598932, 1.00244190853347, 1.00247769146262, 1.00271932965475, 1.00291104630041, 1.0021867880099, 1.00245544305852, 1.00256718845195, 1.0030292875573, 1.00279357325748, 1.00302648889098, 1.00296037396377, 1.00292384057232, 1.00241663047658, 1.00244049037963, 1.00272015129411, 1.0028690018489, 1.00285423530041, 1.00319844709983, 1.00288406625297, 1.0025000584871, 1.00258153581887, 1.00271821489395, 1.00245986715326, 1.00305243914686, 1.00282938715381, 1.00339822873732, 1.00261506389372, 1.00284213636765, 1.00284306538463, 1.00257977204265, 1.00275192851544, 1.00300990119525, 1.00301444356389, 1.00319674094407, 1.00336028625042, 1.0028324421327, 1.00230231488893, 1.00334166785403, 1.00294499150005, 1.00299590698914, 1.00288233459849, 1.00331572483661, 1.00292498690281, 1.00307104790626, 1.00296394547562, 1.00274971953403, 1.00242654740128, 1.00299654584348, 1.00265250851786, 1.00283166053738, 1.00271048227536, 1.00285974239172, 1.0022676603944, 1.00258072875675, 1.00300254036298, 1.00277091683564, 1.00323344538724, 1.00273176949194, 1.00347746958098, 1.00340662302745, 1.00344317279769, 1.00395078636643, 1.00372452773331, 1.00393775985304, 1.00272542892134, 1.00373481170533, 1.00329323415826, 1.00338928094862, 1.00279248029036, 1.00378356279128, 1.00355726309339, 1.00324481378872, 1.00346759355239, 1.00380441142854, 1.00345929965744, 1.00281081043984, 1.00326656476925, 1.00311694056125, 1.00279801461008, 1.0034729230051, 1.00336774569425, 1.0031298068136, 1.00357757452969, 1.00318195763492, 1.00300015757169, 1.00288004504023, 1.0035698784577, 1.0026685574112, 1.00453575966538, 0.994941130968553, 0.999562956579362, 1.00316185248372, 1.0067675953839, 1.01194421350097, 1.01384983662357, 1.00427955117444, 1.00308121486909, 1.00309787264949, 1.00311593960074, 1.00350116119278, 1.00351329327011, 1.00360732433944, 1.00314136910996, 1.00280940157463, 1.00308461065344, 1.00339362417606, 1.00332360606165, 1.00325331321815, 1.00339554168767, 1.00248189967142, 1.00289134958277, 1.00314057153744, 1.00325735823945, 1.0035229091622, 1.0025742040658, 1.00340163765644, 1.00286783609465, 1.00277561120003, 1.00302736011711, 1.00291459656208, 1.00296661993992, 1.00268511092069, 1.00243831551786, 1.00258578481754, 1.00276839685197, 1.00290713668439, 1.0031687167296, 1.00301948738198, 1.00253694191908, 1.0026253184742, 1.00302454688179, 1.00295848417562, 1.002735230689, 1.00277788305929, 1.00266314649957, 1.0030539395854, 1.00270704893635, 1.002932246638, 1.00272496645537, 1.00267158170484, 1.00266682356195, 1.00284029451709, 1.00251638381473, 1.00245669753158, 1.00237252988514, 1.00244528170057, 1.00293401303083, 1.00267739167777, 1.00276303460425, 1.00300106880065, 1.00284355261017, 1.00253622001507, 1.00217202181362, 1.0023439062408, 1.00242004695785, 1.00283379042275, 1.00198212694651, 1.00133358939362, 1.00168369957431, 1.00217784167697, 1.00209866634332, 1.00250151099484, 1.00204304441099, 1.0019598431068, 1.00168591971603, 1.00183042785718, 1.00178317622979, 1.0020059148103, 1.00194285181194, 1.00135367714298, 1.00146349086244, 1.00182897021637, 1.00166636929029, 1.00153656698761, 1.00184152021772, 1.00092681130467, 1.00116010303266, 1.00144708219652, 1.00178194557784, 1.00101776374349, 1.00102020212332, 1.00083491097274, 1.00119041621229, 1.00161816075698, 1.00167105565522, 1.00190263765167, 1.00104995671301, 1.00125792846865, 1.00072199165612, 1.00132622797639, 1.00052174359525, 1.00105024293589, 1.00071741684656, 1.00094234937256, 1.00052681335584, 1.0010792414168, 1.00102058380896, 1.00103564880451, 1.00101804408945, 1.00133207200339, 1.00145972344079, 1.00144277378058, 1.00110049753171, 1.00045566064412, 1.00076049850895, 1.0003134416223, 1.00096284479616, 0.999942866371997, 1.00051939635592, 1.00076389545861, 1.00101121500067, 1.00065896488286, 1.00041845114447, 1.00064346696408, 1.00023039697912, 1.0002017020166, 1.00038708481196, 1.00095242929086, 0.99956126264413, 0.99971756048419, 0.999402464620778, 0.999388879187441, 0.999734379653369, 0.999543971258455, 0.999801012152871, 1.00030807237172, 0.999809144263696, 1.00012033944413, 0.999657588387471, 1.00217814422306, 1.00053011067217, 1.00294294058598, 1.00112355212328, 1.00162377414456, 0.999914964273406, 1.00081925331809, 1.00065318306175, 1.00043115347986, 0.999384531235611, 0.999218021740793, 0.998430374685021, 0.997102311862412, 0.999563667802406, 0.999746711598677, 1.00022070443182, 1.00103218569966, 1.00036377121028, 1.00023242613361, 0.999040920282908, 1.00032119049452, 1.0011161584652, 1.00033879849245, 1.00105143324355, 1.00178570337307, 1.00129181029661, 1.00158950450176, 1.00135631180212, 1.00151631614858, 1.00207071682034, 1.00202264789426, 1.00171941538193, 1.00204665350655, 1.00166637994736, 1.00132582422116, 1.0011942350823, 1.00212962273215, 1.00153331034747, 1.0019656195491, 1.00190567659892, 1.00203327176543, 1.00214342492313, 1.00163178846593, 1.0021915877352, 1.0022574988224, 1.00169562273656, 1.00100251868902, 1.00170753941223, 1.00183546347837, 1.00142579219071, 1.00129536051365, 1.00093535350993, 1.00140436338504, 1.00184365008101, 1.00084781503355, 1.00140621294226, 1.00156973318217, 1.00194545412449, 1.00240262639336, 1.0012776292076, 1.00154715412735, 1.00165269646916, 1.00143096097072, 1.00110965583022, 1.00121697685254, 1.00090168275173, 1.00022889992616, 1.00125751895149, 1.00102258931982, 1.00193380204493, 1.00111193961879, 1.00048415507505, 1.00108166923008, 1.00101649481903, 1.00139555704961, 1.00156947288859, 1.00210971755531, 1.00102439929823, 1.00135161767613, 1.00083214043852, 1.00089450516262, 1.00195591710766, 1.0026555972579, 1.00191485481737, 1.00177782530592, 1.00208970535986, 1.00174588837882, 1.00157787042307, 1.00122405200101, 1.00186942548132, 1.00235006376911, 1.00175129380956, 1.00167410147386, 1.00128424955551, 1.00170185712282, 0.9997454935449, 0.997868558934767, 0.999789191426446, 0.999911484669657, 1.00128683091767, 1.00107466865127, 1.00040946672624, 1.00065647022556, 1.00135658429789, 0.998055371881578, 0.999069045532601, 0.999381810163335, 0.999704139451732, 1.00083455360146, 0.999997162713105, 1.00133211398258, 1.00093555786445, 1.00119646161542, 1.00131010254022, 1.00050157365759, 1.00247365122122, 1.00121482877229, 1.00150946749215, 1.00041541795563, 1.00125869559451, 1.00112550753461, 1.00039397589271, 1.00133638171424, 0.999969872375385, 1.00025002421231, 1.00134845214318, 1.00122580633868, 1.00038402436287, 0.999882701374192, 1.00121017749986, 1.00055133258728, 0.999911628126739, 1.000386904226, 1.00136969866904, 1.00062715311066, 1.00048660758688, 1.00156595099919, 1.00149384222675, 1.00138439554922, 1.00055091293654, 0.999390480788576, 1.00009526324921, 1.00125735294308, 0.998466639717834, 0.978671459707056, 0.997126518768002
  };
  double newE;
  if(Eid>730) newE = epho;
  //else newE = epho + (epho*correction[Eid-1]);
  else newE = epho*correction[Eid-1];
  return newE;
}


double clasEvent::mwTAGRcorrection(double epho)
{
    // beam offsets only calculated for 2.4 and 3.1 from g1c
  // runID = 1 is 3.115 from g1c
  // runID = 2 is 2.4 from g1c
  // runID = 3 is 4.016 from g11a
  // any other runID gets a beam offset of 0.0
  double E0 = 0;
  int runID = 0;
  switch (this->runPeriod()) {
  case g1c:
   if(this->run() >= 21763)
   {
     E0 = 2.445;
     runID = 2;
   }
   else
   {
         E0 = 3.115;
         runID = 1;
   }
    break;
  case g11a:
    E0 = 4.016;
    runID = 3;
    break;
  default:
    E0 = 3.000;
    runID = 0;
    break;
}

    double newE = TAGRcor(epho, E0, runID);
    return newE;
}

// clasParticle code -----------------------------------------------------------------------------------------------------------

clasParticle& clasParticle::operator=(const clasParticle& particle)
{
  this->_ev = particle.ev();
  this->_bcs = particle.bcs();
  this->_pid = particle.pid();
  this->_p = particle.p();
  this->_pos = particle.pos();
  this->_tagrIndex = particle.tagr_id() - 1;
  this->_tbidIndex = particle.tbid();
  this->_ccidIndex = particle.ccid();
  this->_tbidptr = particle.tbidptr();
  this->_NPE = particle.nPhe();
  this->_index = 0;
  this->_sec = particle.sec();
  this->_qpid = particle.quality();
  this->_qtrk = particle.qtrk();
  this->_pidflag = particle.pidFlag();
  this->_gpidIndex = particle.gpidIndex();

  return(*this);
}

echb_t *clasParticle::echb()
{
  echb_t *ret = NULL;
  if (this->IsPhoton()) {
    int echbIndex = this->_tbidptr->ec_id - 1; // c index
    clasECHB_t *ECHB = (clasECHB_t *)getBank(this->_bcs,"ECHB");
    if (ECHB) {
      if (echbIndex < ECHB->bank.nrow)
    ret = &ECHB->echb[echbIndex];
    }
  }
  return(ret);
}


threeVec clasParticle::scPos()
{
    clasTDPL_t *TDPL = (clasTDPL_t *)getGroup(this->_bcs,"TDPL",this->sec());
    threeVec ret(-1000.0,-1000.0,-1000.0);

    if (TDPL) {
        int plane = this->scPlane();
        int track = this->trackNo();
        for (int k = 0; k < TDPL->bank.nrow; ++k) {
            if (plane == TDPL->tdpl[k].trk_pln % 100) {
                if ( TDPL->tdpl[k].tlen < 999.0) {
                    if (TDPL->tdpl[k].trk_pln/100 == track) {
                        ret = threeVec( TDPL->tdpl[k].pos.x, TDPL->tdpl[k].pos.y, TDPL->tdpl[k].pos.z);
                        break;
                    }
                }
            }
        }
    }
        return(ret);
}


int clasParticle::scPlane()
{

  //  std::cerr << " I am here scPlane " << std::endl;
  // cp tbid : ungleich geant id
  // std::cerr << " cp tbid  : " << this->tbid() <<std::endl;
  // cp _tbidIndex
  //std::cerr << " cp _tbidIndex : " << this->_tbidIndex <<std::endl;

  int ret = -1000;
  clasTBID_t *TBID = (clasTBID_t *)getBank(this->_bcs,"TBID");


  if (TBID) {
    tbid_t *tbid = &TBID->tbid[this->_tbidIndex -1];
    int sector = tbid->sec;
    //std::cerr << " tbid track : " << tbid->track <<std::endl;  // tbid->track should be used
    clasTDPL_t *TDPL = (clasTDPL_t *)getGroup(this->_bcs,"TDPL",sector);
    if (TDPL) {

      for (int i = NSC_MIN; i <= NSC_MAX; ++i) {
    for (int k = 0; k < TDPL->bank.nrow; ++k) {
      tdpl_t *tdpl = &TDPL->tdpl[k];
      if ((tdpl->tlen < 999.0) && (i+1 == tdpl->trk_pln % 100) && (tbid->track == tdpl->trk_pln/100)) {
        ret = i+1;
        break;
      }
    }
      }
    }
  }
  return(ret);
}

//ut
int clasParticle::scPaddleId()
{
  int ret = -10000;
  clasTBID_t *TBID = (clasTBID_t *)getBank(this->_bcs,"TBID");
  if (TBID) {
    tbid_t *tbid = &TBID->tbid[this->_tbidIndex -1];

    if (tbid->sc_id) {
      int sector = this->sec();
      clasSCRC_t *SCRC = (clasSCRC_t *)getGroup(this->_bcs,"SCRC",sector);

      if (SCRC) {
    scrc_t *scrc = &SCRC->scrc[tbid->sc_id -1];
    ret = scrc->id;
      }
    }
  }
  return(ret);
}


int clasParticle::st_id()

{
  int ret = -10000;
  clasTBID_t *TBID = (clasTBID_t *)getBank(this->_bcs,"TBID");
  if (TBID && this->st_stat()) {
    tbid_t *tbid = &TBID->tbid[this->_tbidIndex -1];
    ret = tbid->st_id;
  }



  return(ret);
}

int clasParticle::st_paddle()

{
  int ret = -10000;
  clasSCR_t *SCR = (clasSCR_t *) getBank(this->_bcs,"SCR ");
  clasTBID_t *TBID = (clasTBID_t *)getBank(this->_bcs,"TBID");
  if (TBID && this->st_stat()) {
    tbid_t *tbid = &TBID->tbid[this->_tbidIndex -1];
    ret = SCR->scr[tbid->sc_id - 1].id;
  }



  return(ret);
}

threeVec clasParticle::stPos()
{
  threeVec ret(-1000.0,-1000.0,-1000.0);
  clasSCR_t *SCR = (clasSCR_t *) getBank(this->_bcs,"SCR ");
  clasTBID_t *TBID = (clasTBID_t *)getBank(this->_bcs,"TBID");
  if (TBID && this->st_stat()) {
    tbid_t *tbid = &TBID->tbid[this->_tbidIndex -1];
    vector3_t *v =&SCR->scr[tbid->sc_id -1].pos;
    ret.set(v->x,v->y,v->z);
  }



  return(ret);

}



//ut:  get sector information from the tbid-bank: is already included in
//     clasparticle

int clasParticle::scPaddleId_sector()
{
  return(this->sec());
}

/////

double clasParticle::pathlenST_SC()
{
  double tlen = -1000.0;
  clasSTR_t *STR = (clasSTR_t *) getBank(this->_bcs,"STR ");
  if (STR && this->_tbidptr)
    tlen = pathlen2sc((bid_t *)this->_tbidptr,(hdpl_t *)tbid2tdpl(this->_tbidptr)) - STR->str[this->_tbidptr->st_id - 1].st_l;
  return(tlen);

}


double clasParticle::scTOF()
{
  double ret = -1000.0;
  clasTBID_t *TBID = (clasTBID_t *)getBank(this->_bcs,"TBID");
  if (TBID) {
    tbid_t *tbid = &TBID->tbid[this->_tbidIndex -1];
    ret = tbid->sc_time - tbid->vtime;
  }
  return(ret);
}
double clasParticle::scTOFexpected()
{
  double ret = -1000.0;
  ret = this->scPathLen()/(this->Beta() * LIGHT_SPEED);
  return(ret);
}
double clasParticle::scPathLen()
{
  double ret = -1000.0;
  clasTBID_t *TBID = (clasTBID_t *)getBank(this->_bcs,"TBID");
  clasTBTR_t *TBTR = (clasTBTR_t *)getBank(this->_bcs,"TBTR");

  if (!this->ev()->configSCGStatus()) {
    this->ev()->ConfigSCG();
  }

  if (TBID && TBTR) {
    tbid_t *tbid = &TBID->tbid[this->_tbidIndex -1];
    int sector = tbid->sec;
    int trk_ind=tbid->track;

    clasTDPL_t *TDPL = (clasTDPL_t *)getGroup(this->_bcs,"TDPL",sector);
    if (TDPL) {
      tdpl_t *tdpl = tbtr2tdpl(&(TBTR->tbtr[trk_ind-1]));
      ret = pathlen2sc((bid_t *)tbid,(hdpl_t *)tdpl);
    }
  }
  return(ret);
}

double clasParticle::sc_len()
{

 if (this->_pidflag == GPID_FLAG) {
   clasGPID_t *GPID = (clasGPID_t *) getBank(this->_bcs,"GPID");
   return(GPID ? GPID->gpid[this->_gpidIndex].sc_len : -1000);
 }
 else {
   return (this->scPathLen());
 }


}

int clasParticle::tagrID() const
{
  int ret = -1000;
  clasTAGR_t *TAGR = (clasTAGR_t *)getBank(this->_bcs,"TAGR");
  if (this->_pidflag == GPID_FLAG) {
       clasGPID_t *GPID = (clasGPID_t *)getBank(this->_bcs,"GPID");
    if (!GPID) {
    if (TAGR)
      return(TAGR->tagr[GPID->gpid[this->_gpidIndex].tagrid - 1].t_id);
    }
  }
  else {
    if (TAGR)
      if (this->_tagrIndex >= 0 && this->_tagrIndex < TAGR->bank.nrow)
    return(TAGR->tagr[this->_tagrIndex].t_id);
  }
  return(ret);

}

double clasParticle::scMassSquared()
{
  float fMom = this->p().r();
  float fBeta = this->beta();
  float fMassSq, fBeta2;

  fBeta2 = fBeta*fBeta;
  if(fBeta2){
    fMassSq = fMom*fMom*(1.0-fBeta2)/(fBeta2);
  }else{
    fMassSq = -99.0;
  }
  return(fMassSq);
}

double clasParticle::sc_time()
{

  if (this->_pidflag == GPID_FLAG) {
    clasGPID_t *GPID = (clasGPID_t *) getBank(this->_bcs,"GPID");
    return(GPID ? GPID->gpid[this->_gpidIndex].sc_time : -1000);
  }
  else {
    clasTBID_t *TBID = (clasTBID_t *)getBank(this->_bcs,"TBID");
    if (TBID) {
      tbid_t *tbid = &TBID->tbid[this->_tbidIndex -1];
      return(tbid->sc_time);
    }
    else
      return (-1000);
  }


}

double clasParticle::st_time()
{

  if (this->_pidflag == GPID_FLAG) {
    clasGPID_t *GPID = (clasGPID_t *) getBank(this->_bcs,"GPID");
    return(GPID ? GPID->gpid[this->_gpidIndex].st_time : -1000);
  }
  else {
    clasTBID_t *TBID = (clasTBID_t *)getBank(this->_bcs,"TBID");
    if (TBID) {
      tbid_t *tbid = &TBID->tbid[this->_tbidIndex -1];
      return(tbid->st_time);
    }
    else
      return (-1000);
  }


}


double clasParticle::scTrackLen()
{
  double ret = -1000.0;

  clasTBID_t *TBID = (clasTBID_t *)getBank(this->_bcs,"TBID");
  if (TBID) {
    tbid_t *tbid = &TBID->tbid[this->_tbidIndex -1];
    int sector = tbid->sec;

  clasTDPL_t *TDPL = (clasTDPL_t *)getGroup(this->_bcs,"TDPL",sector);
  if (TDPL) {
    // loop over TDPL entries

    for (int i = 0; i < TDPL->bank.nrow; ++i) {
      tdpl_t *tdpl = &TDPL->tdpl[i];
      int track = tdpl->trk_pln/100;
      int plane = tdpl->trk_pln % 100;
      if (track == tbid->track) {
    for (int j = NSC_MIN; j <= NSC_MAX; ++j) {
      if (j == plane) {
        if (tdpl->pos.z < 1000.0) { // this is indicator that hit is a good hit
          ret = tdpl->tlen;
          break;
        }
      }
    }
      }
    }
  }
  }
  return(ret);
}



// clasParticle class


void clasParticle::Print() const
{
  std::cout << id2name((Geant_ID) this->_pid) << " " << "\tbeta: " << this->beta() << "\tmass: "<< this->mass() <<
"\tp: " << this->p().r() << std::endl;


}


double clasParticle::Beta() const
{
  // p/E
  if (this->pidFlag() == GPID_FLAG) {
    clasGPID_t *GPID = (clasGPID_t *) getBank(this->_bcs,"GPID");
    return(GPID ? GPID->gpid[this->_gpidIndex].beta : -1000.0);
  }
  else {
    return(this->p().V().r()/this->p().t());
  }
}




double clasParticle::beta() const
{

  if (this->pidFlag() == GPID_FLAG) {
    clasGPID_t *GPID = (clasGPID_t *) getBank(this->_bcs,"GPID");
    return(GPID ? GPID->gpid[this->_gpidIndex].betam : -1000.0);
  }
  else {
  clasTBID_t *TBID = (clasTBID_t *)getBank(this->_bcs,"TBID");
    return(TBID ? TBID->tbid[this->tbid()-1].beta : -1000.0);
  }
}
clasParticle& clasParticle::p(const fourVec& p4) {
        this->_p = p4;
        return(*this);
    }
fourVec clasParticle::p() const {
  return(this->_p);
}
clasParticle& clasParticle::pos(const threeVec& p3) {
        this->_pos = p3;
        return(*this);
    }
threeVec clasParticle::pos() const {
  return(this->_pos);
}

double clasParticle::gamma()  const
{
  double r = 1.0 - this->beta() * this->beta();
  return(r < 0.0 ? -1.0/sqrt(-r) : 1.0/sqrt(r));
}

double clasParticle::mass() const
{

  if (this->pidFlag() == GPID_FLAG) {
    clasGPID_t *GPID = (clasGPID_t *) getBank(this->_bcs,"GPID");
    return(GPID ? GPID->gpid[this->_gpidIndex].mass : -1000.0);
  }
  else
    return(this->p().r()/(this->beta() * this->gamma()));
}

double clasParticle::pdgMass() const
{
  double ret;
  switch (this->pid()) {
  case Eta:
    ret = ETA_MASS;
    break;
  case PiPlus:
  case PiMinus:
    ret = PI_CHARGED_MASS;
    break;
  case Proton:
  case AntiProton:
    ret = PROTON_MASS;
    break;

  case Deuteron:
    ret = DEUTERON_MASS;
    break;

  case Neutron:
  case AntiNeutron:
    ret = NEUTRON_MASS;
    break;

  case KPlus:
  case KMinus:
    ret = KAON_CHARGED_MASS;
    break;

  case Positron:
  case Electron:
    ret = ELECTRON_MASS;
    break;
  case Gamma:
    ret = 0.0;
    break;
  case KLong:
  case KShort:
    ret = KAON_ZERO_MASS;
    break;
  case Pi0:
    ret = PI_ZERO_MASS;
    break;
  case omega:
    ret = OMEGA_MASS;
    break;

  default:
    ret =0.0;
    break;
  }
  return(ret);
}




int clasParticle::Q() const
{
  int Q(Particle_t);
  int q = Q(this->pid());
  return(q);
}


int clasParticle::ccadc()
{
  int ret = -1000;
  if (this->_tbidptr) {
    clasCC0_t *CC = (clasCC0_t *)this->getEvtBank("CC  ",this->sec());
    if (CC) {
      int id = this->_tbidptr->cc_id;
      if ((id >= 0)  && id <= CC->bank.nrow) {
    ret = CC->cc0[id-1].adc;
      }
    }
  }
  return(ret);
}

// Energy loss

void clasParticle::_eLoss(threeVec &vert,int icell,int iflag)
{
  if (icell) {
    fourVec P;
    vector3_t v;
    vector4_t pout;
    //    vector4_t p;
    vector4_t puli;
    v.x = vert.x();
    v.y = vert.y();
    v.z = vert.z();
    pout.space.x = this->p().x();
    pout.space.y = this->p().y();
    pout.space.z = this->p().z();
    pout.t = this->p().t();
    //std::cerr << " pout.space.z " << pout.space.z << std::endl;
    puli=c_momcor(pout,(float) this->pdgMass(),v,iflag,icell);
    //std::cerr << " puli.space.z " << puli.space.z << std::endl;
    P = fourVec(sqrt(this->pdgMass() * this->pdgMass() + puli.space.x * puli.space.x + puli.space.y * puli.space.y + puli.space.z * puli.space.z),threeVec(puli.space.x,puli.space.y,puli.space.z));

    //    std::cerr << " P.z" << P.z() << std::endl;
    //    std::cerr << " pout.space.z  end " << pout.space.z << std::endl;
    //    std::cerr << " ********************* " << std::endl;

    this->p(P);


  }
}


void clasParticle::_momCorr()
{

  // momentum correction for g6c ONLY!

  fourVec P;
  P = momentum_correction(this->_p,this->_sec,this->_pid);
  this->p(P);

}
void clasParticle::_momCorrg1c()
{

  // momentum correction for g1c ONLY!
  vector4_t pin,pout;
  fourVec P;
  pin.space.x = this->_p.V().x();
  pin.space.y = this->_p.V().y();
  pin.space.z = this->_p.V().z();
  pin.t = this->_p.t();
  pout  = p_correction(pin);
  P.set(pout.t,threeVec(pout.space.x,pout.space.y,pout.space.z));
  this->p(P);

}

void clasParticle::_pcorr_and_eLoss(threeVec &vert,int icell,int iflag)
{
  if (icell) {
    fourVec P;
    vector3_t v;
    vector4_t pout;
    vector4_t pout_2;
    //    vector4_t p;
    vector4_t puli;
    v.x = vert.x();
    v.y = vert.y();
    v.z = vert.z();
    pout.space.x = this->p().x();
    pout.space.y = this->p().y();
    pout.space.z = this->p().z();
    pout.t = this->p().t();
    std::cerr << " pout.space.z " << pout.space.z << std::endl;
    pout_2 = p_correction(pout);
    std::cerr << " pout_2.space.z " << pout_2.space.z << std::endl;
    puli=c_momcor(pout_2,(float) this->pdgMass(),v,iflag,icell);
    std::cerr << " puli.space.z " << puli.space.z << std::endl;
    P = fourVec(sqrt(this->pdgMass() * this->pdgMass() + puli.space.x * puli.space.x + puli.space.y * puli.space.y + puli.space.z * puli.space.z),threeVec(puli.space.x,puli.space.y,puli.space.z));

    //    std::cerr << " P.z" << P.z() << std::endl;
    //    std::cerr << " pout_2.space.z  end " << pout_2.space.z << std::endl;
        std::cerr << " ********************* " << std::endl;

    this->p(P);


  }
}


////////////////////////////////////////////////////////////////////
// Added some stuff needed to calculate the correct covariant
// matrix.
////////////////////////////////////////////////////////////////////
// Return amount of target material
// track passes through. Calls Eugene's code.
double clasParticle::lenTargetTrack()
{

  int cell;
  float punit[3],vert[3],x,x1;
  x = 0;
  x1 = 0;

    if(this->ev()->runPeriod() == g1a || this->ev()->runPeriod() == g1b)
    cell = 1;
    else if(this->ev()->runPeriod() == g2a)
    cell = 2;

    else if(this->ev()->runPeriod() == g1c)
    cell = 3;
//    else if(this->ev()->runPeriod() == g3)
//    cell = 4;
    else if(this->ev()->runPeriod() == g6c || this->ev()->runPeriod() == g8a)
    cell = 5;
    else if(this->ev()->runPeriod() == g10)
    cell = 6;
    else if(this->ev()->runPeriod() == g11a)
    cell = 7;
    else if (this->ev()->runPeriod() == g12)
     cell = 7;
  else
    cell = 0;

  punit[0] = this->p().x()/this->p().r();
  punit[1] = this->p().y()/this->p().r();
  punit[2] = this->p().z()/this->p().r();

  // Using vertex from MVRT
  vert[0] = this->ev()->V().x();
  vert[1] = this->ev()->V().y();
  vert[2] = this->ev()->V().z();

  targcell_(cell,*vert,*punit,x,x1);

  return x;

}
//////////////////////// return TBER quantities
double clasParticle::TBERq_over_p()
{
  double tqop = -10000;
  clasTBER_t *TBER = (clasTBER_t *) getBank(this->_bcs,"TBER");
  clasTBID_t *TBID = (clasTBID_t *) getBank(this->_bcs,"TBID");

  if (TBER  &&  TBID)
    {
    int nTBID_Index = this->tbid();
    if (nTBID_Index && TBID->tbid[nTBID_Index - 1].sc_stat){
      int nTBER_Index = TBID->tbid[nTBID_Index-1].track;
     tqop = TBER->tber[nTBER_Index-1].q_over_p;
        }
   }
  return(tqop);
}

double clasParticle::TBERphi()
{
    double tphi = -10000;
  clasTBER_t *TBER = (clasTBER_t *) getBank(this->_bcs,"TBER");
  clasTBID_t *TBID = (clasTBID_t *) getBank(this->_bcs,"TBID");
  if (TBER  &&  TBID)
    {
    int nTBID_Index = this->tbid();
    if (nTBID_Index && TBID->tbid[nTBID_Index - 1].sc_stat){
      int nTBER_Index = TBID->tbid[nTBID_Index-1].track;
     tphi = TBER->tber[nTBER_Index-1].phi;
        }
   }
    return tphi;
}
double clasParticle::TBERlambda()
{
    double tlambda = -10000;
  clasTBER_t *TBER = (clasTBER_t *) getBank(this->_bcs,"TBER");
  clasTBID_t *TBID = (clasTBID_t *) getBank(this->_bcs,"TBID");
  if (TBER  &&  TBID)
    {
    int nTBID_Index = this->tbid();
    if (nTBID_Index && TBID->tbid[nTBID_Index - 1].sc_stat){
      int nTBER_Index = TBID->tbid[nTBID_Index-1].track;
     tlambda = TBER->tber[nTBER_Index-1].lambda;
        }
   }
    return tlambda;
}

matrix<double> clasParticle::TBERmatrix()
{
    matrix<double> tbermat;
    tbermat = matrix<double>(5,5);
  clasTBER_t *TBER = (clasTBER_t *) getBank(this->_bcs,"TBER");
  clasTBID_t *TBID = (clasTBID_t *) getBank(this->_bcs,"TBID");
  if (TBER  &&  TBID)
    {
    int nTBID_Index = this->tbid();
    if (nTBID_Index && TBID->tbid[nTBID_Index - 1].sc_stat)
        {
      int nTBER_Index = TBID->tbid[nTBID_Index-1].track;

                     tbermat.el(0,0) = TBER->tber[nTBER_Index-1].c11;
                     tbermat.el(0,1) = TBER->tber[nTBER_Index-1].c12;
                     tbermat.el(0,2) = TBER->tber[nTBER_Index-1].c13;
                     tbermat.el(0,3) = TBER->tber[nTBER_Index-1].c14;
                     tbermat.el(0,4) = TBER->tber[nTBER_Index-1].c15;

                     tbermat.el(1,1) = TBER->tber[nTBER_Index-1].c22;
                     tbermat.el(1,2) = TBER->tber[nTBER_Index-1].c23;
                     tbermat.el(1,3) = TBER->tber[nTBER_Index-1].c24;
                     tbermat.el(1,4) = TBER->tber[nTBER_Index-1].c25;

                     tbermat.el(2,2) = TBER->tber[nTBER_Index-1].c33;
                     tbermat.el(2,3) = TBER->tber[nTBER_Index-1].c34;
                     tbermat.el(2,4) = TBER->tber[nTBER_Index-1].c35;

                     tbermat.el(3,3) = TBER->tber[nTBER_Index-1].c44;
                     tbermat.el(3,4) = TBER->tber[nTBER_Index-1].c45;

                     tbermat.el(4,4) = TBER->tber[nTBER_Index-1].c55;

                    ///// Make sure matrix is symmetric
                    for(int i=1;i<5;i++)
                    {
                        for(int j=0;j<i;j++)
                        {
                            if(i!=j) tbermat.el(i,j) = tbermat.el(j,i);
                        }
                    }

    }
  }
  return(tbermat);
}

//////////////////////// return proper covariant matrix
matrix<double> clasParticle::getCovMatrix()
{
  matrix<double> cm;
  cm = matrix<double>(3,3);

  matrix<double> tm;
  tm = this->TBERmatrix();
  int Q = int(fabs(this->TBERq_over_p())/this->TBERq_over_p());
  double P = (double)Q/this->TBERq_over_p();
  double pmag = this->p().r();
  //May want to change this to pass in different M's
  double M = this->p().len();
  //if(this->ev()->verbose()) cerr << "Q: " << Q << "\tP: " << P << "\tM: " << M << endl;

  //// Fill the cm with appropriate values from the tracking matrix
  //// and convert to p from q/p.
  for(int i=0;i<3;i++)
    {
      for(int j=0;j<3;j++)
    {
      if(i==0 && j==0)
        cm.el(i,j) = tm.el(i,j)*(pow(P,4)/pow((float)Q,2));
      else if((i==0 || j==0) && (i!=j))    cm.el(i,j) = tm.el(i,j)*(-pow(P,2)/Q);
      else
        cm.el(i,j) = tm.el(i,j);
    }
    }

  // Calculate how much TARGET material the track passes through
  // using Eugene's function call. Note that this is after any
  // momentum corrections have been performed.
  double tlen = this->lenTargetTrack();

  float alpha, beta, theta;
  alpha = (3.14159/3.0)*this->scPaddleId_sector();
  beta  = pmag/sqrt(pow(M,2) + pow(pmag,2));
  theta = this->p().theta();

    double Xratio = 2.010;            // Xtarget/Xlh2 (radiation length ratio to lh2)
    double Iratio = 1.000;            // Imax/I (torus current ratio to maximum)
    double rhoAratio = 1.000;         // density/A ratio to that of lh2
    double sigMS = 0.002;
    double sigEL = 0.0027;

    double PcorrScale = 1.0;
    if(this->ev()->configPcorStatus() && this->ev()->runPeriod() == g1c) PcorrScale = 0.6;
    else                            PcorrScale = 1.0;

    if(this->ev()->runPeriod() == g10)
    {
        Iratio = 1.716; //2250A
        Xratio = 1.000;
        rhoAratio = 1.1935;
        sigMS = 0.0045;
        sigEL = 0.0035;
    }
    else if(this->ev()->runPeriod() == g11a)
    {
        Iratio = 2.010;
        Xratio = 1.000;
        rhoAratio = 1.000;
        sigMS = 0.0016;
        sigEL = 0.00209;
    }
    else if(this->ev()->runPeriod() == g1c)
    {
        Iratio = 2.010;
        Xratio = 1.000;
        rhoAratio = 1.000;
        sigMS = 0.002;
        sigEL = 0.0027;
    }
    else
    {
        Iratio = 2.010;
        Xratio = 1.000;
        rhoAratio = 1.000;
        sigMS = 0.002;
        sigEL = 0.0027;
    }



  for(int i=0;i<3;i++)
  {
  for(int j=0;j<3;j++)
    {
    // Scale momentum resolution errors by Iratio
     if(i==j && j==0)
    {
        cm.el(i,j) *= pow(Iratio,2);
        if(beta < 0.87)
            cm.el(i,j) += PcorrScale*pow(sigEL/beta,2)*(1-pow(beta,2)/2.0)*(1.0/(1-pow(beta,2)))*(1.0/(2.0*sin(theta))+(1.0/2.0)*rhoAratio*(tlen/2.0));
        else
            cm.el(i,j) += PcorrScale*pow(sigEL,2)*(3.24)*(1.0/(2.0*sin(theta)) + (1.0/2.0)*rhoAratio*(tlen/2.0));
    }
    else if(i==j && j!=0)
    {
        cm.el(i,j) *= pow(1.5,2);
        cm.el(i,j) += pow(sigMS/(pmag*beta),2)*(0.5/(sin(theta)) + (0.5/(Xratio))*(tlen/2.0));
    }
    else if((i==0 || j==0) && (i!=j)) cm.el(i,j) *= 1.5*Iratio;
    else if((i==1 || j==1) && (i!=j)) cm.el(i,j) *= pow(1.5,2);
    }
    }

  return(cm);
}
////////////////////////////////////////
////////////////////////////////////////

runPeriod_t getRunPeriod(int runNo)
{
  runPeriod_t ret = g1c;

  if (runNo >= 34620 && runNo <= 35617)
    ret = g7;
  else if (runNo >= 29644 && runNo <= 30198)
    ret = g6c;

  else if (runNo >= 42299 && runNo <= 43264)
    ret = g10;
  else if (runNo >= 43264 && runNo <= 44133)
    ret = g11a;
  else if (runNo > 56361)
    ret = g12;
  return(ret);


}

runClass_t getRunClass(runPeriod_t runPeriod)
{
  runClass_t ret = G1;

  switch (runPeriod) {
  case g1a:
  case g1b:
  case g1c:
    ret = G1;
    break;

  case g2a:
  case g2b:
    ret = G2;
    break;

  case g6a:
  case g6b:
  case g6c:
    ret = G6;
    break;

  case g7:
    ret = G7;
    break;

  case g8a:
    ret = G8;
    break;

  case g10:
    ret = G10;
    break;
  case g11a:
    ret = G11;
    break;
  case g12:
    ret = G12;

  }
  return(ret);
}

double clasParticle::CS()
{
  double ret = -1000.0;
  clasCChit cchit = this->CChit();

  if (cchit.status()) {
    ret = (double) (400.0 * fabs(this->ccQF()) - cchit.npe() * 10.0)/(double) (400.0 * fabs(this->ccQF()) + cchit.npe() * 10.0);
  }
  return(ret);
}


double clasParticle::ccQF()
{
  double ret = -1000.0;
  clasCChit cchit = this->CChit();
  if (cchit.status()) {
    ret = cchit.theta() - this->CCdir().theta();
  }
  return(ret);
}

int clasParticle::ccPhiMatch()
{
  int ret = -1000;
  int pmt_hit;
  double Phi, Phi_sec;

  clasCChit cchit = this->CChit();
  if (cchit.status()) {

    // set angle phi's range to 0 to 360
    Phi = RAD_2_DEG_CONV*(this->p().V().phi());
    if(Phi<0.0){
      Phi_sec = 360.0 + Phi;
    }else{
      Phi_sec = Phi;
    }

    // change angle phi's range to -30 to 30
    Phi_sec += 30.0;
    Phi_sec -= (int)(Phi_sec/60.0)*60.0;
    Phi_sec -= 30.0;

    // from angle phi, side of sector: left(-1),center(0),right(1)
    if(fabs(Phi_sec)>3.0){
      if(Phi_sec>0.0){
        pmt_hit = 1;
      }else{
        pmt_hit = -1;
      }
    }else{
      pmt_hit = 0;
    }
    // determine if CC hits match in phi from 4V and nrPhi of CCRC bank
    if(cchit.MeanSegment()==1){
      ret = (abs(pmt_hit - cchit.nrPhi())<1);
    }else{
      ret = (abs(pmt_hit - cchit.nrPhi())<2);
    }
  }
  return(ret);
}

clasSThit clasParticle::SThit()
{

  clasSThit ret;
  clasSTR_t *STR = (clasSTR_t *) getGroup(this->_bcs,"STR ",1);
  clasTBID_t *TBID = (clasTBID_t *) getBank(this->_bcs,"TBID");

  if (TBID  &&  STR) {
    if(this->_tbidIndex) {
      tbid_t *tbid = &TBID->tbid[this->_tbidIndex - 1];
      if ( tbid->st_stat) {
    str_t *str = &STR->str[tbid->st_id - 1];
    int sec = str->id/100;
    int paddle = (str->id/10) % 10;
    int id = (sec - 1) * 4 + paddle;
    ret.setID(id);
    ret.setPaddle(paddle);
    ret.setSec(sec);
    ret.setTime(str->st_time);
    ret.setPathLength(str->st_l);
      }
    }
  }
  return(ret);
}

int clasParticle::isSThit()
{

  int ret = 0;
  clasSTR_t *STR = (clasSTR_t *) getGroup(this->_bcs,"STR ",1);
  clasTBID_t *TBID = (clasTBID_t *) getBank(this->_bcs,"TBID");

  if (TBID  &&  STR) {
    if(this->_tbidIndex) {
      tbid_t *tbid = &TBID->tbid[this->_tbidIndex - 1];
      if ( tbid->st_stat) {
    ret = 1;
      }
    }
  }
  return(ret);
}

clasSThit& clasSThit::operator=(const clasSThit& sthit)
{

  this->_id = sthit.id();
  this->_paddle = sthit.paddle();
  this->_sec = sthit.sec();
  this->_time = sthit.time();
  this->_pathLength = sthit.pathLength();

  return(*this);
}



clasCChit clasParticle::CChit()
{

  clasCChit ret;
  clasCC01_t *CC01 = (clasCC01_t *) getGroup(this->_bcs,"CC01",this->sec());
  clasCCRC_t *CCRC = (clasCCRC_t *) getBank(this->_bcs,"CCRC");
  clasTBID_t *TBID = (clasTBID_t *) getBank(this->_bcs,"TBID");

  if (TBID  &&  CC01 && CCRC) {
    if(this->_tbidIndex) {
      tbid_t *tbid = &TBID->tbid[this->_tbidIndex - 1];
      if ( tbid->cc_stat == GOOD_MATCH) {
    cc01_t *cc01 = &CC01->cc01[tbid->cc_id - 1];
    for (int j = 0; j < CCRC->bank.nrow; ++j) {
      ccrc_t *ccrc = &CCRC->ccrc[j];
      int ibeg = ccrc->nrsegm_m/10;
      int iend = ccrc->nrsegm_p/10;

      if ( ccrc->nrsect == this->sec()) {

        if (ccrc->nrphy <= 0 && (cc01->id % 2)) {  // cc01 must be odd
          for (int k = ibeg; k <= iend; k++) {
        int ichk = 2 * k - 1;
        if (ichk == cc01->id) {
          // match found
          ret.setCC01(cc01);
          ret.setCCRC(ccrc);
          ret.status(1);
          break;
        }
          }
        }
        if (ccrc->nrphy >= 0 && !(cc01->id % 2)) {  // cc01 must be odd
          for (int k = ibeg; k <= iend; k++) {
        int ichk = 2 * k;
        if (ichk == cc01->id) {
          // match found
          ret.setCC01(cc01);
          ret.setCCRC(ccrc);
          ret.status(1);
          break;
        }
          }
        }
      }

    }
      }
    }
  }
  return(ret);
}

clasCChit& clasCChit::operator=(const clasCChit& cchit)
{
  this->_cc01 = cchit.cc01();
  this->_ccrc = cchit.ccrc();
  this->_status = cchit.status();
  return(*this);
}

clasSChit clasParticle::SChit()
{

  clasSChit ret;
  clasSCRC_t *SCRC = (clasSCRC_t *) getGroup(this->_bcs,"SCRC",this->sec());
  clasTBID_t *TBID = (clasTBID_t *) getBank(this->_bcs,"TBID");

  if (TBID  &&  SCRC) {
    if(this->_tbidIndex) {
      tbid_t *tbid = &TBID->tbid[this->_tbidIndex - 1];
      if ( tbid->sc_stat) {
    scrc_t *scrc = &SCRC->scrc[tbid->sc_id - 1];
    int paddle = scrc->id;
    int id = scrc->id;
    ret.setID(id);
    ret.setPaddle(paddle);
    ret.setSec(this->sec());
    ret.setTime(scrc->time);
    ret.setEnergy(scrc->energy);


      }
    }
  }
  return(ret);
}

int clasParticle::isSChit()
{

  int ret = 0;
  clasSCRC_t *SCRC = (clasSCRC_t *) getGroup(this->_bcs,"SCRC",this->sec());
  clasTBID_t *TBID = (clasTBID_t *) getBank(this->_bcs,"TBID");

  if (TBID  &&  SCRC) {
    if(this->_tbidIndex) {
      tbid_t *tbid = &TBID->tbid[this->_tbidIndex - 1];
      if ( tbid->sc_stat) {
    ret = 1;
      }
    }
  }
  return(ret);
}
int clasParticle::isCChit()
{

  int ret = 0;

  clasTBID_t *TBID = (clasTBID_t *) getBank(this->_bcs,"TBID");

  if (TBID) {
    if(this->_tbidIndex) {
      tbid_t *tbid = &TBID->tbid[this->_tbidIndex - 1];
      if ( tbid->cc_stat) {
    ret = 1;
      }
    }
  }
  return(ret);
}

int clasParticle::isEChit()
{

  int ret = 0;

  clasTBID_t *TBID = (clasTBID_t *) getBank(this->_bcs,"TBID");

  if (TBID) {
    if(this->_tbidIndex) {
      tbid_t *tbid = &TBID->tbid[this->_tbidIndex - 1];
      if ( tbid->ec_stat) {
    ret = 1;
      }
    }
  }
  return(ret);
}

clasSChit& clasSChit::operator=(const clasSChit& schit)
{

  this->_id = schit.id();
  this->_paddle = schit.paddle();
  this->_sec = schit.sec();
  this->_time = schit.time();
  this->_pathLength = schit.pathLength();
  this->_energy = schit.energy();

  return(*this);
}


void clasTaggedPhoton::TAGRcorrection(float E0, int runID)
{
    // beam offsets only calculated for 2.4 and 3.1 from g1c
  // runID = 1 is 3.115 from g1c
  // runID = 2 is 2.4 from g1c
  // runID = 3 is 4.016 from g11a
  // any other runID gets a beam offset of 0.0

    double newE = TAGRcor(this->_tagr->erg, E0, runID);
    this->_tagr->erg = newE;
}

/*
void clasEvent::g1cPcorrection()
{

  if (this->_configg1cPcorrection == 0) {
    InitPcor();
    this->_configg1cPcorrection = this->run();
  }

  {
    std::list<clasParticle>::iterator p = this->_cp.begin();
    std::list<clasParticle> _newparts;
    std::list<clasParticle>::iterator first,last;

    first = this->_cp.begin();
    last = this->_cp.end();

    while (p != this->_cp.end() ) {
      clasParticle x;
      fourVec p4 = p->p();
      int sector = p->scPaddleId_sector();
      int q = p->Q();
      p->_g1cPcorrection(p4,sector,q);
      x = *p;
      _newparts.push_back(x);
      p++;
    }

    this->_cp.erase(first,last);
    p = _newparts.begin();
    while (p != _newparts.end()) {
      this->addParticle(*p);
      p++;
    }

  }
}

void clasParticle::_g1cPcorrection(fourVec p,int sector,int q)
{
    fourVec P;
    vector4_t puli;
    vector4_t pout;
    pout.space.x = this->p().x();
    pout.space.y = this->p().y();
    pout.space.z = this->p().z();
    pout.t = this->p().t();
    puli=g1cMomCor(pout,sector,q);
    P = fourVec(sqrt(this->pdgMass() * this->pdgMass() + puli.space.x * puli.space.x + puli.space.y * puli.space.y + puli.space.z * puli.space.z),
                threeVec(puli.space.x,puli.space.y,puli.space.z));

    this->p(P);
}
*/

void clasEvent::Pcor()
{

  // runID = 1  is g1c 3.1 GeV
  // runID = 2  is g1c 2.4 GeV
  // runID = 3  is g11
  int runID = 0;
  switch (this->runPeriod()) {
  case g1c:
   if(this->run() >= 21763)
   {
     runID = 2;
   }
   else
   {
         runID = 1;
   }
    break;
  case g11a:
  case g7:   // Use g11a correction for g7
    runID = 3;
    break;
  default:
    runID = 0;
    break;
}


  if (this->_configPcor == 0) {
    InitPcor();
    this->_configPcor = this->run();
  }

  {
    std::list<clasParticle>::iterator p = this->_cp.begin();
    std::list<clasParticle> _newparts;
    std::list<clasParticle>::iterator first,last;

    first = this->_cp.begin();
    last = this->_cp.end();

    while (p != this->_cp.end() ) {
      clasParticle x;
      fourVec p4 = p->p();
      int sector = p->scPaddleId_sector();
      int q = p->Q();
      p->_Pcor(p4,sector,q,runID);
      x = *p;
      _newparts.push_back(x);
      p++;
    }

    this->_cp.erase(first,last);
    p = _newparts.begin();
    while (p != _newparts.end()) {
      this->addParticle(*p);
      p++;
    }

  }
}

void clasEvent::g10pcor(int icor)
{

  // icor = 1   Marco's correction, based on p p pi-, 4-momentum conservation (wiggles tagger).
  // icor = 2   Mibe's correciton, based on p p pi-, transverse momentum conservation, (no tagger).
  // icor = 3   Nathan's correction, based on Kshort -> pi+ pi-, (no tagger).
  // icor = 4   Combination of 1 & 3


  if (this->_configg10pcor == 0) {
    //Initg10pcor((int)this->torusCurrent(), icor);
    this->_configg10pcor = this->run();
  }

  {
    std::list<clasParticle>::iterator p = this->_cp.begin();
    std::list<clasParticle> _newparts;
    std::list<clasParticle>::iterator first,last;

    first = this->_cp.begin();
    last = this->_cp.end();

    while (p != this->_cp.end() ) {
      clasParticle x;
      fourVec p4 = p->p();
        int q = p->Q();
      p->_g10pcor(p4,q,(int)this->torusCurrent(), icor);
      x = *p;
      _newparts.push_back(x);
      p++;
    }

    this->_cp.erase(first,last);
    p = _newparts.begin();
    while (p != _newparts.end()) {
      this->addParticle(*p);
      p++;
    }

  }
}

void clasEvent::nbPcor()
{

  if (this->_configPcor == 0) {
    this->_configPcor = this->run();
  }

  {
    std::list<clasParticle>::iterator p = this->_cp.begin();
    std::list<clasParticle> _newparts;
    std::list<clasParticle>::iterator first,last;

    first = this->_cp.begin();
    last = this->_cp.end();

    while (p != this->_cp.end() ) {
      clasParticle x;
      fourVec p4 = p->p();
      int q = p->Q();
      p->_nbPcor(p4,q);
      x = *p;
      _newparts.push_back(x);
      p++;
    }

    this->_cp.erase(first,last);
    p = _newparts.begin();
    while (p != _newparts.end()) {
      this->addParticle(*p);
      p++;
    }

  }
}


void clasParticle::_Pcor(fourVec p,int sector,int q, int runid)
{
    fourVec P;
    vector4_t puli;
    vector4_t pout;
    pout.space.x = this->p().x();
    pout.space.y = this->p().y();
    pout.space.z = this->p().z();
    pout.t = this->p().t();
    puli=Pcor(pout,sector,q,runid);
    P = fourVec(sqrt(this->pdgMass() * this->pdgMass() + puli.space.x * puli.space.x + puli.space.y * puli.space.y + puli.space.z * puli.space.z),
                threeVec(puli.space.x,puli.space.y,puli.space.z));

    this->p(P);
}

void clasParticle::_g10pcor(fourVec p, int q, int torus,int icor)
{
    fourVec P;
    vector4_t pout;
    pout.space.x = this->p().x();
    pout.space.y = this->p().y();
    pout.space.z = this->p().z();
    pout.t = this->p().t();
    //puli=c_g10pcor(torus, q, icor,  &pout);
    int status = c_g10pcor(torus, q, icor,  &pout);
    if(status>0)
    {
      P = fourVec(sqrt(this->pdgMass() * this->pdgMass() + pout.space.x * pout.space.x + pout.space.y * pout.space.y + pout.space.z * pout.space.z),
                threeVec(pout.space.x,pout.space.y,pout.space.z));

      this->p(P);
    }
}


void clasParticle::_nbPcor(fourVec p4,int charge)
{
  fourVec P;
  P = p4;

  int sec,itheta;
  float p,theta,phi,m2,tmp;
  float dp;

  // quadratic momentum dependence
  float pm[4] = { 0.0004466 , -0.00115  , 0.007155 , 0.3471 }; // -
  float pp[4] = { 0.002109  , -0.007671 , 0.01214  , 0.1641 }; // +

  // linear phi dependence, divided into 4 theta regions, 6 sectors
  float am[6][2][4]; // -
  float ap[6][2][4]; // +

  if( (charge!=-1) && (charge!=1) )
  {
    // return p4; // no correction
    this->p(P);
  }
  else
  {
  // get input kinematics
  p     = p4.r();
  m2    = p4.lenSq();
  theta = p4.theta()*180/3.141592;
  phi   = p4.phi()*180/3.141592;
  if( phi<-30 ) phi+=360;
  if( phi>330 ) phi-=360;
  sec = (int) ( (phi+30.)/60. );
  itheta = 0;

  if( sec<0 || sec>5 )
  {
    // return p4; // no correction
    this->p(P);
  }
  else
  {

  // 0 < theta < 33
  am[0][0][0] = -0.0028709   ; am[0][1][0] =  5.92131e-06 ;
  am[1][0][0] = -0.00204849  ; am[1][1][0] = -0.000139486 ;
  am[2][0][0] = -0.00124416  ; am[2][1][0] =  1.72194e-05 ;
  am[3][0][0] = -0.0006538   ; am[3][1][0] = -8.59115e-05 ;
  am[4][0][0] = -0.00245588  ; am[4][1][0] = -0.000126681 ;
  am[5][0][0] = -0.00306787  ; am[5][1][0] = -4.95702e-06 ;
  // 33 < theta < 48
  am[0][0][1] = -0.00221767  ; am[0][1][1] = -7.72867e-05 ;
  am[1][0][1] =  0.000554396 ; am[1][1][1] = -0.00016143  ;
  am[2][0][1] =  0.000201387 ; am[2][1][1] = -6.75647e-05 ;
  am[3][0][1] = -0.00115231  ; am[3][1][1] = -0.000164746 ;
  am[4][0][1] = -0.00198496  ; am[4][1][1] = -0.000207807 ;
  am[5][0][1] = -0.00328397  ; am[5][1][1] = -9.8782e-05  ;
  // 48 < theta < 81
  am[0][0][2] =  1.15233e-06 ; am[0][1][2] = -0.000233629 ;
  am[1][0][2] =  0.00251902  ; am[1][1][2] = -0.000333463 ;
  am[2][0][2] =  0.00103878  ; am[2][1][2] = -0.000426809 ;
  am[3][0][2] =  0.000621699 ; am[3][1][2] = -0.000378046 ;
  am[4][0][2] =  0.000694473 ; am[4][1][2] = -0.000292799 ;
  am[5][0][2] = -0.00075916  ; am[5][1][2] = -0.000280189 ;
  // 81 < theta < 180
  am[0][0][3] =  0.00253522  ; am[0][1][3] = -0.000460878 ;
  am[1][0][3] =  0.0102952   ; am[1][1][3] = -0.000407892 ;
  am[2][0][3] =  0.00444268  ; am[2][1][3] = -0.000363258 ;
  am[3][0][3] =  0.00428438  ; am[3][1][3] = -0.000392865 ;
  am[4][0][3] =  0.00567705  ; am[4][1][3] = -0.000409297 ;
  am[5][0][3] =  0.0015116   ; am[5][1][3] = -0.00036871  ;

  // 0 < theta < 18
  ap[0][0][0] =  0.00121251  ; ap[0][1][0] = -2.45327e-05 ;
  ap[1][0][0] = -0.00417251  ; ap[1][1][0] = -4.98219e-05 ;
  ap[2][0][0] = -0.00278882  ; ap[2][1][0] = -0.000167266 ;
  ap[3][0][0] =  0.00157576  ; ap[3][1][0] = -0.000220149 ;
  ap[4][0][0] =  0.000771925 ; ap[4][1][0] = -0.000159886 ;
  ap[5][0][0] =  0.00207998  ; ap[5][1][0] = -0.000293169 ;
  // 18 < theta < 30
  ap[0][0][1] =  0.000324894 ; ap[0][1][1] = -0.000191954 ;
  ap[1][0][1] = -0.00362691  ; ap[1][1][1] = -0.000170692 ;
  ap[2][0][1] = -0.00191738  ; ap[2][1][1] = -0.000214619 ;
  ap[3][0][1] = -0.000435924 ; ap[3][1][1] = -0.000288746 ;
  ap[4][0][1] = -0.00199774  ; ap[4][1][1] = -0.000154779 ;
  ap[5][0][1] =  5.49575e-05 ; ap[5][1][1] = -0.000246275 ;
  // 30 < theta < 54
  ap[0][0][2] =  0.000297495 ; ap[0][1][2] = -0.000306386 ;
  ap[1][0][2] = -0.00113373  ; ap[1][1][2] = -0.000310556 ;
  ap[2][0][2] = -0.000580969 ; ap[2][1][2] = -0.000330488 ;
  ap[3][0][2] = -0.000976977 ; ap[3][1][2] = -0.000524708 ;
  ap[4][0][2] = -0.00179485  ; ap[4][1][2] = -0.000381546 ;
  ap[5][0][2] = -0.00139143  ; ap[5][1][2] = -0.000427089 ;
  // 54 < theta < 180
  ap[0][0][3] =  0.00317386  ; ap[0][1][3] = -0.000201917 ;
  ap[1][0][3] =  0.000838974 ; ap[1][1][3] = -0.00040742  ;
  ap[2][0][3] =  0.00443875  ; ap[2][1][3] = -0.000389817 ;
  ap[3][0][3] =  0.000553896 ; ap[3][1][3] = -0.000583283 ;
  ap[4][0][3] =  0.000758088 ; ap[4][1][3] = -0.000372777 ;
  ap[5][0][3] =  0.00186038  ; ap[5][1][3] = -0.000376769 ;


  if(charge==1){
    if(theta>18) itheta++;
    if(theta>30) itheta++;
    if(theta>54) itheta++;

    // correct phi dependence
    p += p*( ap[sec][0][itheta] + ap[sec][1][itheta]*(phi-60*sec) );

    // correct momentum dependence
    if( p<0.5 ){ dp = pp[0] + pp[1]*p + pp[2]*(p-pp[3])*(p-pp[3]); }
    else{ dp = pp[0] + pp[1]*0.5 + pp[2]*(0.5-pp[3])*(0.5-pp[3]);  }
    p += dp;
  }

  if(charge==-1){
    if(theta>33) itheta++;
    if(theta>48) itheta++;
    if(theta>81) itheta++;

    // correct phi dependence
    p += p*( am[sec][0][itheta] + am[sec][1][itheta]*(phi-60*sec) );

    // correct momentum dependence
    if( p<0.5 ){ dp = pm[0] + pm[1]*p + pm[2]*(p-pm[3])*(p-pm[3]); }
    else{ dp = pm[0] + pm[1]*0.5 + pm[2]*(0.5-pm[3])*(0.5-pm[3]);  }
    p += dp;

  }

  //return new 4-vector
  tmp = p4.r();
  double x = ( p*p4.x()/tmp );
  double y = ( p*p4.y()/tmp );
  double z = ( p*p4.z()/tmp );
  double e = ( sqrt(p4.r()*p4.r()+m2) );
  P = fourVec(e, threeVec(x, y, z));
  }

    //return p4;

    this->p(P);
  }
}


Particle_t  sebPID(int sebcode)
{


  switch (sebcode) {
  case 22:
    return Gamma;
    break;
  case 11:
    return Electron;
    break;
  case -11:
    return Positron;
    break;
  case 111:
    return Pi0;
    break;
  case 211:
    return PiPlus;
    break;
  case -211:
    return PiMinus;
    break;
  case -321:
    return KMinus;
    break;
  case 321:
    return KPlus;
    break;
  case 2112:
    return Neutron;
    break;
  case -2112:
    return AntiNeutron;
    break;
  case 2212:
    return Proton;
    break;
  case -2212:
    return AntiProton;
    break;
  case 3122:
    return Lambda;
  case 700201:
    return Deuteron;
    break;
  default:
    return Unknown;
  }
}

int pidSEB(Particle_t pid)
{

  switch (pid) {
  case Gamma:       return (22);    break;
  case Positron:    return (-11);   break;
  case Electron:    return (11);    break;
  case Neutrino:    return (12);    break;
  case MuonPlus:    return (13);    break;
  case MuonMinus:   return (-13);   break;
  case Pi0:         return (111);   break;
  case PiPlus:      return (211);   break;
  case PiMinus:     return (-211);  break;
  case KLong:       return (130);   break;
  case KPlus:       return (321);   break;
  case KMinus:      return (-321);  break;
  case Neutron:     return (2112);  break;
  case Proton:      return (2212);  break;
  case AntiProton:  return (-2212); break;
  case KShort:      return (310);   break;
  case Eta:         return (221);   break;
  case Lambda:      return (3122);  break;
  case SigmaPlus:   return (3222);  break;
  case Sigma0:      return (3212);  break;
  case SigmaMinus:  return (3112);  break;
  case Xi0:         return (3322);  break;
  case XiMinus:     return (3312);  break;
  case OmegaMinus:  return (3334);  break;
  case AntiNeutron: return (-2112); break;
  case AntiLambda:  return (-3122); break;
  case AntiSigmaMinus: return (-3112);  break;
  case AntiSigma0:     return (-3212);  break;
  case AntiSigmaPlus:  return (-3222);  break;
  case AntiXi0:        return (-3322);  break;
  case Rho0:        return (113);   break;
  case RhoPlus:     return (213);   break;
  case RhoMinus:    return (-213);  break;
  case omega:       return (223);   break;
  case EtaPrime:    return (331);   break;
  case phiMeson:    return (333);   break;
  case Deuteron:    return (700201); break;
  default:          return (0);
  }
}

string particleName(Particle_t pid)
{
  string ret;

  switch (pid) {
  case Electron:
    ret = string("e-");
    break;
  case Positron:
    ret = string("e+");
    break;
  case PiPlus:
    ret = string("pi+");
    break;

  default:
    ret = string("???");
    break;

  }
  return(ret);
}




double Mass(nuclear_t x)
{
  double ret  = PROTON_MASS;
  switch (x) {
  case H2:
    ret = PROTON_MASS;
    break;
  case D2:
    ret = DEUTERON_MASS;
    break;
  case CARBON:
    ret = CARBON_MASS;
    break;
  case IRON:
    ret = IRON_MASS;
    break;
  case TITANIUM:
    ret = TITANIUM_MASS;
    break;
  case LEAD:
    ret = LEAD_MASS;
    break;
  case UNKNOWN:
  default:
    ret = PROTON_MASS;
    break;
  }
  return(ret);






}

