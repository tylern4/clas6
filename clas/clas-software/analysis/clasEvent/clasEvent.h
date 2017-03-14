#ifndef _CLASEVENT_H
#define _CLASEVENT_H

#define GPID_FLAG 1
#define SEB_FLAG 2
#define    PART_FLAG 3

#include <event.h>
#include <mwKfit.h>

using namespace std;

#define AMU 0.9315

#define CARBON_MASS 12.0107 * AMU
#define IRON_MASS 55.845 * AMU
#define TITANIUM_MASS 47.867 * AMU
#define LEAD_MASS 207.2 * AMU


#define RAD_2_DEG_CONV 57.2958
#define DEG_2_RAD_CONV 0.01745328
#define SCALER_EVENT 10

#define CC_NPE 2.5
#define ISLEP_MOM_THRESHOLD 0.4
#define ISLEP_MSQ_SLOPE 15.0
#define ISLEP_MSQ_OFFSET 0.005
#define ISLEP_EC_P_LO -0.3
#define ISLEP_EC_P_HI 0.5
#define ISLEP_ECIO_SLOPE 2.0
#define ISLEP_ECIN 0.07
#define ISLEP_ECIN_P 0.09
#define ISLEP_ECOUT 0.14
#define ISLEP_ECOUT_P 0.16
#define SC_CC_TIME_OFFSET 3.344
#define SC_CC_TIME_CUT 5.0
#define G7A_R_LIM 2.0
#define G7A_DZ_LIM 3.0
#define G7A_DT_LIM 1.002

#define DATA_CLASS 15


extern "C" {
#include <ntypes.h>
#include <bostypes.h>
#include <trk.h>
#include <pid.h>
#include <vertex.h>
#include <utility.h>
#include <particleType.h>
#include <kinematics.h>
#include <makebanks.h>
#include <eloss.h>
#include <ec.h>
#include <tagM.h>
#include <PartUtil.h>
#include <trk_run_control.h>
#include <printBOS.h>
#include <itape.h>

  int initDisplay(char *,int);
  int getData(BOSbank *,char *);
  float get_photon_energy(clasTAGR_t *,clasBID_t *);
  tagr_t * get_photon_tagrEcut(clasTAGR_t *,clasBID_t *,double E0,double E1);

  double c_taggerenergy(int runno, float E0);

  // temporary repository for missing prototypes
  void cc_brun_(int *);
  void cc_bevt_();
  void cc_evnt_();
  void trk_set_def_();
  void dc_set_def_();
  void setup_anapatt_();
  void ana_segm_(int *,int *);
  void ana_prfit_(int *);
  void trk_link_(int *,int *);
  void bdrop_(int *,const char *,int);
  void bnames_(int *);

}

typedef enum {g1a,g1b,g1c,g2a,g2b,g6a,g6b,g6c,g7,g8a,g10,g11a,g12} runPeriod_t;
typedef enum {G1,G2,G6,G7,G8,G10,G11,G12} runClass_t;

typedef enum {H2,D2,CARBON,IRON,TITANIUM,LEAD,UNKNOWN} nuclear_t;

runPeriod_t getRunPeriod(int);
runClass_t getRunClass(runPeriod_t);


#define MONTE_CARLO_TYPE -2
#define DATA_TYPE 1

#define CC_BANKS "CCRCCC01"
#define SC_BANKS "SC1 SCR SCRC"
#define EC_BANKS "EC01ECHBECPIECPCECPBEC  EC1 "
#define TAGGER_BANKS "TAGMTAGRTAGIPSO "
#define SEB_BANKS "HEVTEVNTDCPBSCPBCCPBUNUSEVHBTRKSSTPBTGPBLCPB"
#define ST_BANKS "ST1 STR "
#define REGION1_BANKS "RGLK"

#include <tag_cpp_wrapper.h>
#include <momentum_correction.h>
int Q(Particle_t);
double M(Particle_t);
int pid2PDG(Particle_t);
double pidChisq(const threeVec &p,double mass,double len,double t);
fourVec echb2partMVRT(echb_t *echb, double mass,threeVec &v);
int cut(float x,float low,float hi);
int getPi0(int partbank,fourVec &pi0,int *igam1,int *igam2);

Particle_t  sebPID(int sebcode);
int pidSEB(Particle_t pid);
// clasUtilities prototypes
void setPrlinkx(string);

string particleName(Particle_t);
double Mass(nuclear_t);

class clasEvent;

class clasSChit {
 private:
  int _id;
  int _sec;
  int _paddle;
  double _time;
  double _energy;

  double _pathLength;
  threeVec _pos;


 public:
  clasSChit() {this->_id  = this->_sec = this->_paddle = 1000; this->_time = this->_energy = this->_pathLength = -1000.0;}
  int id() const { return(this->_id);}
  int setID(int i) { return(this->_id = i);}
  int paddle() const { return(this->_paddle);}
  int setPaddle(int i) { return(this->_paddle = i);}
  int sec() const { return(this->_sec);}
  int setSec(int i) { return(this->_sec = i);}
  double time() const { return(this->_time);}
  double setTime(double t) { return(this->_time = t);}
  double pathLength() const { return(this->_pathLength);}
  double setPathLength(double t) { return(this->_pathLength = t);}
  double setEnergy(double e)  { return(this->_energy = e);}
  double energy() const {return(this->_energy);}
  clasSChit& operator=(const clasSChit& hit);
};

class clasSThit {
 private:
  int _id;
  int _sec;
  int _paddle;
  double _time;
  double _pathLength;


 public:
  clasSThit() {this->_id  = this->_sec = this->_paddle = 1000; this->_time = this->_pathLength = -1000.0;}
  int id() const { return(this->_id);}
  int setID(int i) { return(this->_id = i);}
  int paddle() const { return(this->_paddle);}
  int setPaddle(int i) { return(this->_paddle = i);}
  int sec() const { return(this->_sec);}
  int setSec(int i) { return(this->_sec = i);}
  double time() const { return(this->_time);}
  double setTime(double t) { return(this->_time = t);}
  double pathLength() const { return(this->_pathLength);}
  double setPathLength(double t) { return(this->_pathLength = t);}
  clasSThit& operator=(const clasSThit& hit);
};
class clasCChit {
 private:
  cc01_t *_cc01;
  ccrc_t *_ccrc;
  tbid_t *_tbid;
  int _status;

 public:
  clasCChit() {this->_cc01 = NULL ; this->_ccrc = NULL;this->_status = 0;}
  void setCC01(cc01_t *cc01) { this->_cc01 = cc01;}
  void setCCRC(ccrc_t *ccrc) { this->_ccrc = ccrc;}
  int status() const {return(this->_status);}
  int status(int s)  {return(this->_status = s);}
  cc01_t *cc01() const { return(this->_cc01);}
  ccrc_t *ccrc() const { return(this->_ccrc);}
  double theta() const { return(this->_status ? (double) this->_ccrc->nrthet/RAD_2_DEG_CONV : -1000.0);}
  double dtheta() const { return(this->_status ? (double) this->_ccrc->nrdthet/RAD_2_DEG_CONV : -1000.0);}
  double npe() const { return(this->_status ? (double) this->_ccrc->nrphe/10.0 : -1000.0);}
  int MeanSegment() const { return(this->_status ? this->_ccrc->nrsegm/10 : -1000);}
  int MinSegment() const { return(this->_status ? this->_ccrc->nrsegm_m/10 : -1000);}
  int MaxSegment() const { return(this->_status ? this->_ccrc->nrsegm_p/10 : -1000);}
  int Sector() const { return(this->_status ? this->_ccrc->nrsect : -1000);}
  int nrPhi() const { return(this->_status ? this->_ccrc->nrphy : -1000);}
  int nrTime() const { return(this->_status ? this->_ccrc->nrtime : -1000);}
  clasCChit& operator=(const clasCChit& hit);
};


class clasTaggedPhoton {
 private:

  tagr_t *_tagr;

 public:
  clasTaggedPhoton(tagr_t *t) { this->_tagr = t;}
  clasTaggedPhoton() { this->_tagr = NULL;}
  double E() {return (this->_tagr ? this->_tagr->erg : 0.0);}
  double ttag() {return (this->_tagr ? this->_tagr->ttag : 0.0);}
  double tpho() {return (this->_tagr ? this->_tagr->tpho : 0.0);}
  int stat() {return (this->_tagr ? this->_tagr->stat : -1);}
  int t_id() {return (this->_tagr ? this->_tagr->t_id : -1);}
  int e_id() {return (this->_tagr ? this->_tagr->e_id : -1);}
  void write(std::ostream& os);
  void TAGRcorrection(float E0, int runID);

};

class clasParticle {
 private:
  friend struct clasEvent;
  BOSbank *_bcs;
  Particle_t _pid;
  int _pidflag;
  int _useseb;
  int _index;
  int _tbidIndex;
  int _ccidIndex;
  int _gpidIndex;
  int _ccadc;
  double _qpid;
  double _qtrk;
  int _sec;
  double _NPE;
  fourVec _p;
  threeVec _pos;
  tbid_t *_tbidptr;
  int _tagrIndex;
  clasEvent *_ev;

  void _eLoss(threeVec &v,int icell,int iflag);
  void _momCorr();
  void _momCorrg1c();
  void _pcorr_and_eLoss(threeVec &v,int icell,int iflag);
  // Mike Williams g1c momentum corrections
  //void _g1cPcorrection(fourVec p4, int sector, int q);
  void _Pcor(fourVec p4, int sector, int q, int runid);
  void _g10pcor(fourVec p4, int q, int torus, int icor);
  void _nbPcor(fourVec p4, int charge);
  void *getEvtBank(const char *name){ return(getBank(this->_bcs,name));}
  void *getEvtBank(const char *name,int sec){ return(getGroup(this->_bcs,name,sec));}


 public:
  clasParticle();
  int IsLepton(int tryTOF); // return 1 for a positron, -1 for an electron and 0 for  anything else. Uses EC/CC cuts only
  int IsLepton(){return(this->IsLepton(0));} // if tryTOF is not given, default is to NOT use the TOF for lepton id
  int IsLepton_EC(); // IsLepton - EC part
  int IsLepton_ECtot(); // IsLepton - EC total vs P part
  int IsLepton_ECio(); // IsLepton - EC inner vs EC outer part
  int IsLepton_ECin2EC(float ecin_ec_lo, float ecin_ec_hi); // IsLepton - ECin/EC part
  int IsLepton_CC(); // IsLepton - CC part
  int IsLepton_MM2(); // IsLepton - TOF MM2 part
  int MaybeLepton(); // IsLepton with CC status cut instead of Nphe
  int IsLeptonPID(); // function based on original pid
  int IsLeptonOld(); // original IsLepton by M. Guillo

  // Thi lepton code was developed for the g7a run.  In order to not break any
  // code with the calls to G7 functions, these wrappers are put in place
  int IsLeptonG7(){ return(this->IsLepton());}
  int IsLeptonG7_EC(){ return(this->IsLepton_EC());}
  int IsLeptonG7_ECtot(){ return(this->IsLepton_ECtot());}
  int IsLeptonG7_ECio(){ return(this->IsLepton_ECio());}
  int IsLeptonG7_ECin2EC(float ecin_ec_lo, float ecin_ec_hi){ return(this->IsLepton_ECin2EC(ecin_ec_lo,ecin_ec_hi));}
  int IsLeptonG7_CC(){ return(this->IsLepton_CC());}
  int IsLeptonG7_MM2(){ return(this->IsLepton_MM2());}
  int MaybeLeptonG7(){ return(this->MaybeLepton());}

  int IsSCkaonG7(); //if PID returns kaon and if SC energy profile fits that of a kaon.

  int IsPhoton() { return(this->_pid == Gamma);}
  Particle_t pid(Particle_t  p) { return(this->_pid = p);}
  int Q() const;
  Particle_t pid() const { return(this->_pid);}
  int tbid(int t) { return(this->_tbidIndex = t);}
  int tbidIndex () {return(this->_tbidIndex);}
  tbid_t *tbidptr() const { return(this->_tbidptr);}
  int trackNo() { return(this->_tbidptr->track);}
  double quality() const { return(this->_qpid);}
  double quality(double qpid) { return(this->_qpid = qpid);}
  int g_stat();
  void setEvent(clasEvent *x) {this->_ev = x;}
  clasEvent *ev() const {return(this->_ev);}
  void write(std::ostream& os);

  //  int verbose() { return(this->_ev->_verbose);}

  threeVec CCdir();
  threeVec CCpos();

  clasCChit CChit();
  clasSThit SThit();
  int isSThit();
  clasSChit SChit();
  int isSChit();
  int isCChit();
  int isEChit();

  // Kossov electron/positron functions

  double ccQF();  // Cerenkov quality factor
  double CS();    // Cerenkov CS factor
  int ccPhiMatch(); // Cerenkov Phi matching inside a sector

  int FidCut(); // return 1 for passed fid cut, 0 for outside fid area
  int triggerParticle(unsigned int t); // find particle causing trigger

  double qtrk(double _qtrk) { return(this->_qtrk = _qtrk);}
  double qtrk() const {return(this->_qtrk);}

  clasParticle reAssign(Particle_t pid);
  int pidFlag() const { return(this->_pidflag);}
  void pidFlag(int flag) { this->_pidflag = flag;}
  void gpidIndex(int i) {this->_gpidIndex = i;}
  int gpidIndex() const {return(this->_gpidIndex);}

  int ccid(int t) { return(this->_ccidIndex = t);}
  void setTBID(tbid_t *ptr) { this->_tbidptr = ptr;}
  void setTAGRindex(int index) { this->_tagrIndex = index;}
  int ccadc(int t) { return(this->_ccadc = t);}
  int ccadc();
  int sec(int t) { return(this->_sec=t);}
  int sec() const { return(this->_sec);}
  double nPhe(double t){ return(this->_NPE = t);}
  int tbid() const { return(this->_tbidIndex);}
  int ccid() const { return(this->_ccidIndex);}
  double nPhe() const { return(this->_NPE);}
  void print() const;
  void Print() const;
  double tprop(); // propagation time using particle vertex
  double tprop(float vz); // propagation time with user supplied z-vertex
  double tVertex();
  double scVertexTime(); // from tof
  double scVertexTimeBeta(); // from p/E
  double beta() const;  // from tof
  double Beta() const;  // p/E
  BOSbank *bcs(BOSbank *b) { return(this->_bcs = b);}
  BOSbank *bcs() const { return(this->_bcs);}
  clasParticle& operator=(const clasParticle& particle);
  clasParticle& p(const fourVec& p4);
  fourVec p() const;
  clasParticle& pos(const threeVec& p3);
  double gamma() const;
  double mass() const;
  double tofLength() const;
  double pdgMass() const;
  double scTrackLen();
  double scPathLen();
  double pathlenST_SC(); // pathlen between ST and SC
  double scTOF();  // time (ns) between vertex sc
  double scTOFexpected();  // given mass, what tof is expected between vertex and sc
  int scPlane(); // plane number of struck TOF plane
  int scPaddleId(); //ut
  int scPaddleId_sector(); //ut
  double scMassSquared(); // calculate the TOF Mass Squared from mom. and beta

  double ttag();
  double tpho();

  int nGammaRF();
  double sc_len();
  double sc_time();
  double st_time();
  double st_len();
  int st_id();
  int st_paddle();
  int st_stat();
  threeVec stPos();
  int sc_stat();
  int tagr_id() const;
  int tagrID() const;

  // EC hit stuff

  echb_t *echbHit();
  echb_t *echbHitIn();
  echb_t *echbHitOut();

  double ecEnergy();
  double ecEnergyIn();
  double ecEnergyOut();
  int ecStatus();
  int ecSector();
  int ecLayer();
  double ecTime();
  double ecVtime();
  threeVec ecPosition();
  threeVec ECdir();
  threeVec ECpos();
  threeVec ECuvw();
  threeVec ECxyz2uvw();
  int ECfidcut();

  // CC hit stuff

  cc01_t *cc01Hit();

  int ccStatus();
  double ccTime();
  double ccVtime();
  double ccNphe();

  // LAC stuff
  int lacStatus();
  double lacTime();
  double lacVtime();
  double lacEtot();
  double lacEin();
  threeVec LACdir();
  threeVec LACpos();

  double scVertexTime(double mass);

  double idChisq(Particle_t pid);

  double scVtime() { return(this->tbidptr() ? this->tbidptr()->sc_vtime: -1000);}
  double stVtime() { return(this->tbidptr() ? this->tbidptr()->st_vtime: -1000);}

  double stTime() { return(this->tbidptr() ? ((this->tbidptr()->st_stat == 2) ? this->tbidptr()->st_time : -1000) : -1000);}

  //ut
  double tbid_beta() { return(this->tbidptr() ? this->tbidptr()->beta: -1000);}
  double tbid_vtime() { return(this->tbidptr() ? this->tbidptr()->vtime: -1000);}
  double tbid_sc_time() { return(this->tbidptr() ? this->tbidptr()->sc_time: -1000);}
  double tbid_sc_beta() { return(this->tbidptr() ? this->tbidptr()->sc_beta: -1000);}

  double tbid_st_time() { return(this->tbidptr() ? this->tbidptr()->st_time: -1000);}
  double tbid_st_beta() { return(this->tbidptr() ? this->tbidptr()->st_beta: -1000);}
  double x();
  double y();

  double z();
  threeVec pos() const;


  //

  threeVec scPos();

  // for photons, return the echb_t structure

  echb_t *echb();

  // Functions used to return proper covariant matrix -----------------------------------------------

  double lenTargetTrack();
  matrix<double> TBERmatrix();
  double TBERq_over_p();
  double TBERlambda();
  double TBERphi();
  matrix<double> getCovMatrix();

};

/// the clasEvent Object
/** clasEvent is a C++ wrapper around BOS data structures
 */
class clasEvent : public event {
 protected:

  int _status;  //< event status
  int _file;    //< file ???
  int _verbose; //< verbose flag
  double _targetZ; //< taget z location
  BOSbank *_bcs;  //< pointer to main BOS bank
  runPeriod_t _runPeriod; //< run period like g1a, ...
  runClass_t _runClass; //< run clas like G1,G2,G6,G7, ...
  clasPART_t *_PART;    //< pointer to clas particles
  clasTBID_t *_TBID;    //< pointer to TBID bosbank
  clasTAGR_t *_TAGR;    //< pointer to TAGR bosbank
  clasTAGM_t *_TAGM;    //< pointer to TAGM bosbank
  clasCC01_t *_CC01;     //< pointer to CC01 bosbank
  clasTBTR_t *_TBTR;    //< pointer to TBTR bosbank
  clasTBER_t *_TBER;    //< pointer to TBER bosbank
  clasCC0_t *_CC;        //< pointer to CC bosbank

  string _prlink;       //< filename for tracking roads

  int _useGPID;         //< ...................
  int _useSEB;
  int _useUnknown;
  int _mc;
  int _noLoad;

  head_t *_head;
  int _partbank;
  tagr_t *_tagr;
  int _tagrIndex;
  tbid_t *_The_tbid;
  mvrt_t *_mvrt;
  vert_t *_vert;
  std::list<string>_newBanks;
  std::list<clasParticle> _cp;
  std::list<clasTaggedPhoton> _tag;
  std::list<clasTaggedPhoton> _tagWindow;
  //  std::list<clasTaggedPhoton> _tag_cp;
  std::vector<int> _tbid;
  int _RawMode;

  // initialization flags
  int _configTBID;
  int _configSWIM;
  int _configPcorr;
  int _configEloss;
  //  int _configg1cPcorrection;
  int _configPcor;
  int _configg10pcor;
  int _configMomentumCorrection;
  int _configEC;
  int _configSC;
  int _configCC;
  int _configTAGGER;
  int _configSCG;
  int _configRunControl;
  int _configGPID;
  int _configTGEO;


  // todo flags
  int _doMomentumCorrection;

  // internal initialization methods
  int _init(char *file,BOSbank *,int partbank,int RawMode);
  int _init(BOSbank *,int partbank,int RawMode);
  int _init(BOSbank *,int RawMode);
  int _load(int partbank);

 public:


  //
  // constructors -----------------------------------------
  clasEvent() { this->_noLoad = this->_status = this->_file = this->_useGPID =  this->_useSEB = 0;dc_set_def_() ; trk_set_def_();this->_useUnknown = 0;}
  clasEvent(char *file,BOSbank *b,int partbank,int RawMode) { this->_status = this->_init(file,b,partbank,RawMode); this->_noLoad = 0;dc_set_def_() ; trk_set_def_();this->_useUnknown = 0;}
  clasEvent(BOSbank *b,int partbank,int RawMode) {this->_status = this->_init(b,partbank,RawMode);this->_noLoad = 0;dc_set_def_() ; trk_set_def_();this->_useUnknown = 0;}
  clasEvent(BOSbank *b,int RawMode) {this->_status = this->_init(b,RawMode);this->_noLoad = 0;dc_set_def_() ; trk_set_def_();this->_useUnknown = 0;}

  //
  // destructor
  ~clasEvent();

  void newBanks();
  void dropNewBanks();

  // copy
  void copy(clasEvent &evt);

  // monte carlo functions

  double weight();
  int eventCode();
  nuclear_t g7Target();

  // clasEvent configuration ------------------------------------
  int mc() {return(this->_mc);}
  void useGPID() { this->_useGPID = 1; this->_useSEB = 0;}
  void usePART() { this->_useGPID = this->_useSEB = 0;}
  void useSEB() { this->_useSEB = 1; this->_useGPID = 0;}
 void useUnknown() {this->_useUnknown = 1;}
  int gpid() { return(this->_useGPID);}
  int seb() { return(this->_useSEB);}
  runPeriod_t runPeriod() { return(this->_runPeriod);}
  runClass_t runClass() { return(this->_runClass);}

  // return beam type from RUNC bank
  int RUNC_BeamType();

  // rebuilding banks
  void buildTBID(int bankNo=1);

  // define event types
  int scaler() {return (this->type() == SCALER_EVENT);}

  //
  // configuring
  void setPrlink(const char *c) { this->_prlink = c;}
  string prlink() {return(this->_prlink);}
  string defaultPrlink();
  void reconfigure();
  void ConfigSwim();
  void ConfigSC();
  void ConfigSCG();
  void ConfigCC();
  void ConfigEC();
  void ConfigTAGGER();
  void ConfigRunControl();
  void ConfigTGEO();
  int configTGEOStatus() {return(this->_configTGEO);}
  int configSCStatus() { return(this->_configSC);}
  int configSCGStatus() { return(this->_configSCG);}
  int configSwimStatus() { return(this->_configSWIM);}
  int configECStatus() { return(this->_configEC);}
  int configCCStatus() { return(this->_configCC);}
  int configPcorrStatus() { return(this->_configPcorr);}
  int configElossStatus() { return(this->_configEloss);}
  //  int configg1cPcorrectionStatus() { return(this->_configg1cPcorrection);}
  int configPcorStatus() { return(this->_configPcor);}
  int configg10pcorStatus() { return(this->_configg10pcor);}
  int configMomentumCorrectionStatus() { return(this->_configMomentumCorrection);}
  int configTAGGERStatus() { return(this->_configTAGGER);}
  int configTBIDStatus() { return(this->_configTBID);}
  int configRunControlStatus() { return(this->_configRunControl);}


  // Data loading - reloading ------------------------------------------------------
  void open(char *file,BOSbank *b,int partbank,int RawMode) { this->_status = this->_init(file,b,partbank,RawMode);}
  int status() { return(this->_status);}
  int status(int s) { return(this->_status = s);}
  int verbose() { return(this->_verbose);}
  int verbose(int s) { return(this->_verbose = s);}
  int read();
  int read(int);
  int init(char *file,BOSbank *b,int partbank,int RawMode) {return( this->_status = this->_init(file,b,partbank,RawMode));}
  int init(BOSbank *b,int partbank,int RawMode) {return( this->_status = this->_init(b,partbank,RawMode));}
  int init(BOSbank *b,int RawMode) {return( this->_status = this->_init(b,RawMode));}
  int reload(int partbank) { return(this->_load(partbank));}
  void clean();

  void Print() const;
  void write(std::ostream& os);

  // header information ------------------------------------------------
  head_t *head() { return(this->_head);}
  int run();
  int run(int newRun) { return(this->_head ? this->_head->nrun = newRun : 0);}
  int event();
  int trig();
  int latch();  // trigger latch from TGBI
  int trigBits() { return(this->_head ? this->_head->trigbits : 0);}
  int time() { return(this->_head ? this->_head->time : 0);}
  int type();

  int evtClass();
  int triggerMask(unsigned int t);

  uint32 latch1(); //< this is the latch1 word from the TGBI bank
  uint32 latch2(); //< this is the latch2 word from the TGBI bank

  //
  // kinematics/vertex --------------------------------------------------------

  // Mandelstam s
  double s() const {  return((this->beam().get4P() + this->target().get4P()).lenSq());}

  // missing mass squared
  double MMsq();

  // V(): returns the vertex from MVRT - will create it if it does not exist
  threeVec V();
  threeVec v();  // Here to get around an mc problem...
  int V_ntrk();
  float V_chi2();
  double x();
  double y();
  double z();
  double zTarget(); // z of the target from TGEO
  threeVec vert2(clasParticle &p1,clasParticle &p2);
  threeVec vert1(clasParticle &p1);
  double vert2Separation(clasParticle &p1,clasParticle &p2);
  double vert1Separation(clasParticle &p1);
  double g7EpEmMass(); // calculate e+e- invariant mass with g7 cuts
  int EpEm_EC_CC(); // check if the event has e+e- pair from EC/CC cuts
  int g7EpEm_EC_CC(){return(this->EpEm_EC_CC());} // wrapper for older g7 function call

  // configuration

  void noLoad() { this->_noLoad = 1;}

  // function to remake event -----------------------------------------------

  // make the TAGR bank

  void makeTAG() { tag_evnt_(); this->_TAGR= (clasTAGR_t *) getBank(this->_bcs,"TAGR");}
  void setTAGRindex(int index) { if (_TAGR) {this->_tagrIndex = index; this->_tagr=&(_TAGR->tagr[index]);}}
  int tagr_id();

  // make the TAGM bank
  void makeTAGM();
  // make GPID bank

  clasGPID_t * MakeGPID(int partbank);

  // Print
  void printGampEvent(ostream& os);

  // build the swim banks
  void buildSwim() {this->buildSwim(1);}
  void buildSwim(int Sector1);
  void makeVertex();
  void convertNeutrons(int partbank);
  void remakeNeutrals(int partbank,double neutronBetaCut);
  void makePartPi0(int partbank,int addBank);

  // clasTaggedPhoton code --------------------------------------------------

  clasEvent& addTaggedPhoton(const clasTaggedPhoton& tag);
  clasEvent& addTaggedPhotonWindow(const clasTaggedPhoton& tag);
  int Ntag() const { return(this->_tag.size());}
  int NtagWindow() const {return(this->_tagWindow.size());}
  clasTaggedPhoton ntag(int i) const;
  list<clasTaggedPhoton>tag() { return(this->_tag);}
  list<clasTaggedPhoton>createTaggedPhoton(double t1,double t2);
  list<clasTaggedPhoton>createTaggedPhoton(double t1);

  // crude photon flux

  int nTagged(double t0,double t1,double E0,double E1);

  // Various tagger corrections

  double mwTAGRcorrection(double epho);
  double ssTAGRcorrection(double epho, int Eid);
  double lgTAGRcorrection(double epho, int Eid);
  double vkTAGRcorrection(float epho);

  // count number of tracks

  int nTracks();

  // kinematic fitting

    void fitPart(int bankNo,clasKineFit  &Fit);


  // clasParticle code -------------------------------------------------------
  clasEvent& addParticle(const clasParticle& cp);
  clasEvent& deleteParticle(std::list<clasParticle>::iterator cp);
  double beta(Particle_t pid,int index);
  int Ncp() const {return(this->_cp.size());}
  int N(int q) const; // return the number of particles with charge q
  int nIsLepton(); //return the number of leptons
  int nIsLepton(int q); //return the number of leptons with charge q
  int IndexIsLepton(); //return cp index of lepton
  int IndexIsLepton(int q); //return cp index lepton with charge q
  clasParticle cpLepton(int i); //return the ith Lepton (counting from 1)
  clasParticle cpLepton(int q,int i); //return the ith Lepton with charge q  (counting from 1)
  int nMaybeLepton(); //return the number of leptons
  int nMaybeLepton(int q); //return the number of leptons with charge q
  clasParticle cpMaybeLepton(int i); //return the ith MaybeLepton (counting from 1)
  clasParticle cpMaybeLepton(int q,int i); //return the ith MaybeLepton with charge q (counting from 1)

  // The lepton code was developed for the g7a run.  In order to not break any
  // code with the calls to G7 functions, these wrappers are put in place
  int nIsLeptonG7(){return(this->nIsLepton());}
  int nIsLeptonG7(int q){return(this->nIsLepton(q));}
  int IndexIsLeptonG7(){return(this->IndexIsLepton());}
  int IndexIsLeptonG7(int q){return(this->IndexIsLepton(q));}
  clasParticle g7Lepton(int i){return(this->cpLepton(i));}
  clasParticle g7Lepton(int q,int i){return(this->cpLepton(q,i));}
  int nMaybeLeptonG7(){return(this->nMaybeLepton());}
  int nMaybeLeptonG7(int q){return(this->nMaybeLepton(q));}
  clasParticle g7MaybeLepton(int i){return(this->cpMaybeLepton(i));}
  clasParticle g7MaybeLepton(int q,int i){return(this->cpMaybeLepton(q,i));}

  void printParticles() const;

  clasParticle cp(int i) const;
  clasParticle cp(Particle_t pid,int index) const;
  clasParticle cp(int q,int index) const; // Get the index_th particle with charge q
  std::list<clasParticle>::iterator xcp(Particle_t pid,int i);


  // DC1

  int makeDC1() { return(make_dc1_());}


  // TAGR --------------------------------------------------------------------

  clasTAGR_t *TAGR() { return(this->_TAGR);}
  clasTAGM_t *TAGM() { if (!this->_TAGM) this->makeTAGM(); return(this->_TAGM);}
  tagr_t *tagr() { return(this->_tagr);}
  tagr_t *setTagr(tagr_t *t) { return(this->_tagr = t);}
  int tagrStatus() { return(this->_tagr ? this->_tagr->stat : 0);}
  double tpho() { return(this->_tagr ? this->_tagr->tpho : -1000);}
  double ttag() { return(this->_tagr ? this->_tagr->ttag : -1000);}
  double Egamma() {  return(this->_tagr ? this->_tagr->erg : -1000);}
  double vtime();
  double scVtime() { return(this->_TBID ? this->_TBID->tbid[0].sc_vtime: -1000);}
  double stVtime();
  double vpho() { return(this->tpho() + this->z()/LIGHT_SPEED);}
  int tagrID() { return(this->_tagr ? this->_tagr->t_id : -1000);}
  int tagrEID() { return(this->_tagr ? this->_tagr->e_id : -1000);}


  float vtime(Particle_t pid,int index) { return(this->_TBID ? this->_TBID->tbid[cp(pid,index).tbid() - 1].vtime : 0);}
  float tbidBeta(Particle_t pid,int index) { return(this->_TBID ? this->_TBID->tbid[cp(pid,index).tbid() - 1].beta : 0);}

  fourVec getBeam(double E0,double E1);

  // PART --------------------------------------------------------------------

  clasPART_t *PART() {return(this->_PART);}

  // TRGS
  clasTRGS_t *TRGS() {return((clasTRGS_t *) getBank(&bcs_,"TRGS"));}
  clasTRGS_t *TRGS(int sec) {return((clasTRGS_t *) getGroup(&bcs_,"TRGS",sec));}

  // CALL bank (RF) ---------------------------------------------------------

  clasCALL_t *RF() { return ((clasCALL_t *) getGroup(this->_bcs,"CALL",0));}

  // TLV1 bank --------------------------------------------------------

  clasTLV1_t *TLV1() { return((clasTLV1_t *) getBank(this->_bcs,"TLV1"));}

  // TAGI bank ---------------------------------------------------------

  clasTAGI_t *TAGI() { return ((clasTAGI_t *) getBank(this->_bcs,"TAGI"));}

  //TAGE -------------------------------------------------------------------
  clasTAGE_t *TAGE() { return ((clasTAGE_t *) getBank(this->_bcs,"TAGE"));}
  //TAGT
  clasTAGT_t *TAGT() { return ((clasTAGT_t *) getBank(this->_bcs,"TAGT"));}

  // TBTR and TBID ---------------------------------------------------------------------
  clasTBTR_t *TBTR() {return(this->_TBTR);}
  clasTBID_t *TBID() {return(this->_TBID);}

  // ST STR
  clasSTR_t *STR() { return ((clasSTR_t *) getBank(this->_bcs,"STR"));}

  // HBTR ---------------------------------------------------------------------
  clasHBTR_t *HBTR() { return ((clasHBTR_t *) getBank(this->_bcs,"HBTR"));}

  // ST,ST1  ---------------------------------------------------------------------
  clasST_t *ST() { return ((clasST_t *) getBank(this->_bcs,"ST"));}
  clasST1_t *ST1() { return ((clasST1_t *) getBank(this->_bcs,"ST1"));}

  // CALL  ---------------------------------------------------------------------
  clasCALL_t *CALL() { return ((clasCALL_t *) getBank(this->_bcs,"CALL"));}


  // SC ----------------------------------------------------------------------
  clasSC_t *SC() { return((clasSC_t *) getBank(&bcs_,"SC  "));}
  int nHitsSC(int sector) { clasSC_t *sc = (clasSC_t *)getGroup(&bcs_,"SC  ",sector); return(sc ? sc->bank.nrow : 0);}
  // DC0 ----------------------------------------------------------------------

  clasDC0_t *DC0() { return((clasDC0_t *) getBank(&bcs_,"DC0 "));}
  clasDC0_t *DC0(int sec) { return((clasDC0_t *) getGroup(&bcs_,"DC0 ",sec));}
  int nHits(int sector) { clasDC0_t *dc = (clasDC0_t *)getGroup(&bcs_,"DC0 ",sector); return(dc ? dc->bank.nrow : 0);}

  // TDPL ---------------------------------------------------------------------
  clasTDPL_t *tdpl() { return( (clasTDPL_t *) getBank(this->_bcs,"TDPL"));}


  // general pointer not included above

  void *getEvtBank(const char *name){ return(getBank(this->_bcs,name));}
  void *getEvtBank(const char *name,int sec){ return(getGroup(this->_bcs,name,sec));}
  // drop banks

  void *dropEvtBank(char *name,int groupNo = 0) { return(dropBank(this->_bcs,name,groupNo));}
  void dropEvtAllBanks(char *name) { return(dropAllBanks(this->_bcs,name));}

  // add banks

  void *addEvtBank(char *name,int sizen,int n,int groupNo = 0) {return(makeBank(this->_bcs,name,groupNo,sizen,n));}

  int load(int partbank) { return(this->_load(partbank));}

  int N(Particle_t);
  int N();
  Particle_t partID(int partIndex);
  fourVec getPfinal(Particle_t pid,int index);
  double beamTime() { return(this->_tagr ? this->_tagr->tpho : -1000.0);}

  BOSbank *bcs() { return(this->_bcs);}

  // Energy loss
  void eLoss();
  void eLoss(char *,int);
  void momentumCorrection();
  void pcorr_eLoss(char *cell = "g1c",int iflag = 1);
  // Mike Williams momentum correction for g1c -----------------
  //    void g1cPcorrection();
  void Pcor();
  void nbPcor();
  void g10pcor(int icor);

  // RUNC information

  double torusCurrent();
  double minitorusCurrent();


  // bosDump

  void bosDump(std::list<string> &banks);
  void bosDump();

  // The a1c Modules



  void doHitBasedTracking();
  void doHBTR();
  void doTimeBasedTracking();

  void doLAC();
  void doCC();
  void doSC();
  void doEC();
  void doTAGGER();



};


class clasOutput {
 private:
  BOSbank *_bcs;
  int _unit;
  char *_name;
  int _nwrite;
  int _status;
  int _init(char *name,int unit);
  int _append(char *name,int unit,char *mode);
 public:
  clasOutput() {this->_status = this->_nwrite = 0;}
  clasOutput(char *name,int unit) {this->_init(name,unit);}
  clasOutput(char *name,int unit,char *mode) {this->_append(name,unit,mode);}
  int init(char *name,int unit) { return(this->_status = this->_init(name,unit));}
    int write(BOSbank *b)
    {
        this->_nwrite++;
        this->_bcs = b;
        return (this->_status = putBOS(b,this->_unit,(char*)"E"));
    }
    int write(BOSbank *b,char *list)
    {
      this->_nwrite++;
      this->_bcs = b;
      return(this->_status = putBOS(b,this->_unit,list));
    }
  int close();
  int status() { return(this->_status);}
  int nwrite() { return(this->_nwrite);}
  int unit() { return(this->_unit);}
  char *name() { return(this->_name);}
  int open(char *name,int unit) { return(this->_init(name,unit));}
  int append(char *name,int unit) { return(this->_append(name,unit,(char*)"A"));}
};


#endif /*CLASEVENT_H_*/
