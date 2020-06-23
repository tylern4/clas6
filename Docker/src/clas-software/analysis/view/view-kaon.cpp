/**
 * view-kaon.cpp
 *
 * for use with makent/makeht scripts.
 * first, create the ascii ntuple:
 * > view-kaon in.bos | cview 'KAON ' > kaon.view
 *
 * then make the root macro with makent:
 * > view-kaon -l > kaon.lab
 * > makent kaon kaon.lab > kaon.C
 * > root kaon.C
 *
 * the you can make the C++ analysis program with makeht:
 * > view-kaon -l | makeht > kaon.cpp
 *
 * compile kaon.cpp as per instructions at botton of kaon.cpp file
 *
 * > cat kaon.view | kaon outfile.root
 * > root outfile.root
 * root[0] .ls // to get a listing of histograms created
 **/
#include <cmath>
#include <map>
#include <vector>

#include "view.hpp"

double scE(clasParticle cpi){
  double scE_kp;
  clasSCRC_t *SCRC = (clasSCRC_t *) getGroup(&bcs_,"SCRC",cpi.sec());
      if(SCRC){
    for (int j = 0; j < SCRC->bank.nrow; ++j) {
      scrc_t *scrc = &SCRC->scrc[j];
      scE_kp = scrc->energy;
    }
    return scE_kp;
      } else {
    return -1000;
      }
}

bool processEvent(clasEvent &event, bool verbose, bool silent) {

  int run;
  clasParticle kp, km;
  int n = event.N();

  double kp_scE, km_scE;
  double kpe, kpx, kpy, kpz, kpb;
  double kpen, kpenIn, kpenOut, kpt, kpst;
  double kme, kmx, kmy, kmz, kmb;
  double kmen, kmenIn, kmenOut, kmt, kmst, kimsq;
  double kstvt, kvt;

  double evt_zvert, km_zvert, kp_zvert;

  double beamen;

  if((event.N(KPlus) == 1) && (event.N(KMinus) == 1)){

    kp = event.cp(KPlus,1);
    km = event.cp(KMinus,1);

    evt_zvert = event.z();
    km_zvert = km.z();
    kp_zvert = kp.z();

    kp_scE = scE(kp);
    km_scE = scE(km);

    fourVec kplus = kp.p();
    kpe = kplus.t();
    kpx = kplus.x();
    kpy = kplus.y();
    kpz = kplus.z();
    kpb = kp.beta();

    kpen = kp.ecEnergy();
    kpenIn = kp.ecEnergyIn();
    kpenOut = kp.ecEnergyOut();
    kpt = kp.ecTime();
    kpst = kp.sc_time();

    fourVec kmin = km.p();
    kme = kmin.t();
    kmx = kmin.x();
    kmy = kmin.y();
    kmz = kmin.z();
    kmb = km.beta();

    kmen = km.ecEnergy();
    kmenIn = km.ecEnergyIn();
    kmenOut = km.ecEnergyOut();
    kmt = km.ecTime();
    kmst = km.st_time();

    fourVec k1 = kplus + kmin;
    kimsq = k1.t()*k1.t() - k1.r()*k1.r();

    fourVec beam = event.beam().get4P();
    beamen = beam.t();

    kstvt = event.stVtime();
    kvt = event.vtime();


  }  else return false;

  cout << "kaon"
       << ' ' << event.run()
       << ' ' << n
       << ' ' << kp_scE
       << ' ' << kpe
       << ' ' << kpx
       << ' ' << kpy
       << ' ' << kpz
       << ' ' << kpb
       << ' ' << kpen
       << ' ' << kpenIn
       << ' ' << kpenOut
       << ' ' << kpt
       << ' ' << kpst
       << ' ' << km_scE
       << ' ' << kme
       << ' ' << kmx
       << ' ' << kmy
       << ' ' << kmz
       << ' ' << kmb
       << ' ' << kmen
       << ' ' << kmenIn
       << ' ' << kmenOut
       << ' ' << kmt
       << ' ' << kmst
       << ' ' << kimsq
       << ' ' << kstvt
       << ' ' << kvt
       << ' ' << beamen
       << ' ' << evt_zvert
       << ' ' << kp_zvert
       << ' ' << km_zvert
       << endl;

  return true;


}

#define COLUMN(vec, tag, desc) vec.push_back(make_pair(tag,desc));

void printLabels() {
    string name = "KAON";
    string desc = "proton pi+";

    /// vector< ntuple_id, description > columns
    vector<pair<string, string> > cols;
    COLUMN(cols, "run", "run number")
      COLUMN(cols, "n", "number of events")
      COLUMN(cols, "kp_scE", "K+ SC energy")
      COLUMN(cols, "kpe", "K+ energy")
      COLUMN(cols, "kpx", "K+ x momentum")
      COLUMN(cols, "kpy", "K+ y momentum")
      COLUMN(cols, "kpz", "K+ z momentum")
      COLUMN(cols, "kpb", "K+ beta")
      COLUMN(cols, "kpen", "K+ ecEnergy")
      COLUMN(cols, "kpenIn", "K+ ecEnergy in")
      COLUMN(cols, "kpenOut", "K+ ecEnergy out")
      COLUMN(cols, "kpt", "K+ ecTime")
      COLUMN(cols, "kpst", "K+ st_time")
      COLUMN(cols, "km_scE", "K- SC energy")
      COLUMN(cols, "kme", "K- energy")
      COLUMN(cols, "kmx", "K- x momentum")
      COLUMN(cols, "kmy", "K- y momentum")
      COLUMN(cols, "kmz", "K- z momentum")
      COLUMN(cols, "kmb", "K- beta")
      COLUMN(cols, "kmen", "K- ecEnergy")
      COLUMN(cols, "kmenIn", "K- ecEnergy in")
      COLUMN(cols, "kmenOut", "K- ecEnergy out")
      COLUMN(cols, "kmt", "K- ecTime")
      COLUMN(cols, "kmst", "K- st_time")
      COLUMN(cols, "kimsq", "invariant mass squared")
      COLUMN(cols, "kstvt", "K stVtime")
      COLUMN(cols, "kvt", "K Vtime")
      COLUMN(cols, "beamen", "beam energy")
      COLUMN(cols, "evt_zvert", "Event vertex")
      COLUMN(cols, "kp_zvert", "K+ vertex")
      COLUMN(cols, "km_zvert", "K- vertex")

    unsigned int nlab = 0;
    vector<pair<string, string> >::iterator itr = cols.begin();
    cout << "Labels for " << name << " (" << desc << "):" << endl;
    while(itr != cols.end()) {
        cout << ++nlab
            << "\t" << itr->second
            << " {" << itr->first << "}"
            << endl;
        ++itr;
    }
}
