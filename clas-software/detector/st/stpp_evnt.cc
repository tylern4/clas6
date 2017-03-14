#include <vector>
#include "stpp_JCalibration.h"
#include "stpp_STdata.h"
#include "stpp_STposition.h"
#include "stpp_SThit.h"
#include "stpp_STvector.h"
#include "stpp_JBosBank.h"

extern "C" {
/*!
  fortran handle to call stpp_evnt
  \param trk_level  0 = hit based (HDPL)  1 = time based (TDPL)    2 = use RGLK
*/
  void stpp_evnt_(int* trk_level);
}

JCalibration* gCalib = NULL;

void stpp_add_hits(vector<SThit>& hit, int sector, STvector& data, const STposition& pos) {
  if (pos.Found()) {                         /// plane good ?

    /// search best ST hit
    int inx = data.BestSegment(sector, pos.GetSegment8() );

    if (inx >= 0 ) {                         /// found a hit (several?)

      /// loop over hits
      for (dataiter_t i=data.begin(inx); i!= data.end(inx); i++) {
        hit.push_back(SThit(*i, pos));    /// store hit in container
      }
    }
  }
}

/*!
  top level routine to handle ST++ event
  \param trk_level  0 = hit based (HDPL)  1 = time based (TDPL)    2 = use RGLK
*/
void stpp_evnt(int trk_level) {

  dropAllBanks(&bcs_, "ST1 ");                     /// drop BOS banks (if any)

  if (trk_level == 0) {
    dropAllBanks(&bcs_, "STR ");
    dropAllBanks(&bcs_, "STRE");
  }
  else {                                           /// but keep results from other
    dropBank(&bcs_, "STR ", trk_level);       /// levels unless trk_level==0
    dropBank(&bcs_, "STRE", trk_level);       /// levels unless trk_level==0
  }

  STvector data;

  vector<SThit> myhit;                             /// container for results


  for (int sector = 1; sector<=6; sector++) {        /// loop sector

    /// trk_level == 2 --> use RGLK banks
    if (trk_level == 2) {
      Jrglk rglk(sector);
      if (rglk.Found()) {                            /// RGLK available for sector?
        for (int irglk=0; irglk<rglk.GetNrows(); irglk++) {
          if (rglk[irglk].status == 3) {             /// satisfied with track quality?

            /// get one point of the track from RGLK bank
            JVector3 pnt(rglk[irglk].x,
                 rglk[irglk].y,
                 rglk[irglk].z);

            /// get direction of the track from RGLK bank
            JVector3 dir;
            dir.SetMagThetaPhi(1.,rglk[irglk].rtheta/DEG, rglk[irglk].rphi/DEG);

            /// calculate vertex position from RGLK bank
            JVector3 vert;
            vert.SetMagThetaPhi(1., 90./DEG, rglk[irglk].rphi/DEG);
            vert = pnt -  (pnt*vert) / (dir*vert) * dir;

            /// construct hit position = track intersection with ST plane
            STposition pos(sector, vert, dir);
            pos.SetTrackIndex(irglk);
            stpp_add_hits(myhit, sector, data, pos); /// add hits (if any)
          }
        }
      }
    }

    /// trk_level != 2 --> use [HT]DPL banks
    else {
      Jxdpl xdpl(sector, trk_level!=0);              /// 1,true = TDPL,  0,false = HDPL

      if (xdpl.Found()) {                            /// [HT]DPL available for sector?
        for (int trk = 0; trk<xdpl.GetNrows(); trk++) {
          STposition pos(sector, trk, trk_level!=0); /// ST plane
          stpp_add_hits(myhit, sector, data, pos);   /// add hits (if any)
        }
      }
    }
  }

  /// copy data information into ST1 bank
  int nst1 = data.size();
  if (nst1) {
    clasST1_t *ST1=NULL;
    if (ST1 = (clasST1_t*) makeBank(&bcs_, "ST1 ", 0, sizeof(st1_t)/4, nst1)) {
      int k=0;
      for (int j=0; j<ST_MAX; j++) {
        for (dataiter_t i=data.begin(j); i!= data.end(j); i++) {
          i->FetchToBos( (ST1->st1) + (k++) );
        }
      }
    }
  }


  /// copy hit information into STR and STRE bank
  if (myhit.size()) {
    clasSTR_t  *STR =NULL;
    clasSTRE_t *STRE=NULL;
    if ((STR  = (clasSTR_t*)  makeBank(&bcs_, "STR ", trk_level, sizeof(str_t)/4,  myhit.size())) &&
    (STRE = (clasSTRE_t*) makeBank(&bcs_, "STRE", trk_level, sizeof(stre_t)/4, myhit.size()))) {
      for (int i=0; i<myhit.size(); i++) {
        myhit[i].FetchToBos( (STR->str)   + (i) );
        myhit[i].FetchToBos( (STRE->stre) + (i) );
      }
    }
  }
}

void stpp_evnt_(int* trk_level) {
  stpp_evnt(*trk_level);
}
