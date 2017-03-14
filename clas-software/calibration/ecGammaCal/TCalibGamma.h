#ifndef TCALIBGAMMA_H
#define TCALIBGAMMA_H

    
extern "C" {
#include <ntypes.h>
#include <bostypes.h>
#include <pid.h>
}

#include <Vec.h>


class TCalibGamma
{

 public:
  
  // ----------------------
  // Initialisation methods
  // ----------------------
  
  TCalibGamma(); 
  TCalibGamma(BOSbank* pBcs, int nGammaIndex, double dMinEnergy = 0.1, int nPartBank = 1) { this->Set(pBcs, nGammaIndex, dMinEnergy, nPartBank); }
  void Set(BOSbank* pBcs, int nGammaIndex, double dMinEnergy = 0.1, int nPartBank = 1);

  // ------------------------------------
  // Info about the quality of the photon
  // ------------------------------------
  
  bool IsGoodPhoton() const { return(m_bGoodPhoton); }
  bool FoundBothEcpc() const { return(m_bFoundBothEcpc); }
  bool FoundBothEchb() const { return(m_bFoundBothEchb); }
  bool FoundAllBanks() const { return(m_bFoundAllBanks); }
  bool IsFiducial() const { return(m_pTbid ? (m_pTbid->ec_stat == GOOD_MATCH) : false); }
  
  // -----------------------------
  // ECPC bank access data methods
  // -----------------------------
  
  double Adc(int nLayer, int nEcpcIndex) const;
  double Tdc(int nLayer, int nEcpcIndex) const;
  double AdcMax(int nLayer, int nView) const;
  double TdcMax(int nLayer, int nView) const;
  int TubeIdMaxAdc(int nLayer, int nView) const;   // tube id with max ADC
  int TubeIndexMaxAdc(int nLayer, int view) const; // tube index (for one dimension array) corresponding to the tube id with max ADC
    // for a given sector, view, layer and tube id, returns an index to be used in a one dimension array. The indexes are following map convention.
  static int TubeIndex(int nSector, int nLayer, int nView, int nTubeId) { return((nLayer - 1) * (m_c_nViews * m_c_nSectors * m_c_nTubes) + (nView -1) * (m_c_nSectors * m_c_nTubes) + (nSector -1) * m_c_nTubes + (nTubeId - 1)); }

  // ----------------------------
  //  Useful distances for timing
  // ----------------------------

  // distance from the vertex to the inner surface of the calorimeter
  double DistVertex2Hit() const;
  // distance from EC inner surface to the view edges.
  double DistHit2View(int nLayer, int nView) const;

  // -----------------
  // Utilities methods
  // -----------------
  
  // vertex position
  static threeVec Clas2Local(double dX, double dY, double dZ);
  static threeVec Clas2Local(const threeVec& v3ClasCoord);
  threeVec Vertex() const { return(m_pMvrt ? threeVec(m_pMvrt->vert.x, m_pMvrt->vert.y, m_pMvrt->vert.z) : m_c_v3Error) ;}
 // centroid position of the shower (CLAS system coordinates)
  threeVec CentroidPosition() const { return(m_pEchbWhole ? threeVec(m_pEchbWhole->x_hit, m_pEchbWhole->y_hit, m_pEchbWhole->z_hit) : m_c_v3Error); }
  int    Sector() const { return(m_pTbid ? m_pTbid->sec : m_c_nError); }
  double Energy() const { return(m_pPart ? m_pPart->p.t : m_c_dError); }
  double Beta() const { return(m_pTbid ? m_pTbid->beta : m_c_dError); }
  double VertexTime() const;     // vertex time from TBID bank
  int    HitsInSameSector() const;   // number of EC hits in same sector than photon
  bool   IsHitInLayer(int nLayer) const;  // hit in a given layer?
  int    Reject() const;    // flags the photon not to be used for further analysis.
  // converts from CLAS system coordinates (dX, dY, dZ) into EC local system coordinates 
  void   Print() const;

 private:
  
  // ---------------
  // private methods
  // ---------------
  
  bool m_InitEchb();  // try to find inner and outer ECHB banks
  bool m_InitEcpc();  // try to find inner and outer ECPC banks
  int  m_EcpcIndexMaxAdc(int nLayer, int view) const; // index of ecpc which has the greatest ADC value.
  
  // ---------------
  // private members
  // ---------------
  
  BOSbank* m_bcs;         // pointer on the bcs BOS array.
  // set of flags to see how photon can be used if photon useful or not
  bool m_bFoundAllBanks;  // existence of PART, ECHB, TBID and MVRT banks ?
  bool m_bFoundBothEchb;  // "inner" and "outer" ECHB banks have been found ?
  bool m_bFoundBothEcpc;  // "inner" and "outer" ECPC banks have been found ?
  bool m_bGoodPhoton;     // status of photon (good for fit or not)
  
  // pointers on the different banks. 
  part_t*     m_pPart;       // pointer to part bank corresponding to photon.
  tbid_t*     m_pTbid;       // pointer to tbid bank corresponding to photon.
  mvrt_t*     m_pMvrt;       // pointer to mvrt (vertex bank)  corresponding to event.
  clasECPC_t* m_pEcpcInner;  // pointer to the inner ECPC bank
  clasECPC_t* m_pEcpcOuter;  // pointer to the outer ECPC bank
  echb_t*     m_pEchbWhole;  // pointer to the whole ECHB bank
  echb_t*     m_pEchbInner;  // pointer to the inner ECHB bank
  echb_t*     m_pEchbOuter;  // pointer to the outer ECHB bank
  
  // photon info for debugging purpose
  int m_nRunNumber;          // run number
  int m_nEventNumber;        // event number
  int m_nPartIndex;          // index of the photon in the PART bank (C-style)
  int m_nTbidIndex;          // index of the photon in the TBID bank (C-style)
  int m_nEchbIndex;          // index of the photon in the ECHB bank (C-style)

 public:

  // ----------------------------------------------------------------
  // constant static members (public so can be accessed from outside)
  // ----------------------------------------------------------------

  // hardware constants
  static const int m_c_nSectors = 6;  // 6 sectors in CLAS
  static const int m_c_nViews = 3;    // 3 views in calorimeter (U, V and W)
  static const int m_c_nLayers = 2;   // 2 layers in calorimeter ("in" and "out")
  static const int m_c_nTubes = 36;   // 36 tubes per layer, view and sector
  static const double m_c_dDistanceOrigine2EcInner =  510.32; // distance (in cm) from CLAS origine coordinates to EC origine coordinates
  static const double m_c_dECTheta = 0.4363323; // azimutal angle (in rad) for orifine of EC local coordinates 

  // banks convention constants
  static const int m_c_nInnerEcpc = 10;  // inner ECPC code convention.
  static const int m_c_nOuterEcpc = 20;  // outer ECPC code convention.
  static const int m_c_nWholeEchb = 9;   // whole ECHB code convention.
  static const int m_c_nInnerEchb = 10;  // inner ECHB code convention.
  static const int m_c_nOuterEchb = 11;  // outer ECHB code convention.
  
  static const int m_c_nWhole = 0;      // code for Whole layer in switch statement
  static const int m_c_nInner = 1;      // code for Inner layer in switch statement
  static const int m_c_nOuter = 2;      // code for Outer layer in switch statement
  
  // const error code
  static const double m_c_dError = -1000.;
  static const int m_c_nError = -1000;
  static const threeVec m_c_v3Error;
  static const int m_c_nOk = 1;
  
  // misc const
  static const double m_c_dPi = 3.141592653589793;
  static const double m_c_dSpeedOfLight = 29.9792458;  // speed of light in air (in cm/ns) 

  // ClassDef (TCalibGamma, 0)

};

#endif







