using namespace std;


#include <iostream>
#include <cmath>
#include "TCalibGamma.h"

// ClassImp (TCalibGamma);

//////////////////////////////////////////////////////////
//        constant static data members
//////////////////////////////////////////////////////////

// hardware constants
const int TCalibGamma::m_c_nSectors;
const int TCalibGamma::m_c_nViews;
const int TCalibGamma::m_c_nLayers;
const int TCalibGamma::m_c_nTubes;
const double TCalibGamma::m_c_dDistanceOrigine2EcInner;
const double TCalibGamma::m_c_dECTheta;

// banks convention constants
const int TCalibGamma::m_c_nInnerEcpc;
const int TCalibGamma::m_c_nOuterEcpc;
const int TCalibGamma::m_c_nWholeEchb;
const int TCalibGamma::m_c_nInnerEchb;
const int TCalibGamma::m_c_nOuterEchb;

const int TCalibGamma::m_c_nWhole;
const int TCalibGamma::m_c_nInner;
const int TCalibGamma::m_c_nOuter ;

// const error code
const double TCalibGamma::m_c_dError;
const int TCalibGamma::m_c_nError;
const threeVec TCalibGamma::m_c_v3Error(m_c_dError, m_c_dError, m_c_dError);
const int TCalibGamma::m_c_nOk;

// misc const
const double TCalibGamma::m_c_dPi;
const double TCalibGamma::m_c_dSpeedOfLight;

//////////////////////////////////////////////////////////
//              Initialisation methods 
//////////////////////////////////////////////////////////

/*******************************************************
********************************************************
Name:TCalibGamma::TCalibGamma
Date: 05/31/2000
Author: Matthieu Guillo

Description: default constructor. 
    
Input:
 . pointer on the BOS bcs array (variable pBcs).
Output:
 . none
*******************************************************
*******************************************************/
TCalibGamma::TCalibGamma()
{
  m_bGoodPhoton    = false;               
  m_bFoundAllBanks = false;
  m_bFoundBothEchb = false;
  m_bFoundBothEcpc = false;
  m_pPart          = NULL;
  m_pTbid          = NULL;
  m_pMvrt          = NULL;
  m_pEcpcInner     = NULL;
  m_pEcpcOuter     = NULL; 
  m_pEchbWhole     = NULL;
  m_pEchbInner     = NULL; 
  m_pEchbOuter     = NULL;
  m_nRunNumber     = m_c_nError;
  m_nEventNumber   = m_c_nError;
  m_nPartIndex     = m_c_nError;
  m_nTbidIndex     = m_c_nError;
  m_nEchbIndex     = m_c_nError;
  m_bcs = NULL;

}


/*******************************************************
********************************************************
Name: TCalibGamma::Set
Date: 04/09/2000
Author: Matthieu Guillo and Dennis Weygand

Description: This function main goal is to test whether the "photon" can be used for calibration or not. It also initialises some private members variables.  

A good photon is defined by the following criteria:
1) The photon hasn't been flaged before as not useful (variable flags in the PART bank is not set to c_nEroor using the Reject() method).
2) The hit is in the fiducial region: IsFiducial() methods checks that.
3) The calorimeter hit should be the only one in a given region. This prevents overlapping signals to give erroneous information on ADC and TDC. Method HitsInSector() counts the number of hits in the photon sector.
4) All useful banks should be present: PART, ECHB, TBID && MVRT. Variable m_bFoundAllBanks checks that.
5) The gamma energy should be above a treshold defined by user.
6) If a layer is hit (inner or outer), both corresponding echb and ECPC banks should be found.
   A hit in the inner layer is required (we are dealing with photons, this is the expected behavior) but not in the outer layer. Variable bShower checks that.

Input:
  . pointer on the BOS bcs array (variable pBcs).
  . index of the gamma in the PART bank (variable nGammaIndex).
  . minimum energy of the photon for rejecting background (variable dMinEnergy). Default is 0.1 GeV.
  . Sector number (variable nPartBank): default is 1, for real data (Monte Carlo data could be 0). See getGroup function.
Output:
  . none. Function just initializes private members variables. Variable m_bGoodPhoton is set up.
*******************************************************
*******************************************************/
void TCalibGamma::Set(BOSbank* pBcs, int nGammaIndex, double dMinEnergy /* = 0.1 GeV */, int nPartBank /* = 1 */)
{
  // initialization
  m_bGoodPhoton    = false;
  m_bFoundAllBanks = false;
  m_bFoundBothEchb = false;
  m_bFoundBothEcpc = false;
  m_pPart          = NULL;
  m_pTbid          = NULL;
  m_pMvrt          = NULL;
  m_pEcpcInner     = NULL;
  m_pEcpcOuter     = NULL; 
  m_pEchbWhole     = NULL;
  m_pEchbInner     = NULL; 
  m_pEchbOuter     = NULL;
  m_nRunNumber     = m_c_nError;
  m_nEventNumber   = m_c_nError;
  m_nPartIndex     = m_c_nError;
  m_nTbidIndex     = m_c_nError;
  m_nEchbIndex     = m_c_nError;
  m_bcs = pBcs; 

  // Try to find some BOS banks
  clasPART_t *PART = (clasPART_t *) getGroup(m_bcs,"PART", nPartBank);
  clasECHB_t *ECHB = (clasECHB_t *) getBank(m_bcs,"ECHB");
  clasTBID_t *TBID = (clasTBID_t *) getBank(m_bcs,"TBID");
  clasMVRT_t *MVRT = (clasMVRT_t *) getBank(m_bcs,"MVRT");
  clasHEAD_t *HEAD = (clasHEAD_t *) getBank(m_bcs,"HEAD");

  // for debugging pupose (used by method print)
  m_nPartIndex = nGammaIndex;
  if (HEAD) {
    m_nRunNumber = HEAD->head[0].nrun;
    m_nEventNumber = HEAD->head[0].nevent;
  }
  else {
    m_nRunNumber = m_nEventNumber = 0;
  }



  // Try to see if the event has all necessary banks
  m_bFoundAllBanks = PART && ECHB && TBID && MVRT;
  
  bool bRejectGamma = false; // check if this photon has been rejected before (see reject() method) 
  
  if (m_bFoundAllBanks){
    // now we are dealing with the photon
    m_pPart = &PART->part[nGammaIndex]; // gets pointer on desired part bank
    bRejectGamma = (m_pPart->flags == m_c_nError);  // checks if photon has been flaged for rejection.
    if (!bRejectGamma) {
      m_pMvrt = &MVRT->mvrt[0];            // gets pointer on  mvrt bank;
      m_nTbidIndex = m_pPart->trkid - 1;   // save value for debugging
      m_pTbid = &TBID->tbid[m_nTbidIndex]; // gets pointer on photon tbid bank

      
      if (m_pTbid->ec_id){ 
	// photon has hit in forward calorimeter
	m_nEchbIndex = m_pTbid->ec_id - 1;  // save value for debugging
	m_pEchbWhole = &ECHB->echb[m_nEchbIndex];
	
	m_InitEchb(); // tries to find "inner echb" and "outer echb" banks.
	m_InitEcpc(); // tries to find "inner ecpc" and "outer ecpc" banks. 
      }
    }
  }
  
  bool bShower =  (m_pEchbInner && m_pEcpcInner) && (static_cast<bool>(m_pEchbOuter) ? static_cast<bool>(m_pEcpcOuter) : true);  // needs a "complete hit information" in inner layer, and if outer layer is hit, needs complete info in it also. Complete means both echb and ecpc banks.

  m_bGoodPhoton = (!bRejectGamma) && (this->IsFiducial()) && (this->HitsInSameSector() == 1) && (this->Energy() > dMinEnergy) && m_bFoundAllBanks && bShower;

}


//////////////////////////////////////////////////////////
//          ECPC bank access data methods 
//////////////////////////////////////////////////////////

/*******************************************************
********************************************************
Name: TCalibGamma::Adc
Date: 05/31/2000
Author: Matthieu Guillo

Description: returns the ADC value recorded by the strip in a given layer (inner or outer). 
  
Input:    
  . layer (inner, outer), variable nLayer.
  . ecpc index, variable nEcpcIndex.
Output:
  . ADC value (or error code if something was wrong). 
*******************************************************
*******************************************************/
double TCalibGamma::Adc(int nLayer, int nEcpcIndex) const
{
  clasECPC_t *ECPC = NULL;

  switch (nLayer){
  case m_c_nInner:
    // inner stack
    ECPC = m_pEcpcInner;
    break;
  case m_c_nOuter:
    // outer stack
    ECPC = m_pEcpcOuter;
    break;
  default:
    break; 
  }
  
  return(ECPC ? ECPC->ecpc[nEcpcIndex].adc : m_c_dError);

}

/*******************************************************
********************************************************
Name: TCalibGamma::Tdc
Date: 05/31/2000
Author: Matthieu Guillo
Description: this method returns the TDC value recorded by the strip indexed by a given index in a given layer (inner or outer).
  
Input:    
  . layer (inner, outer), variable nLayer.
  . ecpc index, variable nEcpcIndex.
Output:
  . TDC value (or error code if something was wrong). 
*******************************************************
*******************************************************/
double TCalibGamma::Tdc(int nLayer, int nEcpcIndex) const
{
  clasECPC_t *ECPC = NULL;

  switch (nLayer){
  case m_c_nInner:
    // inner stack
    ECPC = m_pEcpcInner;
    break;
  case m_c_nOuter:
    // outer stack
    ECPC = m_pEcpcOuter;
    break;
  default:
    break;
  }
  
  return(ECPC ? ECPC->ecpc[nEcpcIndex].tdc : m_c_dError);
}


/*******************************************************
********************************************************
Name: TCalibGamma::AdcMax
Date: 05/31/2000
Author: Matthieu Guillo
Description: this method returns the maximum ADC value recorded by a strip in a given layer and view.
   
Input:    
  . layer (in, out), variable nLayer.
  . view (U, V, W), variable nView. 
Output:
  . maximum ADC value (or error code if something was wrong). 
*******************************************************
*******************************************************/
double TCalibGamma::AdcMax(int nLayer, int nView) const
{
  int nEcpcIndex = this->m_EcpcIndexMaxAdc(nLayer, nView); // get ecpc index of the tube with maximum adc 
  
  return ((nEcpcIndex == m_c_nError) ? m_c_dError : this->Adc(nLayer, nEcpcIndex));

}


/*******************************************************
********************************************************
Name: TCalibGamma::TdcMax
Date: 05/31/2000
Author: Matthieu Guillo
Description: this method returns the TDC value read by the strip which had recorded the maximum ADC value in a given layer and view. The name can be confusing: it does NOT return the maximum TDC value recorded.

Input:    
  . layer (in, out), variable nLayer.
  . view (U, V, W), variable nView. 
Output:
  . TDC value (or error code if something was wrong). 
*******************************************************
*******************************************************/
double TCalibGamma::TdcMax(int nLayer, int nView) const
{
  int nEcpcIndex = this->m_EcpcIndexMaxAdc(nLayer, nView); // gets ecpc index of the tube with maximum adc 
  
  if (nEcpcIndex == m_c_nError){
    return (m_c_dError); // exit with error code
  }

  double dTdc = this->Tdc(nLayer, nEcpcIndex);
  
  return (dTdc ? dTdc : m_c_dError);
 
}  

/*******************************************************
********************************************************
Name: TCalibGamma::TubeIdMaxAdc
Date: 05/31/2000
Author: Matthieu Guillo
Description: this method returns the tube id which has recorded the maximum ADC in a given layer (in, out) and view (U, V, W).  
  
Input:    
  . layer (in, out), variable nLayer.
  . view (U, V, W), variable nView.
Output:
  . tube id (or error code if something was wrong). 
*******************************************************
*******************************************************/
int TCalibGamma::TubeIdMaxAdc(int nLayer, int nView) const
{
  int nTubeId = m_c_nError; // initialize to a bad value
  clasECPC_t *ECPC = NULL;

  switch (nLayer){
  case m_c_nInner:
    // inner stack
    ECPC = m_pEcpcInner;
    break;
  case m_c_nOuter:
    // outer stack
    ECPC = m_pEcpcOuter;
    break;
  default:
    break; // do nothing, ecpcPtr will keep the NULL value.
  } 

  if (ECPC) {
    int nIndex = this->m_EcpcIndexMaxAdc(nLayer, nView);
    if (nIndex != m_c_nError){
      nTubeId = ECPC->ecpc[nIndex].id % 100;
    }
  }

  return (nTubeId);
}

/*******************************************************
********************************************************
Name: TCalibGamma::TubeIndexMaxAdc
Date: 05/31/2000
Author: Matthieu Guillo
Description: this method returns the index (for a 1-D array) of the tube which has recorded the maximum ADC in a given layer (in, out) and view (U, V, W).

Input:    
  . layer (in, out), variable nLayer.
  . view (U, V, W), variable nView.
Output:
  . tube index (or error code if something was wrong). 
*******************************************************
*******************************************************/
int TCalibGamma::TubeIndexMaxAdc(int nLayer, int nView) const
{  
  int nTubeIndex = m_c_nError; // initialize to a bad value
  clasECPC_t *ECPC = NULL;

  switch (nLayer){
  case m_c_nInner:
    // inner stack
    ECPC = m_pEcpcInner;
    break;
  case m_c_nOuter:
    // outer stack
    ECPC = m_pEcpcOuter;
    break;
  default:
    break; // do nothing, ecpcPtr will keep the NULL value.
  } 

  int nTubeId = this->TubeIdMaxAdc(nLayer, nView);
  int nSector = this->Sector();
  if (ECPC && (nTubeId != m_c_nError) && (nSector != m_c_nError)) {
    nTubeIndex = this->TubeIndex(nSector, nLayer, nView, nTubeId);
  }
  
  return (nTubeIndex);
}



////////////////////////////////
//  Useful distances for timing
////////////////////////////////

/*******************************************************
********************************************************
Name: TCalibGamma::DistVertex2Hit
Date: 02/31/2000
Author: Matthieu Guillo.

Description: returns the distance (in cm) from the vertex interaction to the centroid position.

Input:
  . none, uses private members
Output:
  . distance in cm.
*******************************************************
*******************************************************/
double TCalibGamma::DistVertex2Hit() const
{
  double dDistance = m_c_dError;
 
  threeVec v3HitPosition = this->CentroidPosition();
  threeVec v3Vertex =  this->Vertex();
 
  if ((v3HitPosition != m_c_v3Error) && (v3Vertex != m_c_v3Error)){
    dDistance = ~ (v3HitPosition - v3Vertex);  // magnitude of the 3 vector
  }
 
  if(isnan(dDistance)){ // M. Wood 23July2008 - quick fix to avoid NaN in MVRT
    dDistance = m_c_dError;
  }

  return (dDistance);
}


/*******************************************************
********************************************************
Name: TCalibGamma::DistHit2View
Date: 02/31/2000
Author: Matthieu Guillo shamelessly ripped and adapted from Sasha Philips and al.,

Description: returns the distance (in cm) from the hit point on a given layer of the calorimeter to the edge of a given view.

Input:
  . layer type (inner, outer), variable nLayer
  . view type (U, V, W), variable nView.
Output:
  . distance to the view edge in cm (function return value)
*******************************************************
*******************************************************/
double TCalibGamma::DistHit2View(int nLayer, int nView) const
{
 
  static const double a[2][3] = {
    {94.86, 193.56, 94.86},
    {96.29, 202.10, 96.29}
  };
  
  static const double b[3] = { -1.00, 0.00, 1.00};
  static const double c[3] = { 0.512, -1.00, 0.512};
  static const double d[3] = { 1.00,  1.123, 1.097};
  
  double dDistance = m_c_dError;

  if (m_pEchbInner){
    // Yes! This routine requires the use of the innner bank, not whole (hit is on the surface of inner calorimeter).
    double dXi = m_pEchbInner->j_hit;
    double dYi = m_pEchbInner->i_hit;
    dDistance =  (a[nLayer - 1][nView - 1] + b[nView -1] * dXi + c[nView -1] * dYi) * d[nView -1];
  } 

  return (dDistance);
}


//////////////////////////////////////////////////////////
//            Utilities member functions 
//////////////////////////////////////////////////////////

/*******************************************************
********************************************************
Name: TCalibGamma::Clas2Local
Date: 04/25/2000
Author: Sasha Philips and al., shamelessly adapted by Matthieu Guillo
Description: For a given set of CLAS system coordinates (x, y, z), returns the corresponding local EC coordinates on the surface (xi, yi, and zi).

Input:
  . CLAS coordinates x, y and z (variables dX, dY and dZ).
Output:
  . three vector (xi, yi, zi) local coordinates (function returned value).
*******************************************************
*******************************************************/
threeVec TCalibGamma::Clas2Local(double dX, double dY, double dZ)
{
  double dECPhi, dPhi, adRotation[3][3];

  if (dY < 0.) {
    dPhi = 2. * m_c_dPi - acos(dX / sqrt(dX * dX + dY * dY));
  } 
  else {
    dPhi = acos(dX / sqrt( dX * dX + dY * dY));
  }

  if (dPhi < 0.) {
    dPhi += 2. * m_c_dPi;
  } 
  dPhi += m_c_dPi / 6.;  
  if (dPhi >= (2. * m_c_dPi)) {
    dPhi -= 2. * m_c_dPi;  
  }

  dECPhi = (int) (dPhi / (m_c_dPi / 3.)) * m_c_dPi / 3.;

  adRotation[0][0] = cos(m_c_dECTheta) * cos(dECPhi);  
  adRotation[0][1] = -sin(dECPhi);  
  adRotation[0][2] = sin(m_c_dECTheta) * cos(dECPhi);  
  adRotation[1][0] = cos(m_c_dECTheta) * sin(dECPhi);  
  adRotation[1][1] = cos(dECPhi);
  adRotation[1][2] = sin(m_c_dECTheta) * sin(dECPhi);  
  adRotation[2][0] = -sin(m_c_dECTheta);  
  adRotation[2][1] = 0.;  
  adRotation[2][2] = cos(m_c_dECTheta);

  // rotate axis system
  double dXi, dYi, dZi;

  dYi = dX * adRotation[0][0] + dY * adRotation[1][0] + dZ * adRotation[2][0];  
  dXi = dX * adRotation[0][1] + dY * adRotation[1][1] + dZ * adRotation[2][1];  
  dZi = dX * adRotation[0][2] + dY * adRotation[1][2] + dZ * adRotation[2][2];

  // translate on the EC surface
  dZi -= m_c_dDistanceOrigine2EcInner;

  threeVec v3Coord(dXi, dYi, dZi);
  
  return (v3Coord);
}


threeVec TCalibGamma::Clas2Local(const threeVec& v3ClasCoord)
{
  return (TCalibGamma::Clas2Local(v3ClasCoord.x(), v3ClasCoord.y(), v3ClasCoord.z()));
}


/*******************************************************
********************************************************
Name: TCalibGamma::VertexTime
Date: 05/31/2000
Author: Matthieu Guillo

Description: returns the time at the vertex as computed in the TBID bank.
     
Input:    
  . none
Output:
  . vertex time (or error code if something was wrong). 
*******************************************************
*******************************************************/
double TCalibGamma::VertexTime() const
{
  double dVertexTime = -1.0; // impossible value

  if (m_pTbid) {
    dVertexTime = m_pTbid->vtime;
  }
  return (dVertexTime);  
  //return (dVertexTime > 0 ? dVertexTime : m_c_dError);
}


/*******************************************************
********************************************************
Name: TCalibGamma::HitsInSameSector
Date: 05/15/2000
Author: Matthieu Guillo

Description: returns the number of EC hits in the same sector than the photon. Useful to chech there is only one hit to ensures that there is no signals overlapping.

Input:
  . none (uses private members)
Output:
  . number of hits in the calorimeter or error code if something wrong.
*******************************************************
*******************************************************/
int TCalibGamma::HitsInSameSector() const
{
  int nHits = 0;
  clasECHB_t *ECHB = (clasECHB_t *) getBank(m_bcs,"ECHB");

  if (m_pEchbWhole) {
    int nGammaSector = m_pEchbWhole->sect / 100; // gets photon's sector
    for (int i = 0; i < ECHB->bank.nrow; i++) {
      echb_t* echb = &ECHB->echb[i];
      if ( ((echb->sect % 100) == m_c_nWholeEchb) && ((echb->sect / 100) == nGammaSector) ){
	// use "Whole" banks only and check if hit is in same sector than photon
	nHits++;
      }
    }
  }

  // return error code if hits == 0
  return (nHits ? nHits : m_c_nError);
}


/*******************************************************
********************************************************
Name: TCalibGamma::IsHitInLayer
Date: 11/28/2000
Author: Matthieu Guillo

Description: returns true if there is a "complete hit information" for a given layer
 
Input:
 . nLayer: layer number.
Output:
 . true or false
*******************************************************
*******************************************************/
bool TCalibGamma::IsHitInLayer(int nLayer) const
{
  bool bHit = false;
  
  switch (nLayer){
  case m_c_nInner:
    bHit = m_pEchbInner && m_pEcpcInner;
    break;
  case m_c_nOuter:
    bHit = m_pEchbOuter && m_pEcpcOuter;
    break;
  default:
    break;
  }

  return(bHit);
}


/*******************************************************
********************************************************
Name: TCalibGamma::Reject
Date: 05/31/2000
Author: Matthieu Guillo

Description: flags the photon filling the "flags" variable with an error value.
     
Input:    
  . none
Output:
  . c_nOk or m_c_nError according if flag was successfully switched or not. 
*******************************************************
*******************************************************/
int TCalibGamma::Reject() const
{
  if (m_pPart){
    m_pPart->flags = m_c_nError;
    return(m_c_nOk);
  }
  else {
    return (m_c_nError);
  }
}


/*******************************************************
********************************************************
Name: TCalibGamma::Print
Date: 04/05/2000
Author: Matthieu Guillo and Dennis Weygand
Description: prints on the screen different informations relative to the photon for debugging purposes.
  
Input:
  . none. Just uses the private members variables
Output:
  . Many data on the STDERR
*******************************************************
*******************************************************/
void TCalibGamma::Print() const
{
  
  cerr << "\n\nTCalibGamma:\t Status:\t" << (int) m_bGoodPhoton << "\tSector:\t" << this->Sector() << "\tEnergy:\t" << this->Energy() << endl;
  cerr << "\tRun #:\t" << m_nRunNumber << "\tEvent#:\t" << m_nEventNumber << endl;  
  cerr << "\tIndexes:\tPart:\t" << m_nPartIndex << "\tTbid:\t"<< m_nTbidIndex<< "\tECHB:\t" << m_nEchbIndex << endl;

  cerr << "\tEC centroid position:\t";
  this->CentroidPosition().print();
    
  // Print vertex info
  cerr << "\tVertex position:\t";
  this->Vertex().print();
  cerr << "\tVertex time:\t" << this->VertexTime() << endl;
  
  // Print distance vertex->calo
  cerr << "\tDistance vertex to hit:\t" << this->DistVertex2Hit() << endl;
  
  for (int view = 1; view <= 3; ++view) {
    cerr << "\tView:\t" << view << endl;
    cerr << "\t\tHit to inner edge:\t" << this->DistHit2View(m_c_nInner, view) << "\tHit to outer edge:\t" << this->DistHit2View(m_c_nOuter, view) << endl;
    cerr << "\t\tadc(inner):\t" << this->AdcMax(m_c_nInner, view) << "\t\tadc(outer)\t" << this->AdcMax(m_c_nOuter, view)  << endl;
    cerr << "\t\ttdc(inner):\t" << this->TdcMax(m_c_nInner, view) << "\ttdc(outer):\t" << this->TdcMax(m_c_nOuter, view) << endl;
    cerr << "\t\ttube Index(inner):\t" << this->TubeIndexMaxAdc(m_c_nInner, view) << "\ttube Index(outer):\t" << this->TubeIndexMaxAdc(m_c_nOuter, view) << endl;
    cerr << "\t\ttube id (inner):\t" << this->TubeIdMaxAdc(m_c_nInner, view) << "\ttube strip(outer):\t" << this->TubeIdMaxAdc(m_c_nOuter, view) << endl;
  }

  cerr << endl;
}



//////////////////////////////////////////////////////////
//                    Private methods
//////////////////////////////////////////////////////////

/*******************************************************
********************************************************
Name: TCalibGamma::m_InitEchb()
Date: 05/31/2000
Author: Matthieu Guillo and Dennis Weygand.

Description: From a photon in the part bank, we can point to the corresponding TBID bank and from there to the corresponding ECHB bank. However the latest bank is an "average" bank, called "whole echb" (and we point to it through the m_pEchbWhole member variable), comming from the reading of the inner layer and outer layer. Every echb bank has a variable called "sect" that contains 2 informations: the sector number (let's call it "sect#") and its layer type (inner, outer or whole, let's call it "layer"). Then the encoding is sect = sect# * 100 + layer. We can then easily gets from "sect" the sector and layer type informations.
This routines tries to find, for a "whole echb" bank, the corresponding "inner echb" and "outer echb". But it may happen that one of the layer is missing (because hit was not fiducial for example or energy released was not enough etc...) so we do not necessary find both corresponding inner and outer echb banks.
A "whole echb" bank has a variable called matchid1 corresponding to the "inner bank", and one other variable called matchid2 corresponding to the "outer bank"(a zero value means not correspondance). Both "inner echb" and "outer echb" banks have a variable called matchid1 corresponding to the "whole echb" bank. So we need 2 things:
. match the sector numbers
. match the matchid1 number from a "whole echb" with the matchid1 number from a "inner echb" and the matchid2 number from a "whole echb" with the matchid1 number from a "outer echb".
    
Input:
  . none, read the ECHB banks involved in the event and try to match with the m_pEchbWhole pointer.
Output:
  . true or false according if found inner and outer ECHB banks. Also this function fills the member variables m_pEchbInner and m_pEchbOuter (corresponding to inner and outer ECHB banks respectively) when match is ok.
*******************************************************
*******************************************************/
bool TCalibGamma::m_InitEchb()
{
  clasECHB_t *ECHB = (clasECHB_t *) getBank(m_bcs,"ECHB");

  m_pEchbInner = m_pEchbOuter = NULL;  

  if (ECHB && m_pEchbWhole) {
    int nGammaSector = m_pEchbWhole->sect / 100;  // sector where the photon is in

    // try to find inner echb bank
    int nMatchInner = m_pEchbWhole->matchid1;  // gets match inner index
    if (nMatchInner){
      // case there exists a corresponding inner echb bank.
      for (int i = 0; (i < ECHB->bank.nrow) && !m_pEchbInner; i++) { 
	echb_t *echb = &ECHB->echb[i];	// gets next echb bank
	if ((echb->sect / 100) == nGammaSector) { // same sector than photon?
	  if ( (echb->sect % 100) == m_c_nInnerEchb ) { // is it an inner echb bank?
	    if (echb->matchid1 == nMatchInner) { // same id? if yes, match found
	      m_pEchbInner = echb; // inner points to the correct bank
	    }
	  }
	}
      }
    }

    // try to find outer echb bank
    int nMatchOuter = m_pEchbWhole->matchid2;  // gets match outer index
    if (nMatchOuter){
      // case there exists a corresponding outer echb banks
      for (int i = 0; (i < ECHB->bank.nrow) && !m_pEchbOuter; i++){ 
	echb_t *echb = &ECHB->echb[i];	// gets next echb bank
	if ((echb->sect / 100) == nGammaSector) { // same sector than photon?
	  if ( (echb->sect % 100) == m_c_nOuterEchb ) { // is it an outer echb bank?
	    if (echb->matchid1 == nMatchOuter) { // same id? if yes, match found
	      m_pEchbOuter = echb; // outer points to the correct bank
	    }
	  }
	}
      }
    }
    
  }
   
  m_bFoundBothEchb  = m_pEchbInner && m_pEchbOuter; 

  return (m_bFoundBothEchb);
}


/*******************************************************
********************************************************
Name: TCalibGamma::m_InitEcpc()
Date: 05/31/2000
Author: Matthieu Guillo and Dennis Weygand

Description: We have started with the ECHB bank to fill the TCalibGamma object. However, ADCs and TDCs values are found only in the ECPC bank (EC calibration bank). For each of the "inner echb" and "outer echb" bank, we want to point to the corresponding ECPC bank.  
The ECPC bank number contains info about the sector, layer type (inner or outer) and the hit number within the sector and layer. The information is encoded as following: bank number = sector * 100 + layer_type * 10 + hit index. We can also extract those informations from the echb bank (variable "sect" for sector and layer type, variable "istat" for hit index  in the echb_t structure).
This function finds the match between echb banks and ECPC banks and once done points to the corresponding ECPC bank.
  
Input:
  . none, we loop over all ECPC bank in the event to find the match.
Output:
  . true or false depending if both banks are found or not. If the match is found, the m_pEcpcInner or m_pEcpcOuter member variable points to the corresponding ECPC bank.
*******************************************************
*******************************************************/
bool TCalibGamma::m_InitEcpc()
{
  if (!m_pEchbWhole) {
    // needs a "whole" hit
    return (false);
  }
  
  clasECPC_t *ECPC = (clasECPC_t *) getBank(m_bcs, "ECPC"); // gets 1st ECPC bank in the event
  m_pEcpcInner = m_pEcpcOuter = NULL;  // initialization (point to null)
  m_bFoundBothEcpc = false;
  
  int nGammaSector   = m_pEchbWhole->sect / 100;  
  int nGammaHitIndex = m_pEchbWhole->istat % 100;
  
  // try to find both ecpc bank
  while (ECPC && (!m_bFoundBothEcpc)) {
    // loop over ECPC banks as long as we have some AND we haven't found both ecpc banks (inner and outer)
    if (ECPC->bank.nrow) {                // check if ECPC bank is not empty!

      int nSector = ECPC->bank.nr / 100;  // gets ECPC sector
      if (nSector == nGammaSector) {       // same sector than photon?
	int nHitIndex = ECPC->bank.nr % 10;      // gets ECPC hit number in sector and layer
	if (nHitIndex == nGammaHitIndex) {        // same hit number?
	  int nLayerType = ECPC->bank.nr % 100 - nHitIndex; // gets ECPC inner or outer hit type
	  if (nLayerType == m_c_nInnerEcpc) {
	    m_pEcpcInner = ECPC;        // m_pEcpcInner points to the correct ECPC bank 
	  }
	  else if (nLayerType == m_c_nOuterEcpc){
	    m_pEcpcOuter = ECPC;        // m_pEcpcOuter points to the correct ECPC bank
	  }
	}
      }
    }
    
    ECPC = (clasECPC_t *) getNextBank(m_bcs, (bankHeader_t *) ECPC); // get next ECPC bank in the event
  }

  m_bFoundBothEcpc = m_pEcpcInner && m_pEcpcOuter;

  return(m_bFoundBothEcpc);  // returns result of matching
}
    

/*******************************************************
********************************************************
Name: TCalibGamma::m_EcpcIndexMaxAdc
Date: 05/31/2000
Author: Matthieu Guillo

Description: For a given layer (in, out) and a given view (U, V or W), this functions looks for the strip that has recorded the maximum ADC value and returns the strip index in the ECPC bank. If the maximum is 0 or a ecpc bank has not be initialized, returns error code.
  
Input:
  . layer (in, out), variable nLayer.
  . view (U, V, W), variable nView. 
Output:
  . index of the strip in the ECPC bank (or error code if something was wrong).
*******************************************************
*******************************************************/
int TCalibGamma::m_EcpcIndexMaxAdc(int nLayer, int nView) const
{
  clasECPC_t *ECPC = NULL;
  double dMaxAdc = 0.0;      // initialize max ADC recorded
  int    nMaxAdcIndex = 0;   // initialize index of tube with maximum ADC

  switch (nLayer){
  case m_c_nInner:
    // inner stack
    ECPC = m_pEcpcInner;
    break;
  case m_c_nOuter:
    // outer stack
    ECPC = m_pEcpcOuter;
    break;
  default:
    break; // do nothing, pEcpc will keep the NULL value.
  }
  
  if (ECPC) {
    for (int nEcpcIndex = 0; nEcpcIndex < ECPC->bank.nrow ; nEcpcIndex++) {  // loop over all strips
      int nViewType = ECPC->ecpc[nEcpcIndex].id / 100; 
      if (nViewType == nView) {                      // same view than we want the max ADC?
	double dAdc = this->Adc(nLayer, nEcpcIndex);  // gets ADC recorded by strip index nEcpcIndex
	if (dAdc > dMaxAdc) {                 // compare ADC with current max
	  nMaxAdcIndex = nEcpcIndex;          // max ADC index is now current index
	  dMaxAdc = dAdc;                     // max ADC is now current ADC
	}
      }
    }
  }
  
  // if max is different of 0, return corresponding index, otherwise error code
  return (dMaxAdc ? nMaxAdcIndex : m_c_nError);
}










