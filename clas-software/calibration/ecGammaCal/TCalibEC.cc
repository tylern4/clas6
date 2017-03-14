#include <iostream>
#include "TCalibEC.h"
#include "particleType.h"

//ClassImp (TCalibEC);

//////////////////////////////////////////////////////////
//              Global variables 
//////////////////////////////////////////////////////////

// hardware constants
const int TCalibEC::m_c_nSectors;
const int TCalibEC::m_c_nViews;
const int TCalibEC::m_c_nLayers;
const int TCalibEC::m_c_nTubes;
const int TCalibEC::m_c_nTotalTubes;
const double TCalibEC::m_c_dEffectiveSpeedInPlastic;

// const error code
const double TCalibEC::m_c_dError; 
const int TCalibEC::m_c_nError;
const int TCalibEC::m_c_nOk;

// const for fitting
const int TCalibEC::m_c_nCalibConsts;



/*******************************************************
 *******************************************************
Name: TCalibEC::TCalibEC
Date: 05/24/2001
Author: Matthieu Guillo

Description:
Default constructor. Set up the value of the minimum ADC for calibration.

Inputs: 
  . minimum ADC value for calibration (variable dMinAdcValue).
Output:
  . none
 *******************************************************
*******************************************************/
TCalibEC::TCalibEC(double dMinAdcValue /* 100. */)
  : m_c_dMinAdcValue(dMinAdcValue)
{
  ;  
}


/*******************************************************
 *******************************************************
Name: TCalibEC::FindGammas
Date: 04/01/2001
Author: Matthieu Guillo

Description:
Finds gamma that can be used for calibration. Checks first that the event has all necessary particles (call "IsGoodEvent()" method).

Inputs: 
  . BOS array for the event (variable pBcs).
  . photon minimum energy (in GeV) for use for calibration (variable dMinGammaEnergy). 
  . group number of PART bank (variable nGroupIndex).
Output:
  . number of photons in the event that have been used for calibration. 
 *******************************************************
*******************************************************/
int TCalibEC::FindGammas(BOSbank* pBcs, double dMinGammaEnergy /* = 0.1 */, int nGroupIndex /* = 1 */)
{
  int nCalibGammas = 0; // number of gammas used in the event
  m_bcs = pBcs;
  clasPART_t *PART = (clasPART_t*) getGroup(m_bcs, "PART", nGroupIndex);

  if (this->IsGoodEvent(PART)) {
    
    for (int nPartIndex = 0; nPartIndex < PART->bank.nrow; nPartIndex++) {
      // process every particle to find photons

      if (PART->part[nPartIndex].pid == Gamma) {
	TCalibGamma gam(m_bcs, nPartIndex, dMinGammaEnergy, nGroupIndex);  // builds fitGamma object
	if (gam.IsGoodPhoton()) {
	  nCalibGammas++;
	  m_CurrentGamma = &gam; // save adress of the current gamma
	  
	  double dVertexTime = gam.VertexTime(); // gets time at vertex
	  double dVertex2Centroid = gam.DistVertex2Hit(); // gets distance from vertex to centroid.

	  for (int nLayer = 1; nLayer <= m_c_nLayers; nLayer++) {
	    // process inner and outer layers
	    if (gam.IsHitInLayer(nLayer)) { // hit in layer?
	      for (int nView = 1; nView <= m_c_nViews; nView++) {
		// process U, V and W views
		int nTubeIndex = gam.TubeIndexMaxAdc(nLayer, nView);
		if (m_adCalibConsts[nTubeIndex][0] != m_c_dError) {
		  double dTdc = gam.TdcMax(nLayer, nView); // gets TDC of tube with maximum ADC signal
		  double dAdc = gam.AdcMax(nLayer, nView); // gets ADC of tube with maximum ADC signal
		  double dEcSurface2Edge  = gam.DistHit2View(nLayer, nView); // gets distance from hit position on calorimeter inner surface to the view edge (the edge can be on the inner or outer layer).
		  if ((dVertexTime != m_c_dError) && (dVertex2Centroid != m_c_dError) && (dTdc != m_c_dError) && (dAdc != m_c_dError) && (dAdc > m_c_dMinAdcValue) && (dEcSurface2Edge != m_c_dError)){
		    // hit is good and can be used
		    this->UseGamma(dAdc, dTdc, dVertex2Centroid, dEcSurface2Edge, dVertexTime, nLayer, nView);  // overridden in derived classes.
		  }  // end of if ((dVertexTime != ...
		}  // end of if (m_adCalibConsts[nTubeIndex]...
	      }  // end of for (int nView = 1...
	    }  // end of if (gam.IsHitInLayer...
	  }  // end of for (int nLayer = 1 ...

	}  // end of if (gam.IsGoodPhoton...
      }  // if (PART->part...

    }  //  for (int nPartIndex...  
  }  // if (this->IsGoodEvent

  return(nCalibGammas);

}


/*******************************************************
 *******************************************************
Name: TCalibEC::IsGoodEvent
Date: 04/01/2001
Author: Matthieu Guillo

Description:
Tests if the event can be used or not for calibration. A good event for calibration should contains one proton, at least one gamma and at least 2 charged tracks (for better vertex reconstruction). There is also a test on the vertex time for pathologic events.

Inputs: 
  . pointer on the PART bank.
Output:
  . true of false according if the event can be used for calibration. 
 *******************************************************
*******************************************************/
bool TCalibEC::IsGoodEvent(const clasPART_t* PART) const
{

  bool bGoodEvent = false;
  int nProtons = 0, nGammas = 0, nChargedTracks = 0;
  double dVertexTime = 0.;
  
  if (PART) {
    clasTBID_t *TBID = (clasTBID_t*) getBank(m_bcs, "TBID");
    if (TBID) {
      dVertexTime = TBID->tbid[0].vtime;  // gets vertex time from first track
    }
    for (int i = 0; i < PART->bank.nrow; i++) {
      const part_t* part = &PART->part[i]; // get part associated to ith particle
      if (part->q) {
	nChargedTracks++; 
      }
      
      switch ((Particle_t) part->pid) {
      case Proton:
	nProtons++;
	break;
      case Gamma:
	nGammas++;
	break;
      default:
	break;
      }
    }
  }

  bGoodEvent = (nProtons == 1) && (nGammas) && (nChargedTracks > 1); 
  //bGoodEvent = (dVertexTime > 0.) && (nProtons == 1) && (nGammas) && (nChargedTracks > 1); 

  return(bGoodEvent);
}





