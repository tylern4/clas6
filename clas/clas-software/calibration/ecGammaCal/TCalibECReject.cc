
using namespace std;

#include <cstdlib>
#include <iostream>
#include "TCalibECReject.h"


/*******************************************************
********************************************************
Name: TCalibECReject::TCalibECReject
Date: 05/01/2001
Author: Matthieu Guillo

Description:
Constructor. Reads the calibration constants and fit constants.

Input:
  . name of the file containing calibration constants (variable szCalibConstIn)
  . name of the file for outputing fit constants (variable szFitConstIn)
  . minimum ADC value in a tube for calibration (variable dMinAdcValue)
  . number of sigmas from centroid position, see method UseGamma (variable nNumberSigmas)
Output:
  . none
*******************************************************
*******************************************************/
TCalibECReject::TCalibECReject(const char* szCalibConstIn /* = "calibconsts.dat "*/,
                   const char* szFitConstIn /* = "fitconsts.dat" */,
                   double dMinAdcValue /* = 100. */,
                   int nNumberSigmas /* = 2 */)
  : TCalibEC(dMinAdcValue),
    m_nNumberSigmas(nNumberSigmas)
{

  // ----- Try to open the calibration constants and fit constants files for reading -----
  ifstream fsCalibConstIn(szCalibConstIn);
  if (!fsCalibConstIn) {
    cerr << "Couldn't read calibration file " << szCalibConstIn << "!\n";
    exit(8);
  }

  ifstream fsFitConstIn(szFitConstIn);
  if (!fsFitConstIn) {
    cerr << "Couldn't read fit file " << szFitConstIn << "!\n";
    exit(8);
  }

  // ----- Reads calibration constants -----
  for (int nTubeIndex = 0; nTubeIndex < m_c_nTotalTubes; nTubeIndex++) {
    int nCounts;
    fsCalibConstIn >> nCounts;
    for (int nCalibConst = 0; nCalibConst < m_c_nCalibConsts; nCalibConst++) {
      double dCalibConst, dErrorCalibConst;
      fsCalibConstIn >> dCalibConst >> dErrorCalibConst;
      m_adCalibConsts[nTubeIndex][nCalibConst] = dCalibConst;
    }
  }
  fsCalibConstIn.close();

  // ----- Reads fit constants -----
  for (int nTubeIndex = 0; nTubeIndex < m_c_nTotalTubes; nTubeIndex++) {
    double dHeight, dCentroid, dSigma;
    fsFitConstIn >> dHeight >> dCentroid >> dSigma;
    m_adFitCentroid[nTubeIndex] = dCentroid;
    m_adFitSigma[nTubeIndex] = dSigma;
  }
  fsFitConstIn.close();

}


/*******************************************************
********************************************************
Name: TCalibECReject::UseGamma
Date: 05/01/2001
Author: Matthieu Guillo

Description:
This method does 2 things:
  . computes the EC time from the calibration as well as the expected time form the hit position
  . Flags the gamma not to be used in the next occurence of the calibration if the difference of time between the expected one and the fitted one is too big. The definition of "too big" is controlled by the private member variable m_nNumberSigmas.

Input:
  . values for computing the time from the calibration
Output:
  . none
*******************************************************
*******************************************************/
void TCalibECReject::UseGamma(double dAdc, double dTdc, double dVertex2Centroid, double dEcSurface2Edge, double dVertexTime, int nLayer, int nView)
{
  int nTubeId = m_CurrentGamma->TubeIdMaxAdc(nLayer, nView);
  int nSector = m_CurrentGamma->Sector();
  int nTubeIndex = TCalibGamma::TubeIndex(nSector, nLayer, nView, nTubeId);

  double dFitCentroid = m_adFitCentroid[nTubeIndex]; // centroid of the gaussian
  double dFitSigma = m_adFitSigma[nTubeIndex];  // sigma of the gaussian

  if ((dFitCentroid != m_c_dError) && (dFitSigma != m_c_dError)) {
    double dValue[m_c_nCalibConsts] = { 1.0, dTdc, 1. / sqrt(dAdc), dEcSurface2Edge * dEcSurface2Edge, dEcSurface2Edge * dEcSurface2Edge * dEcSurface2Edge };

    // ----- computes time from the fit -----
    double dTimeFit = 0.;
    for (int n = 0; n < m_c_nCalibConsts; n++) {
      dTimeFit += m_adCalibConsts[nTubeIndex][n] * dValue[n];
    }
    dTimeFit -= dVertexTime; // substract vertex time (reference);

    // ----- computes time expected -----
    double dTimeExpected = dVertex2Centroid / TCalibGamma::m_c_dSpeedOfLight +  dEcSurface2Edge / m_c_dEffectiveSpeedInPlastic;

    // ----- checks times difference -----
    double dTimeDiff = dTimeExpected - dTimeFit;
    double dTimeDiffMin = dFitCentroid - m_nNumberSigmas * dFitSigma;
    double dTimeDiffMax = dFitCentroid + m_nNumberSigmas * dFitSigma;

    if ((dTimeDiff < dTimeDiffMin) || (dTimeDiff > dTimeDiffMax)) {
      // photon time is too far to have a good fit, reject it!
       m_CurrentGamma->Reject();
    }
  }

}





