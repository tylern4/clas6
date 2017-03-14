using namespace std;

#include <cstdlib>
#include <iostream>
#include "TCalibECCalib.h"
#include "TCalibEC.h"

// ClassImp (TCalibECCalib);


/*******************************************************
********************************************************
Name: TCalibECCalib::TCalibECCalib
Date: 05/01/2001
Author: Matthieu Guillo

Description:
Initailizes some arrays for calibration.

Input:
  . minimum number of hits in a given tube for calibration (variable  nMinHitsForCalib).
  . Minimum ADC value in a tube for calibration (variable dMinAdcValue)
Output:
  . none
*******************************************************
*******************************************************/
TCalibECCalib::TCalibECCalib(int nMinHitsForCalib /* = 50 */,
                 double dMinAdcValue /* = 100. */)
  : TCalibEC(dMinAdcValue),
    m_c_nMinHitsForCalib(nMinHitsForCalib)
{

  for (int nTube = 0; nTube < m_c_nTotalTubes; nTube++) {
    m_mtxM.push_back(matrix<double>(m_c_nCalibConsts, m_c_nCalibConsts));  // declare M-Matrix
    m_mtxV.push_back(matrix<double>(m_c_nCalibConsts, 1));                 // declare V-Matrix
    m_mtxM[nTube].zero();  // initializes M-Matrix to 0
    m_mtxV[nTube].zero();  // initializes V-Matrix to 0
    m_anCounts[nTube] = 0; // initializes number of hits in tubes to 0
  }

  // initializes calibration constants and errors on them to 0
  for (int nTubeIndex = 0; nTubeIndex < m_c_nTotalTubes; nTubeIndex++) {
    for (int nCalibConst = 0; nCalibConst < m_c_nCalibConsts; nCalibConst++) {
      m_adCalibConsts[nTubeIndex][nCalibConst] = 0.;
      m_adErrorCalibConsts[nTubeIndex][nCalibConst] = 0.;
    }
  }

}


/*******************************************************
 *******************************************************
Name: TCalibECCalib::UseGamma
Date: 05/01/2001
Author: Matthieu Guillo

Description:
Fills the M and V matrices (for details on those matrices, see calibration manual).

Inputs:
  . values for filling M and V matrices.
Output:
  . none.
 *******************************************************
*******************************************************/
void TCalibECCalib::UseGamma(double dAdc, double dTdc, double dVertex2Centroid, double dEcSurface2Edge, double dVertexTime, int nLayer, int nView)
{

  int nTubeId = m_CurrentGamma->TubeIdMaxAdc(nLayer, nView);
  int nSector = m_CurrentGamma->Sector();
  int nTubeIndex = TCalibGamma::TubeIndex(nSector, nLayer, nView, nTubeId);

  m_anCounts[nTubeIndex]++; // one more hit in this tube

  double dAlpha = dVertexTime + dVertex2Centroid / TCalibGamma::m_c_dSpeedOfLight +  dEcSurface2Edge / TCalibEC::m_c_dEffectiveSpeedInPlastic;  // intermediate value
  double dValue[m_c_nCalibConsts] = { 1.0, dTdc, 1. / sqrt(dAdc), dEcSurface2Edge * dEcSurface2Edge, dEcSurface2Edge * dEcSurface2Edge * dEcSurface2Edge };

  for (int nRow = 0; nRow < m_c_nCalibConsts; nRow++) {
    m_mtxV[nTubeIndex].el(nRow, 0) = m_mtxV[nTubeIndex].el(nRow, 0) + dAlpha * dValue[nRow];
    for (int nCol = 0; nCol < m_c_nCalibConsts; nCol++) {
      m_mtxM[nTubeIndex].el(nRow, nCol) = m_mtxM[nTubeIndex].el(nRow, nCol) + dValue[nRow] * dValue[nCol];
    }
  }
}


/*******************************************************
 *******************************************************
Name: TCalibECCalib::ComputeCalibConstants
Date: 05/01/2001
Author: Matthieu Guillo

Description:
From the M and V matrices, computes the calibration constants and associated errors due to statistics. This is done for every tube, except if there was not enough hits in the tube or if the M-matix is dingular (determinant very close to 0). If calibration cannot be done for a given tube, all its calibration constants are set to an error value. The results are output into a file to be retrieve latter.

Inputs:
  . calibration constants output name (variable szCalibConstOut).
Output:
  . none.
 *******************************************************
*******************************************************/
void TCalibECCalib::ComputeCalibConstants(char* szCalibConstOut /* = "calibconsts.dat" */)
{
  //  cout<<"The offset is "<<a0_offset;

  ofstream fsCalibConstOut(szCalibConstOut);

  if (fsCalibConstOut.bad()) {
    // exit if we cannot save calibration constants!
    cerr << "Couldn't write into calibration constants file " << szCalibConstOut << "!\n";
    exit(8);
  }

  for (int nTubeIndex = 0; nTubeIndex < m_c_nTotalTubes; nTubeIndex++) {
    fsCalibConstOut << m_anCounts[nTubeIndex] << " ";
    double dDeterminant = m_mtxM[nTubeIndex].det();

    //    cout<<endl<<"Tube "<< nTubeIndex<<" "<<m_anCounts[nTubeIndex]<<endl;
    //    cout<<"V matrix"<<endl;
    //    m_mtxV[nTubeIndex].print();
    //    cout<<"M matrix"<<endl;
    //    m_mtxM[nTubeIndex].print();
    //    cout<<"M determinant: "<<m_mtxM[nTubeIndex].det()<<endl;

    if ((m_anCounts[nTubeIndex] >= m_c_nMinHitsForCalib) && (abs(dDeterminant) > 0.0001)) {
      // enough counts in the tube for fitting and non singular matrix
      matrix<double> mtxInvM(m_c_nCalibConsts, m_c_nCalibConsts);
      mtxInvM = m_mtxM[nTubeIndex].inv();  // inverse M matrix
      matrix<double> mtxA(m_c_nCalibConsts, 1);
      mtxA = mtxInvM * m_mtxV[nTubeIndex];  // computes fit parameters for this tube.
      //      cout<<"invM matrix"<<endl;
      //      mtxInvM.print();
      //      cout<<"A matrix"<<endl;
      //      mtxA.print();

      for (int nCalibConst = 0; nCalibConst < m_c_nCalibConsts; nCalibConst++) {
    // fills arrays of constants for calibration.
    double dCalibConst = mtxA.el(nCalibConst, 0);
    double dErrorCalibConst = sqrt(mtxInvM.el(nCalibConst, nCalibConst) * m_anCounts[nTubeIndex]); // sigma on fit parameter
    //  if(nCalibConst==0) dCalibConst += a0_offset;
    fsCalibConstOut << dCalibConst << " " << dErrorCalibConst << " ";
      }
    }
    else {
      // either not enough statistics, either singular matrix
      for (int nCalibConst = 0; nCalibConst < m_c_nCalibConsts; nCalibConst++) {
    fsCalibConstOut << m_c_dError << " " << m_c_dError << " ";
      }
    }
    fsCalibConstOut << endl; // go to next tube
  }
  fsCalibConstOut.close(); // close file when all constants are written.

}


/*******************************************************
 *******************************************************
Name: TCalibECCalib::PrintTubesCounts
Date: 05/01/2001
Author: Matthieu Guillo

Description:
Prints the number of hits in all tubes. Usefull to know which tubes do not have enough statistics for  calibration. The results are written into a file which name is provided by user.

Inputs:
  . Output filename.
Output:
  . none.
 *******************************************************
*******************************************************/
void TCalibECCalib::PrintTubesCounts(const char* szFilename /* = "tubescounts.dat" */) const
{

  ofstream fsCountsOut(szFilename);

  if (fsCountsOut.bad()) {
    cerr << "Couldn't write into tubescounts file " << szFilename << "!\n";
    return;
  }

  for (int nSector = 1; nSector <= m_c_nSectors; nSector++) {
    for (int nLayer = 1; nLayer <= m_c_nLayers; nLayer++) {
      for (int nView = 1; nView <= m_c_nViews; nView++) {
    for (int nTubeId = 1; nTubeId <= m_c_nTubes; nTubeId++) {

      int nTubeIndex = TCalibGamma::TubeIndex(nSector, nLayer, nView, nTubeId);
      fsCountsOut << "Sector:\t" << nSector << "\tLayer:\t" << nLayer << "\tView:\t" << nView << "\tTube:\t" << nTubeId << "\tCounts:\t" << m_anCounts[nTubeIndex] << endl;

    }
      }
    }
  }

  fsCountsOut.close();

}












