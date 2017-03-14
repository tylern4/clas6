
using namespace std;

#include <cstdlib>
#include <iostream>
#include <TStyle.h>
#include <TF1.h>
#include "TCalibECFit.h"


/*******************************************************
********************************************************
Name: TCalibECFit::TCalibECFit
Date: 05/01/2001
Author: Matthieu Guillo

Description:
Constructor. Reads calibration constants from a file, create the ROOT tree as well as histogramms to store EC time expected - EC time fitted for every tube.

Input:
  . input calibration constants file name (variable szCalibConstIn)
  . output ROOT file name for storing the tree and the histograms (variable szRootOut)
  . minimum ADC value in a tube for calibration (variable dMinAdcValue)
  . minimum number of hits in a tube for fitting Texpected -Tfitted distribution (variable nMinHitsForFit)
Output:
  . none
*******************************************************
*******************************************************/
TCalibECFit::TCalibECFit(const char* szCalibConstIn /* = "calibconsts.dat" */,
             const char* szRootOut /* = "calib.root" */,
             double dMinAdcValue /* = 100. */,
             int nMinHitsForFit /* = 50 */)
  : TCalibEC(dMinAdcValue),
    m_fileROOT(szRootOut, "RECREATE"),
    m_tree("tree", "calibration check"),
    m_c_nMinHitsForFit(nMinHitsForFit)

{

  // ----- Try to open the calibration constants and fit constants files for reading -----
  ifstream fsCalibConstIn(szCalibConstIn);
  if (!fsCalibConstIn) {
    // exit if  cannot read calibration entries!
    cerr << "Couldn't read calibration file " << szCalibConstIn << "!\n";
    exit(8);
  }

  // ----- Try to open the ROOT file in the constructor, before the tree and histos are instantiated in order to associate them to the file (ROOT trick)  -----
  if (!m_fileROOT.IsOpen()) {
    // no exit! tree and histograms will not be written, this doesn't prevent the fit, just no visual informations.
    cerr << "Couldn't write into ROOT file " << szRootOut << "!\n";
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

  // ----- initialise tree (branches definition) and dynamically create histograms -----
  m_InitTree();
  m_InitHistos();

}


/*******************************************************
********************************************************
Name: TCalibECFit::~TCalibECFit
Date: 05/01/2001
Author: Matthieu Guillo

Description:
Destructor. Free allocated space for histograms and close ROOT file.

Input:
  . none
Output:
  . none
*******************************************************
*******************************************************/
TCalibECFit::~TCalibECFit()
{

  for (int nTubeIndex = 0; nTubeIndex < m_c_nTotalTubes; nTubeIndex++) {
    if (m_h1Fit[nTubeIndex]) {
      delete m_h1Fit[nTubeIndex];
    }
    m_h1Fit[nTubeIndex] = NULL;  // security style
  }

  if (m_fileROOT.IsOpen()) {
    m_fileROOT.Close();
  }

}


/*******************************************************
 *******************************************************
Name: TCalibECFit::m_InitTree
Date: 05/01/2001
Author: Matthieu Guillo

Description:
Creates 2 branches for the tree. The first holds informations like ADC,TDC, time expected etc... while the second holds the value of each term of the model.

Input:
  . none
Output:
  . none
 *******************************************************
*******************************************************/
void TCalibECFit::m_InitTree()
{

  // branch for gamma information
  m_tree.Branch("gamma", &m_brGamma, "vtime/F:adc/F:tdc/F:L/F:l/F:texpect/F:tmodel/F:sector/I:layer/I:view/I:tube/I:tubeindex/I");

  // branch for different terms in the fit
  char szVar[20];
  sprintf(szVar, "term[%d]/F", m_c_nCalibConsts);
  m_tree.Branch("terms", &m_afModelTerms, szVar);

}


/*******************************************************
 *******************************************************
Name: TCalibECFit::m_InitHistos
Date: 05/01/2001
Author: Matthieu Guillo

Description:
Dynamically creates histograms to store the difference between the time expected and time calculated from the model in the EC. Those histogramms will be later fitted by a gaussian (hopefully the distribution is gaussian) to check the quality of the calibration constants.

Input:
  . none
Output:
  . none
 *******************************************************
*******************************************************/
void TCalibECFit::m_InitHistos()
{

  for (int nSector = 1; nSector <= m_c_nSectors; nSector++) {
    for (int nLayer = 1; nLayer <= m_c_nLayers; nLayer++) {
      for (int nView = 1; nView <= m_c_nViews; nView++) {
    for (int nTubeId = 1; nTubeId <= m_c_nTubes; nTubeId++) {
      int nTubeIndex = TCalibGamma::TubeIndex(nSector, nLayer, nView, nTubeId);
      char  szHistoTitle[80];
      char  szHistoRef[30];      // histo ref (for ROOT)

      // --------- define histos for Texpect - Tfit ---------

      sprintf(szHistoTitle, "T_{expect} - T_{model} (ns); S:%d, L:%d, V:%d, T:%d", nSector, nLayer, nView, nTubeId); // create histogramm title
      sprintf(szHistoRef, "h_S%dL%dV%dT%d", nSector, nLayer, nView, nTubeId); // create histogramm reference for Root
      m_h1Fit[nTubeIndex] = new TH1F(szHistoRef, szHistoTitle, 100, -6.0, 6.0);  // dynamically create histogram
    }
      }
    }
  }

}


/*******************************************************
 *******************************************************
Name: TCalibECFit::UseGamma
Date: 05/01/2001
Author: Matthieu Guillo

Description:
This methods does 2 things:
  . computes the EC time from the calibration as well as the expected time form the hit position.
  . fills the corresponding histogram and the tree.

Input:
  . values for computing the time from the calibration
Output:
  . none
 *******************************************************
*******************************************************/
void TCalibECFit::UseGamma(double dAdc, double dTdc, double dVertex2Centroid, double dEcSurface2Edge, double dVertexTime, int nLayer, int nView)
{

  int nTubeId = m_CurrentGamma->TubeIdMaxAdc(nLayer, nView);
  int nSector = m_CurrentGamma->Sector();
  int nTubeIndex = TCalibGamma::TubeIndex(nSector, nLayer, nView, nTubeId);

  double dValue[m_c_nCalibConsts] = { 1.0, dTdc, 1. / sqrt(dAdc), dEcSurface2Edge * dEcSurface2Edge, dEcSurface2Edge * dEcSurface2Edge * dEcSurface2Edge };

  // ----- computes time from the calibration -----
  double dTimeModel = 0.;   // initialisation
  // each term in the expansion is recorded in the fitTerms array and addded to the time fitted
  for (int n = 0; n < m_c_nCalibConsts; n++) {
    m_afModelTerms[n] = (float) (m_adCalibConsts[nTubeIndex][n] * dValue[n]);
    dTimeModel += m_adCalibConsts[nTubeIndex][n] * dValue[n];
  }
  dTimeModel -= dVertexTime; // substract vertex time (reference);

  // ----- computes time expected from hit position -----
  double dTimeExpected =  dVertex2Centroid / TCalibGamma::m_c_dSpeedOfLight +  dEcSurface2Edge / m_c_dEffectiveSpeedInPlastic;

  // ----- fills structure for the tree -----
  m_brGamma.fVTime = (float) dVertexTime;
  m_brGamma.fAdc = (float) dAdc;
  m_brGamma.fTdc = (float) dTdc;
  m_brGamma.fVertex2Centroid = (float) dVertex2Centroid;
  m_brGamma.fHit2View = (float) dEcSurface2Edge;
  m_brGamma.fTimeExpected = (float) dTimeExpected;
  m_brGamma.fTimeModel = (float) dTimeModel;
  m_brGamma.nSector = nSector;
  m_brGamma.nLayer = nLayer;
  m_brGamma.nView = nView;
  m_brGamma.nTubeId = nTubeId;
  m_brGamma.nTubeIndex = nTubeIndex;

  cout<<"!!!! "<<dTimeExpected<<" "<<dTimeModel<<" "<<dVertexTime<<endl;
  // fills tree and histogramms
  m_tree.Fill();
  m_h1Fit[nTubeIndex]->Fill((float) (dTimeExpected - dTimeModel));

}


/*******************************************************
 *******************************************************
Name: TCalibECFit::FitTimeDifference
Date: 05/01/2001
Author: Matthieu Guillo

Description:
This methods does 2 things:
1) Try to fit the histograms with a gaussian distribution.
2) Write the fits results (height, centroid and sigma) into a file.

Input:
 . output fit constants file name (variable szFitConstOut)
Output:
 . number of histograms fitted (if not enough statistics, don't try the fit)
 *******************************************************
*******************************************************/
int TCalibECFit::FitTimeDifference(const char* szFitConstOut /* = "fitconsts.dat" */) const
{
  int nFittedHistos = 0;
  ofstream fsFitConstOut(szFitConstOut);

  if (fsFitConstOut.bad()) {
    // exit if couldn't open the file
    cerr << "Couldn't write into fit constants file " << szFitConstOut << "!\n";
    exit(8);
  }

  gStyle->SetOptStat(11);  // ROOT statistic box option
  gStyle->SetOptFit(11);   // ROOT fit box option

  for (int nTubeIndex = 0; nTubeIndex < m_c_nTotalTubes; nTubeIndex++) {
    int nEntries = static_cast<int>(m_h1Fit[nTubeIndex]->GetEntries());
    if  (nEntries < m_c_nMinHitsForFit) {
      // some "error" values are written when fit not possible
      fsFitConstOut << m_c_dError << " " << m_c_dError << " " << m_c_dError << endl;
    }
    else {
      // enought statistics for the fit
      nFittedHistos++;  // one more histo fitted
      // gets starting guesses for fit
      float fMean = m_h1Fit[nTubeIndex]->GetMean();
      float fRms = m_h1Fit[nTubeIndex]->GetRMS();
      float fMaxValue = m_h1Fit[nTubeIndex]->GetMaximum();
      // range of the fit
      float fXmin =  fMean - fRms;
      float fXmax = fMean + fRms;

      TF1 fxGauss("fxGauss","gaus", fXmin, fXmax); // gaussian fit with a range

      // initialization for fitting
      fxGauss.SetParameter(0, fMaxValue); // starting point for gaussian height
      fxGauss.SetParameter(1, fMean); // starting point for gaussian centroid (x0)
      fxGauss.SetParameter(2, fRms);  // starting point for gaussian width (sigma)
      fxGauss.SetParNames("A","x_{0}","#sigma"); // gives names for the fit parameters

      m_h1Fit[nTubeIndex]->Fit("fxGauss","R0"); // fits in the range given by fitGauss and draw fit on histogram.

      double dParam[3]; // will contain parameters for Gaussian
      fxGauss.GetParameters(&dParam[0]); // get fit parameters
      double dHeight =  dParam[0], dPeak = dParam[1], dSigma = dParam[2];

      // write height, centroid and sigma in that order
      fsFitConstOut << dHeight << " " << dPeak << " " << dSigma << endl;
    }
  }

  return(nFittedHistos);
}


/*******************************************************
 *******************************************************
Name: TCalibECFit::WriteHistos
Date: 05/01/2001
Author: Matthieu Guillo

Description:
Writes the tree and histograms into the ROOT file.

Input:
 . none
Output:
 . true or false according of success or failure of writting into ROOT file.
 *******************************************************
*******************************************************/
bool TCalibECFit::WriteHistos()
{
  bool bWriteSuccess = false;

  if (m_fileROOT.IsOpen()){
    bWriteSuccess = true;
    m_tree.Write(); // writes the tree
    for (int nTubeIndex = 0; nTubeIndex < m_c_nTotalTubes; nTubeIndex++) {
      m_h1Fit[nTubeIndex]->Write();  // writes histo
    }
  }

  return (bWriteSuccess);
}








