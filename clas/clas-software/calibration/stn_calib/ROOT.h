//$Id: ROOT.h,v 1.3 2008/04/08 15:18:37 fklein Exp $
#ifndef __ALL_ROOT_HEADER_H
#define __ALL_ROOT_HEADER_H

#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <Hoption.h>
#include <Hparam.h>
#include <TApplication.h>
#include <TArrow.h>
#include <TButton.h>
#include <TCanvas.h>
#include <TCint.h>
#include <TControlBar.h>
#include <TDialogCanvas.h>
#include <TDirectory.h>
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,16,0)
#include <TDirectoryFile.h>
#else
#include <TPadView3D.h>
#endif
#include <TEnv.h>
#include <TExec.h>
#include <TF1.h>
#include <TF2.h>
#include <TFile.h>
#include <TFrame.h>
#include <TGenPhaseSpace.h>
#include <TGraph.h>
#include <TGraphErrors.h>
#include <TH1.h>
#include <TH2.h>
#include <TH3.h>
#include <THistPainter.h>
#include <TLatex.h>
#include <TLine.h>
#include <TLorentzRotation.h>
#include <TLorentzVector.h>
#include <TMarker.h>
#include <TMatrixD.h>
#include <TPad.h>
#include <TProfile.h>
#include <TPolyLine3D.h>
#include <TH2.h>
#include <TROOT.h>
#include <TRandom.h>
#include <TRint.h>
#include <TRootEmbeddedCanvas.h>
#include <TSocket.h>
#include <TSQLResult.h>
#include <TSQLRow.h>
#include <TSQLServer.h>
#include <TStyle.h>
#include <TSysEvtHandler.h>
#include <TSystem.h>
#include <TTimer.h>
#include <TVector.h>
#include <TVector3.h>
#include <TView.h>
#include <TVirtualPS.h>
#include <TVirtualX.h>

using namespace std;

#endif
