//$Id: rf_check.C,v 1.2 2008/09/25 19:23:05 fklein Exp $

#ifdef __CINT__
const double M_PI = 3.141592654;
#else
#include "ROOT.h"
#endif

// subdirectoy where the files are kept
char *subdirectory="data";

//-------------------------------------------------------------------------------
//    root> .x rf_check()
//
// macro to check RF related plots: TDC difference for RF3 and RF4 (from RFT bank)
// to get RF slope and relative offset between RF3 and RF4
//
//-------------------------------------------------------------------------------

const double RFcycle = 2.004008;

TObject* locateObject(char* name) {
  /// search memory
  TObject* obj = gROOT->FindObject(name);
  if (obj) return obj;
  /// search files
  TIter nextfile(gROOT->GetListOfFiles());
  TFile *f;
  while ( (f = (TFile*) nextfile()) ) {
    f->UseCurrentStyle();
    if ( (obj = f->Get(name)) ) break;
  }
  return obj;
}

void rf_check(char* myoutfile=NULL) {

  gStyle->SetPalette(1);
  gStyle->SetFrameFillColor(19);
  gStyle->SetOptFit(11);
  gStyle->SetOptStat(11);

  gROOT->cd();
  TH1F *hconst = (TH1F*) locateObject("const_rf");
  if( !hconst ) {
    cout <<"RF constants not found "<<endl;
    return;
  }
  int use_rf = (int) hconst->GetBinContent(1);
  cout<<" RF tdc "<<use_rf<<" is used"<<endl;
  double rf_slope[4];
  for (int i=0; i<4; i++)
    rf_slope[i] = hconst->GetBinContent(19+i);   //entry=22 for RF_tdc4

  TH1F *hrunconst = (TH1F*) locateObject("const_run");
  char chrun[16]=" \0";
  int runno = 0;
  if( hrunconst ) {
    runno = hrunconst->GetBinContent(1);
    sprintf(chrun,"(run %d)",runno);
  }
  char hstname[80];

  TProfile* hrawprof[4], *hrfprof[4];
  TH2F* hrffun[4], *hrf1fun[4], *hrfcl01[4];
  TH1F* hrawdiff[2], *hrawmod[2], *hrfmod[2];
  hrawdiff[0] = (TH1F*) locateObject("rfrawdiff0");
  hrawdiff[1] = (TH1F*) locateObject("rfrawdiff1");
  hrawmod[0]  = (TH1F*) locateObject("rfrawmod0");
  hrawmod[1]  = (TH1F*) locateObject("rfrawmod1");
  hrfmod[0]   = (TH1F*) locateObject("rfmod0");
  hrfmod[1]   = (TH1F*) locateObject("rfmod1");
  hrffun[0]   = (TH2F*) locateObject("rffun0");
  hrffun[1]   = (TH2F*) locateObject("rffun1");
  hrffun[2]   = (TH2F*) locateObject("rffun2");
  hrffun[3]   = (TH2F*) locateObject("rffun3");
  hrfcl01[0]  = (TH2F*) locateObject("rfcl010");
  hrfcl01[1]  = (TH2F*) locateObject("rfcl011");
  hrfcl01[2]  = (TH2F*) locateObject("rfcl012");
  hrfcl01[3]  = (TH2F*) locateObject("rfcl013");

  double sloperaw[4]={0.,0.,0.,0.};
  double sloperf[4]={0.,0.,0.,0.};
  for (int i=0; i<4; i++) {
    if (hrffun[i] && hrffun[i]->Integral() > 10000) {
      // check whether distribution on lower or upper edge
      TH1D* hrfproj = hrffun[i]->ProjectionY();
      double xsig=1.0;
      double xpos=0.0;
      if (!hrfproj->Fit("gaus","q")) {
	TF1* fitproj = (TF1*) hrfproj->GetFunction("gaus");
	xpos = fitproj->GetParameter(1);
	xsig = fitproj->GetParameter(2);
      }
      if (xsig>0.5 || xpos>0.8 || xpos<-0.8) {
	int nbinx = hrffun[i]->GetNbinsX();
	int nbiny = hrffun[i]->GetNbinsY();
	double xlow = hrffun[i]->GetBinLowEdge(1);
	double xup  = hrffun[i]->GetBinLowEdge(nbinx)+hrffun[i]->GetBinWidth(1);
	sprintf(hstname,"rf1fun%d",i);
	hrf1fun[i] = new TH2F(hstname,hstname,nbinx,xlow,xup,100,-0.55,1.55);
	for (int k=1; k<=nbinx; k++) {
	  for (int j=3; j<nbiny-2; j++) {
	    float xy = hrffun[i]->GetBinContent(k,j);
	    hrf1fun[i]->SetBinContent(k,(j<26?j+72:j-22),xy);
	  }
	}
      }
      else 
	hrf1fun[i] = (TH2F*) hrffun[i]->Clone();

      hrawprof[i] = hrf1fun[i]->ProfileX();
      if (hrawprof[i]->Fit("pol1","q")) {
	cout << "fit failed for RF profile " << i+1 << endl;
      }
      else {
	TF1* fitprof = (TF1*) hrawprof[i]->GetFunction("pol1");
	fitprof->SetLineColor(4);
	sloperaw[i]  = rf_slope[i] - fitprof->GetParameter(1);
      }

      hrfprof[i] = hrfcl01[i]->ProfileX();
      if (hrfprof[i]->Fit("pol1","q","",9900,11400)) {
	cout << "fit failed for RF profile " << i+1 << endl;
      }
      else {
	TF1* fitrfprof = (TF1*) hrfprof[i]->GetFunction("pol1");
	fitrfprof->SetLineColor(2);
	sloperf[i]  = rf_slope[i] - fitrfprof->GetParameter(1);
      }
    }      
  }

  char canvtitle[80];
  sprintf(canvtitle,"RF profile  %s",chrun);
  TCanvas *c0 = new TCanvas("c0", canvtitle, 850,850);
  c0->Divide(2,2);
  if (use_rf<3) {
    c0->cd(1); hrf1fun[0]->Draw("col"); 
    c0->cd(3); hrfcl01[0]->Draw("col");
    c0->cd(2); hrf1fun[1]->Draw("col"); 
    c0->cd(4); hrfcl01[1]->Draw("col");
  }
  else {
    c0->cd(1); hrf1fun[2]->Draw("col"); hrawprof[2]->Draw("same"); 
    c0->cd(3); hrfcl01[2]->Draw("col"); hrfprof[2]->Draw("same"); 
    c0->cd(2); hrf1fun[3]->Draw("col"); hrawprof[3]->Draw("same"); 
    c0->cd(4); hrfcl01[3]->Draw("col"); hrfprof[3]->Draw("same"); 

    sprintf(canvtitle,"RF check  %s",chrun);
    TCanvas *c1 = new TCanvas("c1",canvtitle, 100,100,650,650);
    c1->Divide(2,2);

    TF1* fit = new TF1("fit","gaus");
    fit->SetLineWidth(1);
    fit->SetLineColor(9);
    double slope[2];
    for (int i=0; i<2; i++) {
      double mean = hrawdiff[i]->GetMean();
      double ymax = hrawdiff[i]->GetMaximum();
      double rms  = hrawdiff[i]->GetRMS();
      c1->cd(1+i);
      hrawdiff[i]->Draw();
      if( ymax > 100 ) {
	fit->SetParameters(ymax*0.7, mean, rms);
	hrawdiff[i]->Fit("fit","q","same",mean-3*rms,mean+3*rms);
	double xpos = fit->GetParameter(1);
	double sigm = fit->GetParameter(2);
	slope[i] = -80*RFcycle/xpos;
	cout<<"for RF tdc"<<i+3<<": peak at "<<xpos<<" +/- "<<2*sigm<<endl;
	cout<<"       slope "<<sloperf[i+2]<<" (raw: "<<sloperaw[i+2]<<
	  ", diff: "<<slope[i]<<"; slope in caldb: "<<rf_slope[i+2]<<")"<<endl;
      }
      else {
	slope[i] = rf_slope[i+2];
	cout<<"for RF tdc"<<i+3<<": not enough entries in histogram => keep slope "<<rf_slope[i+2]<<endl;
      }
      c1->cd(3+i);
      hrawmod[i]->Draw();
    }
    c1->Update();
  }
  // write slope to file
  char outfile[255];
  if ( myoutfile ) 
    strcpy(outfile,myoutfile);
  else {
    char *dir=gSystem->OpenDirectory(subdirectory);
    if( !dir ) {
      if( gSystem->MakeDirectory(subdirectory) )   // 0 for success, -1 for fail
	strcpy(subdirectory,".");
    }
    sprintf(outfile,"%s/CALL_T1.%d.dat",subdirectory,runno);
  }
  ofstream fp(outfile);

  for (int i=1; i<7; i++)
    fp <<"\t"<< hconst->GetBinContent(12+i)<<endl;
  for (int i=0; i<4; i++) 
    fp <<"\t"<< (sloperf[i]!=0.0? sloperf[i] : rf_slope[i]) <<endl;
  //  fp<<"\t"<<slope[0]<<endl;
  //  fp<<"\t"<<slope[1]<<endl;
  fp.close();
}


