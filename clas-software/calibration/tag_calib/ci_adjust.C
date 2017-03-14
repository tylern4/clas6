//$Id: ci_adjust.C,v 1.3 2008/09/25 19:23:05 fklein Exp $

#ifdef __CINT__
const double M_PI = 3.141592654;
#else
#include "ROOT.h"
#endif
#include "TList.h"
#include "TClassMenuItem.h"

// subdirectoy where the files are kept
char *subdirectory="data";

// 2-dim histogram to slice and fit
char *chist_ttag="rftag";

//-------------------------------------------------------------------------------
//    root> .x ci_adjust()
// or root> .x ci_adjust(plot_opt,myoutfile,myinfile)
//                 default: myoutfile=<subdirectory>/tagt_ci.<runno>.dat
//                 plot_opt=1: show fits for all 121 bins in canvas (default=0)
//
// macro to slice the plot (RFtime - TaggerTime)_vs_Tid (or similar plot), 
// then fit a gaussian (above an n degree polynomial?) and calculate the offsets. 
//-------------------------------------------------------------------------------

const int TBIN_MAX = 121;
const int maxpeaks = 3;

void show_tbin(int tbin=0);
void locate_cntr(void);

TDirectory *mem = NULL;
TCanvas* ce = NULL;
TCanvas* cr = NULL;
TCanvas* cl = NULL;
TLine* cursor = NULL;
bool busy = false;
int runno = 0;

TH1D* htci[TBIN_MAX];
TH1F* htbinmean, *htbinoffset, *htbinsigma, *htbinchisq, *htbinarea;

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

void ci_adjust(int plot_opt=0, char *myoutfile=NULL, char *myinfile=NULL) {

  TH2F* hist2d=NULL;

  gStyle->SetPalette(1);
  gStyle->SetFrameFillColor(19);
  gStyle->SetNumberContours(30);

  if( myinfile && gSystem->AccessPathname(myinfile) ) {
    gROOT->Reset();
    TFile* fin=new TFile(myinfile,"READ");  //Read the root file
    hist2d=(TH2F*)fin->Get(chist_ttag);
  }

  gROOT->cd();
  mem = new TDirectory("mem", "mem");
  gROOT->GetListOfBrowsables()->Add(mem, "mem");
  mem->cd();
  if( !hist2d ) hist2d = (TH2F*) locateObject(chist_ttag);
  
  if( !hist2d ) {
    cout << "histogram \""<<chist_ttag<<"\" not found"<<endl;
    return;
  }

  TH1F* hrunconst  = (TH1F*) locateObject("const_run");
  char chrun[16]=" \0";
  if ( hrunconst ) {
    runno = hrunconst->GetBinContent(1);
    if (runno) sprintf(chrun,"(run %d)",runno);
  }

  TH1F* hconst_tci = (TH1F*) locateObject("const_tci");
  if( !hconst_tci ) {
    cout <<"  no histogram with calibration constants found " <<endl;
    return;
  }

  char outfile[255];
  if ( myoutfile ) 
    strcpy(outfile,myoutfile);
  else {
    char *dir=gSystem->OpenDirectory(subdirectory);
    if( !dir ) {
      if( gSystem->MakeDirectory(subdirectory) )   // 0 for success, -1 for fail
	strcpy(subdirectory,".");
    }
    sprintf(outfile,"%s/tagt_ci.%d.dat",subdirectory,runno);
  }
  ofstream fp(outfile);

  htbinoffset= new TH1F("htbinoffset","offset",TBIN_MAX,0.5,TBIN_MAX+0.5);
  htbinmean  = new TH1F("htbinmean","mean",    TBIN_MAX,0.5,TBIN_MAX+0.5);
  htbinarea  = new TH1F("htbinarea","fit area",TBIN_MAX,0.5,TBIN_MAX+0.5);
  htbinsigma = new TH1F("htbinsigma","sigma",  TBIN_MAX,0.5,TBIN_MAX+0.5);
  htbinchisq = new TH1F("htbinchisq","chisq",  TBIN_MAX,0.5,TBIN_MAX+0.5);
  
  char canvtitle[80];
  sprintf(canvtitle,"RF - Ttag  %s",chrun);
  TCanvas* c0 = new TCanvas("c0", canvtitle, 600,400);
  c0->cd();
  hist2d->Draw("colz");
  c0->Update();

  if( plot_opt ) {
    sprintf(canvtitle,"RF - Ttag per T-counter bin  %s",chrun);
    cl = new TCanvas("c1", canvtitle, 10,0,1110,1100);
    cl->Divide(11,11);
    cl->SetLogy();
  }

  char histname[80];

  for(int i=0;i<TBIN_MAX;i++){

    if( plot_opt ) cl->cd(i+1);
    sprintf(histname,"Tbin%03d",i+1);
    htci[i]=hist2d->ProjectionY(histname,i+1,i+1);	//slice the histogram
    sprintf(histname,"Ttag offset in Tbin %d",i+1);
    htci[i]->SetTitle(histname);
    //    htci[i]->Draw();

    TSpectrum* spect = new TSpectrum(maxpeaks);
    Int_t  npeaks = spect->Search(htci[i], 1, "nobackground", 0.1);
    Float_t *xpeaks= spect->GetPositionX();
    cout <<"In hist for bin "<<i+1<<" found "<<npeaks<<" peaks:";
    for (int j=0; j<npeaks; j++) cout <<" "<<xpeaks[j];
    cout <<endl;
    if ( npeaks ) {
      Int_t imax    = TMath::LocMax(npeaks,spect->GetPositionY());
      Double_t mean = xpeaks[imax];
      Double_t max  = spect->GetPositionY()[imax];
      Double_t rms  = 0.3;  // guess
      if( npeaks>1 && ( ( imax>0 && fabs(xpeaks[imax]-xpeaks[imax-1])<0.5 ) || 
			( imax<npeaks && fabs(xpeaks[imax]-xpeaks[imax+1])<0.5 ) ) ) 
	rms *=2;
    }
    else {
      Double_t mean = htci[i]->GetMean();
      Double_t max  = htci[i]->GetMaximum();
      Double_t rms  = htci[i]->GetRMS();
    }

    // fit the spectrum around its maximum (xpeaks[0])
    // need background fit?
    fit  = new TF1("fit","gaus");
    fit->SetLineWidth(1);
    fit->SetLineColor(9);
    fit->SetParameters(max*0.7, mean, rms*0.5);
    if( plot_opt ) 
      htci[i]->Fit("fit","q","same",mean-rms,mean+rms);       //fit the gaussian
    else
      htci[i]->Fit("fit","q","",mean-rms,mean+rms);           //fit the gaussian
    //Double_t par[3];
    //fit->SetParameters(par);
    //fit->SetParLimits(2,par[2]-par[3],par[2]+par[3]);
    //fit->SetParLimits(3,0,par[3]);
    //fit->SetLineWidth(1);
    //fit->SetLineColor(9);
    //htci[i]->Fit("fit","q","");        //do the full fit
    //fit->GetParameters(&par[0]);

    Double_t maxy  = fit->GetParameter(0); //Get the maximum
    Double_t peak  = fit->GetParameter(1); //Get the peak position
    Double_t sigma = fit->GetParameter(2);  //Get the sigma value
    Double_t chisq = fit->GetChisquare();

    cout<<" Bin "<<i+1<<": max "<<maxy<<", pos. "<<peak<<", sigma "<<sigma<<", chisq "<<chisq<<endl;

    Double_t tci_db = hconst_tci->GetBinContent(i+1);
    if ( sigma > 0.05 && sigma < 0.5 ) {
      htbinmean->SetBinContent(i+1, peak);
      htbinoffset->SetBinContent(i+1, tci_db+peak);
      htbinarea->SetBinContent(i+1, sigma*maxy*sqrt(2*M_PI));
      htbinsigma->SetBinContent(i+1, sigma);
      htbinchisq->SetBinContent(i+1, chisq);
    }
    else {
      htbinmean->SetBinContent(i+1, 0);
      htbinoffset->SetBinContent(i+1, tci_db);
    }

    fp<<"\t"<<tci_db+peak<<endl;  //Print out the channel and peak position

    if( plot_opt ) cl->Update();
  }

  fp.close();	//close the output file

  mem->cd();
  c0->cd();
  TClass* clc0 = c0->IsA();
  TList* mlist = clc0->GetMenuList();
  TClassMenuItem* mnew = new TClassMenuItem(TClassMenuItem::kPopupUserFunction,clc0,
			    "Plot time offset for T-bin","show_tbin",0,"int");
  mlist->AddFirst(mnew);
  hist2d->Draw("colz");
  c0->Update();

  sprintf(canvtitle,"Tbin fit  %s",chrun);
  cr = new TCanvas("cr", canvtitle, 600,800);
  cr->ToggleEventStatus();
  TPad* pad1 = new TPad("pad1","offset",  0.0,0.751,1.0,1.0);
  TPad* pad2 = new TPad("pad2","sigma",   0.0,0.501,1.0,0.75);
  TPad* pad3 = new TPad("pad3","fit area",0.0,0.251,1.0,0.50);
  TPad* pad4 = new TPad("pad4","chisq",   0.0,0.0,  1.0,0.25);
  TClass* clpad = pad1->IsA();
  TList*  mlst  = clpad->GetMenuList();
  TClassMenuItem* mitem2 = new TClassMenuItem(TClassMenuItem::kPopupUserFunction,clpad,
                            "checkin","checkin");
  mlst->AddFirst(mitem2);
  TClassMenuItem* mitem1 = new TClassMenuItem(TClassMenuItem::kPopupUserFunction,clpad,
                            "Plot time offset for T-bin","show_tbin",0,"int");
  mlst->AddFirst(mitem1);
  pad1->Draw();
  pad2->Draw();
  pad3->Draw();
  pad4->Draw();
  cr->Update();
  mem->cd();
  cr->cd();
  pad4->cd();
  htbinchisq->Draw();
  pad3->cd();
  htbinarea->Draw();
  pad2->cd();
  htbinsigma->Draw();
  pad1->cd();
  htbinoffset->Draw();

  cout<<"\n Click on counter bin entry in upper plot (with yellow border) or"<<endl;
  cout<<" or right-click menu list in any pad to get plot for single T-counter bin"<<endl;

  TExec* ex = new TExec("ex", "locate_cntr()");
  ex->Draw("same");
  gSystem->ProcessEvents();
  cr->Modified();
  cr->Update();
}


void show_tbin(int tbin) {  //tbin=1...TBIN_MAX
  char cname[80];
  if( tbin<1 || tbin>TBIN_MAX ) return;

  sprintf(cname,"T-counter bin %d",tbin);
  if ( ce ) {
    ce->cd();
    ce->Clear();
    ce->SetTitle(cname);
  }
  else 
    ce = new TCanvas("ce",cname, 600, 500);
  
  mem->cd();
  cout<<"\n----------------------------------------------------"<<endl;
  if( htci[tbin-1] ) {
    htci[tbin-1]->Draw();
    double xpos = htbinmean->GetBinContent(tbin);
    double xsig = htbinsigma->GetBinContent(tbin);
    double xarea= htbinarea->GetBinContent(tbin);
    double xchi = htbinchisq->GetBinContent(tbin);
    double xoff = htbinoffset->GetBinContent(tbin);
    if( tbin%2 ) 
      cout<<" Results for T-bin "<<tbin<<" (T-counter "<<(tbin+1)/2<<" -no overlap) :"<<endl;
    else
      cout<<" Results for T-bin "<<tbin<<" (overlap T-counters "<<tbin/2<<","<<tbin/2+1<<") :"<<endl;
    cout<<" pos. "<< xpos <<"\t sigma "<< xsig <<"\t fit area "<< xarea <<"\t chisq "<< xchi <<endl;
    cout<<" Corrected time offset: "<< xoff <<endl;
  }
  else
    cout <<" no fit result for T-counter bin "<<tbin<<endl;
  cout<<"----------------------------------------------------"<<endl;
  ce->Update();
  cr->cd();
}

void locate_cntr() {
  if (busy) return;
  int px = gPad->GetEventX();
  int py = gPad->GetEventY();
  int ix = (int) floor ((gPad->AbsPixeltoX(px) + 0.5));
  int iy = (int) floor (gPad->AbsPixeltoY(py)*100);
  //   cout << gPad->GetEvent() << ",\tx = " << ix << ",\ty = " << iy << endl;
  if (gPad->GetEvent() != 11) return;
  if (ix < 1 || ix > TBIN_MAX) return;

  busy = true;
  if (cursor) delete cursor;
  cursor = new TLine(ix+0., cr->GetUymin(), ix+0., cr->GetUymax());
  cursor->SetLineColor(4);
  cursor->Draw();
  cr->Modified();
  cr->Update();
  show_tbin(ix);
  busy = false;
}

void checkin() {
  char command[255];
  sprintf(command,"./checkin.tcl %d %s tci",runno,subdirectory);
  cout << "Exec " << command << endl;
  gSystem->Exec(command);
}
