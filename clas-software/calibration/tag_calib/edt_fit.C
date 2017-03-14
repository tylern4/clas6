//$Id: edt_fit.C,v 1.2 2008/09/25 19:23:05 fklein Exp $

#ifdef __CINT__
const double M_PI = 3.141592654;
#else
#include "ROOT.h"
#endif
#include "TList.h"
#include "TClassMenuItem.h"

// subdirectory where to keep the output files
char* subdirectory="data";

// 2-dim histogram to slice and fit
char *chist_edt="ecnttagr";
// char *chist_edt="ecntr";

//---------------------------------------------------------------------
//    root> .x edt_fit()
// or root> .x edt_fit(first_counter,last_counter,plot_option)
// or root> .x edt_fit(first_counter,last_counter,plot_option,myoutfile)
//               defaults: first_counter=1, last_counter=384, plot_option=0,
//                         myoutfile=<subdirectory>/tage_dt.<runno>.dat
//               plot_option=-1: plot all fits in projections
//
//  macro to slice the plot (Etime-Ttime)_vs_Ecnt and fit the offsets
//---------------------------------------------------------------------

const int ECNT_MAX = 384;
const int maxpeaks=5;

void show_ecounter(int ecnt=0);
void locate_cntr(void);

TH1D* hedt[ECNT_MAX];
TH1F* hedtmean, *hedtoffset, *hedtsigma, *hedtchisq, *hedtarea;
TCanvas* ce = NULL;
TCanvas* cr = NULL;
TLine* cursor = NULL;
TDirectory* mem = NULL;
bool busy = false;
int runno = 0;

void edt_fit(int imin=1, int imax=ECNT_MAX, int opt_plot=0, char *myoutfile=NULL){

  memset(hedt, 0, sizeof(hedt));

  gStyle->SetPalette(1);
  gStyle->SetFrameFillColor(19);
  gStyle->SetNumberContours(30);
  gStyle->SetOptStat(0);

  gROOT->cd();
  mem = new TDirectory("mem","mem");
  gROOT->GetListOfBrowsables()->Add(mem,"mem");
  mem->cd();

  TH2F* hist_edt = (TH2F*) locateObject(chist_edt);
  if( !hist_edt ) {
    cout << "histogram \""<<chist_edt<<"\" not found"<<endl;
    return;
  }

  TH1F* hrunconst = (TH1F*) locateObject("const_run");
  char chrun[16]=" \0";
  if ( hrunconst ) {
    runno = hrunconst->GetBinContent(1);
    if(runno) sprintf(chrun,"(run %d)",runno);
  }
  TH1F* hconst_edt = (TH1F*) locateObject("const_edt");
  if( !hconst_edt ) {
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
    sprintf(outfile,"%s/tage_dt.%d.dat",subdirectory,runno);
  }
  ofstream fp(outfile);

  hedtmean  = new TH1F("edtmean",  "mean",    ECNT_MAX,0.5,ECNT_MAX+0.5);
  hedtoffset= new TH1F("edtoffset","offset",  ECNT_MAX,0.5,ECNT_MAX+0.5);
  hedtarea  = new TH1F("edtarea",  "fit area",ECNT_MAX,0.5,ECNT_MAX+0.5);
  hedtsigma = new TH1F("edtsigma", "sigma",   ECNT_MAX,0.5,ECNT_MAX+0.5);
  hedtchisq = new TH1F("edtchisq", "chisq",   ECNT_MAX,0.5,ECNT_MAX+0.5);
  
  char canvname[80];
  sprintf(canvname,"E-counter time difference  %s",chrun);
  TCanvas *c1 = new TCanvas("c1", canvname,800,500);
  mem->cd();
  c1->cd();
  hist_edt->Draw("colz");
  c1->Update();

  int nbiny   = hist_edt->GetNbinsY();
  double ymin = hist_edt->GetYaxis()->GetBinLowEdge(1);
  double ymax = hist_edt->GetYaxis()->GetBinUpEdge(nbiny);

  // for opt_plot==1
  TCanvas *canv[13];
  int icanv= -1;
  int ipad = opt_plot ? 31 : 0;

  linear = new TF1("linear","pol1");
  gauss  = new TF1("gauss","gaus");
  gauss->SetLineColor(2);
  // fit    = new TF1("fit","gauss+linear");
  fit  = new TF1("fit","gaus");
  fit->SetLineWidth(1);
  fit->SetLineColor(9);

  char histname[80];

  imin= imin>0&&imin<ECNT_MAX ?      imin-1 : 0;
  imax= imax>=imin&&imax<=ECNT_MAX ? imax   : ECNT_MAX;

  for(int i=imin; i < imax; i++){

    if( opt_plot ) {
      if( ipad>30 ) {
	ipad = 1;
	icanv++;
	sprintf(canvname,"c%.2d",icanv+1);
	canv[icanv] = new TCanvas(canvname, "E-counters",
				icanv*5,icanv*5,icanv*5+900,icanv*5+950);
	canv[icanv]->Divide(5,6);
	canv[icanv]->SetLogy();
      }
      canv[icanv]->cd(ipad);
    }

    mem->cd();
    sprintf(histname,"Ecounter%03d",i+1);
    hedt[i]=hist_edt->ProjectionY(histname,i+1,i+1);	//slice the histogram
    sprintf(histname,"Time offset Ecounter %d",i+1);
    hedt[i]->SetTitle(histname);
    
    TSpectrum* spect = new TSpectrum(maxpeaks);
    Int_t  npeaks = spect->Search(hedt[i], 1, "nobackground new", 0.1);
    Float_t *xpeaks= spect->GetPositionX();
    cout <<"In hist for E-cntr "<<i+1<<" found "<<npeaks<<" peaks at pos.:";
    for (int j=0; j<npeaks; j++) cout <<"  "<<xpeaks[j];
    cout <<endl;
    if ( npeaks ) {
      Int_t imm     = TMath::LocMax(npeaks,spect->GetPositionY());
      Double_t mean = xpeaks[imm];
      Double_t max  = spect->GetPositionY()[imm];
      Double_t rms  = 3.0;  // guess
      if( npeaks>1 && ( ( imm>0 && fabs(xpeaks[imm]-xpeaks[imm-1])<2*rms) || 
			( imm<npeaks-1 && fabs(xpeaks[imm]-xpeaks[imm+1])<2*rms ) ) ) {
	rms *=2.5;
      }
    }
    else {
      Double_t max  = hedt[i]->GetMaximum();
      Double_t mean = hedt[i]->GetMean();
      Double_t rms  = hedt[i]->GetRMS();
    }
    //  hslice->Fit("poly","","",-40.,40.);	//fit 1st degree polynomial
    //poly->GetParameters(&par[0]);

    fit->SetParameters(max*0.7,mean,rms*0.7);		
    if( opt_plot ) 
      hedt[i]->Fit("fit","q","same",mean-rms,mean+rms);	//fit the gaussian
    else
      hedt[i]->Fit("fit","q","",mean-rms,mean+rms);	//fit the gaussian

    //    gauss->GetParameters(&par[0]);
    //joefit->SetParameters(par);		
    //hslice->Fit("joefit","","",-25.,25);	//do the full fit
    //joefit->SetLineWidth(1);
    //joefit->SetLineColor(9);
    //hslice->Fit("joefit","","");	//do the full fit
    
    Double_t maxy  = fit->GetParameter(0); //Get the maximum
    Double_t peak  = fit->GetParameter(1); //Get the peak position
    Double_t sigma = fit->GetParameter(2);  //Get the sigma value
    Double_t chisq = fit->GetChisquare();

    cout<<" Ecounter "<<i+1<<": max "<<maxy<<", pos. "<<peak<<", sigma "<<sigma<<", chisq "<<chisq<<endl;

    Double_t edt_db = hconst_edt->GetBinContent(i+1);
    if ( sigma > 0.1 && sigma < 1.5 ) {
      hedtmean->SetBinContent(i+1, peak);
      hedtoffset->SetBinContent(i+1, edt_db+peak);
      hedtarea->SetBinContent(i+1, sigma*maxy*sqrt(2*M_PI));
      hedtsigma->SetBinContent(i+1, sigma);
      hedtchisq->SetBinContent(i+1, chisq);
    }
    else {
      hedtoffset->SetBinContent(i+1, edt_db);
    }

    fp<<"\t"<<edt_db+peak<<endl;  //Print out the channel and peak position

    if( opt_plot ) {
      canv[icanv]->Update();
      ipad++;
    }
  }

  fp.close();	//close the output file

  mem->cd();
  c1->cd();
  TClass* clc1 = c1->IsA();
  TList* mlist = clc1->GetMenuList();
  TClassMenuItem* mitem = new TClassMenuItem(TClassMenuItem::kPopupUserFunction,clc1,
                            "Plot time offset for E-counter","show_ecounter",0,"int");
  mlist->AddFirst(mitem);
  c1->ToggleEventStatus();
  hist_edt->Draw("colz");
  c1->Update();

  sprintf(canvname,"E-counter time offsets  %s",chrun);
  cr = new TCanvas("cr", canvname, 10,10,950,910);
  cr->ToggleEventStatus();
  cr->cd();
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
                            "Plot time offset for E-counter","show_ecounter",0,"int");
  mlst->AddFirst(mitem1);
  pad1->Draw();
  pad2->Draw();
  pad3->Draw();
  pad4->Draw();
  cr->Update();
  mem->cd();
  pad4->cd();
  hedtchisq->Draw();
  pad3->cd();
  hedtarea->Draw();
  pad2->cd();
  hedtsigma->Draw();
  pad1->cd();
  hedtoffset->Draw();

  cout<<"\n Click on counter bin entry in upper plot (with yellow border) or"<<endl;
  cout<<" or right-click menu list in any pad to get plot for single E-counter"<<endl;

  TExec* ex = new TExec("ex", "locate_cntr()");
  ex->Draw("same");
  gSystem->ProcessEvents();
  cr->Modified();
  cr->Update();
}

TObject* locateObject(char* name) {
  /// search memory
  TObject* obj = gROOT->FindObject(name);
  if (obj) return obj;
  /// search files
  TIter nextfile(gROOT->GetListOfFiles());
  TFile *f;
  while (f = (TFile*) nextfile() ) {
    f->UseCurrentStyle();
    if ((obj = f->Get(name))) break;
  }
  return obj;
}

void show_ecounter(int ecnt) {  //ecnt=1...ECNT_MAX
  char cname[80];
  if( ecnt<1 || ecnt>ECNT_MAX ) return;

  sprintf(cname,"E-counter %d",ecnt);
  if ( ce ) {
    ce->cd();
    ce->Clear();
    ce->SetTitle(cname);
  }
  else 
    ce = new TCanvas("ce",cname, 600, 500);
  
  mem->cd();
  cout<<"\n----------------------------------------------------"<<endl;
  if( hedt[ecnt-1] ) {
    hedt[ecnt-1]->Draw();
    double xpos = hedtmean->GetBinContent(ecnt);
    double xsig = hedtsigma->GetBinContent(ecnt);
    double xarea= hedtarea->GetBinContent(ecnt);
    double xchi = hedtchisq->GetBinContent(ecnt);
    double xoff = hedtoffset->GetBinContent(ecnt);
    cout<<" Results for Ecounter "<<ecnt<<":"<<endl;
    cout<<" pos. "<< xpos <<"\t sigma "<< xsig <<"\t fit area "<< xarea <<"\t chisq "<< xchi <<endl;
    cout<<" Corrected time offset: "<< xoff <<endl;
  }
  else
    cout <<" no fit result for E-counter "<<ecnt<<endl;
  cout<<"----------------------------------------------------"<<endl;
  ce->Update();
  cr->cd();
}

void locate_cntr() {
  if (busy) return;
  int px = gPad->GetEventX();
  int py = gPad->GetEventY();
  int ix = (int) floor ((gPad->AbsPixeltoX(px) - 0.5));
  int iy = (int) floor (gPad->AbsPixeltoY(py)*100);
  //   cout << gPad->GetEvent() << ",\tx = " << ix << ",\ty = " << iy << endl;
  if (gPad->GetEvent() != 11) return;
  if (ix < 1 || ix > ECNT_MAX) return;

  busy = true;
  if (cursor) delete cursor;
  cursor = new TLine(ix+0., cr->GetUymin(), ix+0., cr->GetUymax());
  cursor->SetLineColor(4);
  cursor->Draw();
  cr->Modified();
  cr->Update();
  show_ecounter(ix);
  busy = false;
}

void checkin() {
  char command[255];
  sprintf(command,"./checkin.tcl %d %s edt",runno,subdirectory);
  cout << "Exec " << command <<endl;
  gSystem->Exec(command);
}
