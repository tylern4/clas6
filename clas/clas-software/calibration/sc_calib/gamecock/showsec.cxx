#include "ROOT.h"
#include <fstream>
#include "jglobal.h"
#include "Inx.h"

void style(int index);
int fitoffset   (int index, int ii);
int fitlinear   (int index, SingleStripe* sstr=NULL);
int fittimewalk (int index, SingleStripe* sstr=NULL);
int fitgauss    (int index, SingleStripe* sstr=NULL);
int fittdc      (int index, SingleStripe* sstr=NULL);
int fitlandau   (int index, SingleStripe* sstr=NULL);

TCanvas* showhisto   (SingleStripe* sstr);
TCanvas* showtimewalk(SingleStripe* sstr);
TCanvas* showmass    (SingleStripe* sstr);
TCanvas* showtdc     (SingleStripe* sstr, bool refit=false);

TF1* FunctionTdc     (int index, int ii=0);
TF1* FunctionVeff    (int index, int ii=0);
TF1* FunctionAtten   (int index, int ii=0);
TF1* FunctionTimewalk(int index, int ii=0);
TF1* FunctionGmean2  (int index, int ii=0);
TF1* FunctionGauss  (int index, int ii=0);

void compose_fname (char* s, const char* stub, const int index,
		    int style = 0) {
  int sector = index / N_SECTOR +1;
  int stripe = index % N_SECTOR +1;
  switch (style) {
  case 0: sprintf (s, "%s%d%02d", stub, sector, stripe);   break;
  case 1: sprintf (s, "%s%d.%02d", stub, sector, stripe);  break;
  case 2: sprintf (s, "%d_%d_%s", sector, stripe, stub);  break;
  case 3: sprintf (s, "%s single channel: Sector %d, Stripe %d", 
		   stub, sector, stripe);   break;
  case 4: sprintf (s, "%s%d%03d", stub, sector, stripe);   break;
  case 5: sprintf (s, "tdc_SC_s%d_%02d%s", sector, stripe, stub);  break;
  case 6: sprintf (s, "tdc_SCT_s%d_%02d%s", sector, stripe, stub);  break;
  default: throw "compose_fname: style not defined";
  }
}

void fitsingle(int index, SingleStripe* sstr) {
  char cname [80], cmesg[255];
  switch (gCalib->GetType()) {
  case c_atten:
  case c_veff:  
    fitlinear(index, sstr);
    break; 
  case c_timewalk:
    fittimewalk(index, sstr);
    break;     
  case c_gmean:
    fitlandau(index, sstr);
    break;     
  case c_tdc:
  case c_tdcp:
    fittdc(index, sstr);
    break;
  case c_p2pdelay:
    fitgauss(index, sstr);
    break;
  case c_mass:
    fitgauss(index, sstr);
    break;
  default:
    cerr << "fitsingle: no fitsingle defined for type " << *gCalib << endl;
    throw "fitsingle: calibration type not supported";
  }
  
  if (gSector) gSector->Update(index);
  gSurvey->UpdateValues(index,1);
  gSurvey->Update();
  
  compose_fname (cname, "stripe ", index, 1);
  sprintf (cmesg, "%s: refit done", cname);
  gCock->AddLog(cmesg);

}

void showsingle(Inx index) {
  if (gSurvey) gSurvey->SetCursor(index.GetIndex());

  if (!gStripe) {
    gStripe = new SingleStripe(gClient->GetRoot(), 700, 1100, index.GetIndex());
  }
  else {
    gStripe->Clear(index.GetIndex());
  }

  switch (gCalib->GetType()) {
  case c_atten:
  case c_veff:  
  case c_gmean:
  case c_p2pdelay:
  case c_tdc:
  case c_tdcp:
    showhisto(gStripe);
    break; 
  case c_timewalk:
    showtimewalk(gStripe);
    break;     
  case c_mass:
    showmass(gStripe);
    break;
  default:
    cerr << "showsec: no showsingle defined for type " << *gCalib << endl;
    throw "showsingle: calibration type not supported";
  }
  if (gSector) gSector->SetCursor(index.GetIndex());
  if (gStripe) gStripe->RaiseWindow();
}

void showsector(int sec) {
  if (!sec) return;
  gSector = new ShowSector(sec);
}

void setfunctions(int iscon) {
  for (int i=0; i<N_CHANNEL; i++) {
    switch (gCalib->GetType()) {
    case c_tdc:
    case c_tdcp:
      gFitFn[iscon][i] = FunctionTdc(i, iscon);
      fitoffset(i,iscon);
      break;
    case c_veff:     
      gFitFn[iscon][i] = FunctionVeff(i, iscon);     
      fitoffset(i,iscon);
      break;
    case c_atten:
      gFitFn[iscon][i] = FunctionAtten(i, iscon);
      break;
    case c_timewalk: 
      gFitFn[iscon][i] = FunctionTimewalk(i, iscon); 
//      fitoffset(i,iscon);
      break;
    case c_gmean:
      gFitFn[iscon][i] = FunctionGmean2(i, iscon);
      break;
    case c_p2pdelay: //probably SHOULD work for c_mass also
      gFitFn[iscon][i] = FunctionGauss(i, iscon);
      break;
    default:
      throw "setfunctions not defined for this calibration";
      break;
    }
  }
}

void readfile() {
  for (int ipar=0; ipar<gCalib->GetNparSave(); ipar++) {
    char filename[80];
    sprintf (filename, "%s_%s.dat", gCalib->GetSubsystem(ipar), gCalib->GetItem());
    gConst[0] -> SetParameter(filename, gCalib->GetSaveOffs()+ipar);
  }
  for (int i=0; i<N_CHANNEL; i++) {
    switch (gCalib->GetType()) {
    case c_veff:     
      gFitFn[0][i] = FunctionVeff(i);     
      fitoffset(i, 0);
      break;
    case c_atten:
      gFitFn[0][i] = FunctionAtten(i);
      break;
    case c_timewalk: 
      gFitFn[0][i] = FunctionTimewalk(i); 
      fitoffset(i, 0);
      break;
    case c_gmean:
      fitlandau(i,NULL);
      break;
    default:
      cerr << "showsec (read): no function defined for type " << *gCalib << endl;
      throw "readfile: calibration type not supported";
    }
  }
}

void fitall() {
  for (int i=0; i<N_CHANNEL; i++) {
    int retvalue;
    switch (gCalib->GetType()) {
    case c_veff:
    case c_atten:       retvalue = fitlinear(i);       break;
    case c_timewalk:    retvalue = fittimewalk(i);     break;
    case c_gmean:       retvalue = fitlandau(i);       break;
    case c_tdcp:
    case c_tdc:         retvalue = fittdc(i);          break;
    case c_p2pdelay:    retvalue = fitgauss(i);        break;
    case c_mass:        retvalue = fitgauss(i);        break;
    default:
      cerr << "showsec: fitall not defined for type " << *gCalib << endl;
      throw "fitall: calibration type not supported";
    }
    char cname [80];
    compose_fname (cname, "stripe ", i, 1);
    if (retvalue) {
      char warning[256];
      if((i%57 - 48) < 0)
        sprintf (warning, "%s: fit returns with value %d", cname, retvalue);
      else{sprintf (warning, "%s: fit returns with value %d.  If source run prior to 55357 then don't worry about it.", cname, retvalue);}
      gCock->AddLog(warning);
      cout << warning << endl;
    }
    else {
/*       cout.width(3);
       cout << i;
       for (int j=0; j<gCalib->GetNparFit(); j++) {
 	cout << " ";
 	cout.width(12);
 	cout << gConst[0]->GetParameter(i, j);
       }
       cout << endl; */
    }
    style(i);  // define color for (up to five) functions
    gCock->GoProgress((int) floor(100.*i / (double)N_CHANNEL + 0.5)); 
  }
}

TH1F* read_histogram(char* hstname, TDirectory* ff) {
  TH1F* retval = (TH1F*) ff->Get(hstname);
  if (! retval) {
    retval = (TH1F*) gROOT->FindObject(hstname);
    if (! retval) {
//      ff->ls();
      cerr << "error reading histogram " << hstname << endl; 
//      throw "read_histogram: histogram not found in file"; 
    }
  }
  return retval;
}

void showall() {
  char hstname[80];
  gConst[0]  = new SConstants();
  for (int i=1; i<5; i++) gConst[i] = NULL;
  gStyle->SetPalette(1);
  gStyle->SetOptStat(0);
  
  TFile* ff = new TFile (gCalib->GetFileName());
  if (!ff || !ff->IsOpen()) {
    char errmsg[256];
    sprintf (errmsg, "Can't open selected root file %s", 
	     gCalib->GetFileName()); 
    new TGMsgBox(gClient->GetRoot(), gCock, "fit histogram error", 
		 errmsg, kMBIconStop);
    return;
  }

  ff->UseCurrentStyle();

 /// just in case it is needed
  TDirectory* subdir, *subdir2; 
  TH2F* hsector;
  int nbins;

  /// read all histograms
  for (int i=0; i<N_CHANNEL; i++) {
    switch (gCalib->GetType()) {
    case c_tdc:
      /// first one: find subdirectory
      if (!i) {
	subdir = (TDirectory*) ff->Get("SC") ;
	if (!subdir) throw "showall: subdirectory not found";
      }
      //      compose_fname(hstname, (gCalib->IsRight()?"R":"L"), i, 1);
      compose_fname(hstname, (gCalib->IsRight()?"R":"L"), i, 5);
      gGraph[0][i]=(TGraphErrors*) subdir->Get(hstname);
      if (!gGraph[0][i]) {
        if((i%57 - 48) < 0)
          cerr << "graph " << hstname << " not found in file" << endl;
        else{cerr << "graph " << hstname << " not found in file.  If source run prior to 55357 then don't worry about it." << endl;}
	gGraph[0][i]=NULL;
      }
      // graph found : delete stored functions (if any)
      else {     
	gGraph[0][i]->GetListOfFunctions()->Clear();
      }
      break;

    case c_tdcp:
      /// first one: find subdirectory
      if (!i) {
	subdir = (TDirectory*) ff->Get("SCT") ;
	if (!subdir) throw "showall: subdirectory not found";
      }
      //      compose_fname(hstname, (gCalib->IsRight()?"R":"L"), i, 1);
      compose_fname(hstname, (gCalib->IsRight()?"R":"L"), i, 6);
      gGraph[0][i]=(TGraphErrors*) subdir->Get(hstname);
      if (!gGraph[0][i]) {
        if((i%57 - 48) < 0)
     	cerr << "graph " << hstname << " not found in file" << endl;
        else{cerr << "graph " << hstname << " not found in file.  If source run prior to 55357 then don't worry about it." << endl;}
	   gGraph[0][i]=NULL;
      }
      // graph found : delete stored functions (if any)
      else {     
	gGraph[0][i]->GetListOfFunctions()->Clear();
      }
      break;

    case c_p2pdelay:
      /// first one: find subdirectory
      if (!i) {
	subdir = (TDirectory*) ff->Get("FineTune") ;
	if (!subdir) throw "showall: subdirectory not found";
      }
      if (!i) {
	subdir2 = (TDirectory*) ff->Get("CrudeTune") ;
	if (!subdir2) throw "showall: subdirectory not found";
      }
      compose_fname(hstname, "fine", i, 2);
      gHisto[0][i] = read_histogram(hstname, subdir);
      compose_fname(hstname, "crude", i, 2);
      gHisto[1][i] = read_histogram(hstname, subdir2);
      break;

    case c_mass:
      /// first one: find subdirectory
      if (!i) {
	subdir = (TDirectory*) ff->Get("CalibTest") ;
	if (!subdir) throw "showall: subdirectory not found";
      }
      /// new sector
      if (! (i%N_SECTOR)) {
	sprintf (hstname, "MvsS_1_%d", i/N_SECTOR + 1);
	hsector = (TH2F*) read_histogram(hstname, subdir);
      }
      compose_fname(hstname, "h", i);
      nbins =  hsector->GetYaxis()->GetNbins();
      gHisto[0][i] = new TH1F(hstname, hstname, nbins, 
			      hsector->GetYaxis()->GetXmin(), 
			      hsector->GetYaxis()->GetXmax());
      for (int j=0; j<nbins; j++)
	gHisto[0][i]->SetBinContent(j+1, hsector->GetBinContent (i%N_SECTOR+1, j+1));

      break;

    case c_timewalk:
/*      if (gCalib->IsRight()) {
	compose_fname(hstname, "h", i, 4);
	gHisto[0][i] = read_histogram(hstname, ff);
	break;
      } */
    case c_veff:
    case c_gmean:
      if(i%57 >= 48){
        int locTempIndex = 100*(i/57 + 1) + i%57 + 9 + 1;
        compose_fname(hstname, "h", i);
        char *locTempName = new char[20];
        sprintf(locTempName,"h%d", locTempIndex);
        if(ff->Get(locTempName) == NULL){ //true if sc_version_flag = 2
          compose_fname(hstname, "h", i);
          gHisto[0][i] = read_histogram(hstname, ff);
        } //else sc_version_flag = 1, don't want these hists
        delete locTempName;
      }else{
        compose_fname(hstname, "h", i);
        gHisto[0][i] = read_histogram(hstname, ff);
      }
      break;

    default:
      compose_fname(hstname, "h", i);
      gHisto[0][i] = read_histogram(hstname, ff);
      break;
    }
  }

  fitall();

  gSurvey = new JSurvey (gClient->GetRoot(), 700, 700);

  for (int i=0; i<N_CHANNEL; i++) {
    gSurvey->UpdateValues(i,0);
  }
  gSurvey->Update();

  showsector (1);
}
