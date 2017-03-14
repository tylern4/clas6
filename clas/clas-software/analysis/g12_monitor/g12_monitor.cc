/*************************************************************/
/*    g12_monitor: some useful plots from cooked bos files   */
/*************************************************************/
using namespace std;

extern "C" {
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <errno.h>
#include <malloc.h>

#include "ntypes.h"
#include "bostypes.h"
#include "utility.h"
#include "sc.h"
#include "scExtern.h"
#include "pid.h"
#include "kinematics.h"
}

#include <fstream>
#include <iostream>
#include <string>
#include <sstream>

#include "g12_monitor.h"

#define MPROTON2 0.8803505929
#define PI_CHARGED_MASS 0.13957

/* declare the bos common */
BOSbank bcs_;
BOSbank wcs_;
extern "C" {
  void bnames_(int *);
}

static char * tempbuf = NULL;
static char * prefix = NULL;
static char * outfile = NULL;

/*************************************************************/
/*                       GUI parts                           */
/*...........................................................*/

TCanvas * cc;
TPad * cc1;
TList * histList;
TList * histDescList;
TApplication * theApp;
MyMainFrame * listWindow;

/*************************************************************/
/*                    GUI constructor                        */
/*...........................................................*/

MyMainFrame::MyMainFrame(const TGWindow * p, UInt_t w, UInt_t h) :
   TGMainFrame(p, w, h)
{
  // Create main frame
  fListBox = new TGListBox(this, 150);
  for(int i=0; i<histDescList->GetSize(); i++)
    fListBox->AddEntry(histDescList->At(i)->GetName(), i+1);
  fListBox->Resize(400, 400);
  AddFrame(fListBox, new TGLayoutHints(kLHintsTop | kLHintsLeft | kLHintsExpandX | kLHintsExpandY, 5, 5, 5, 5));
  fListBox->Associate(this);
  fListBox->Connect("SelectionChanged()", "MyMainFrame", this, "DrawSelected()");
  fListBox->GetContainer()->Connect("ReturnPressed(TGFrame *)", "MyMainFrame", this, "DrawSelected()");
//***  fListBox->Connect("Selected(Int_t)", "MyMainFrame", this, "DrawSelected()");

  // Create a horizontal frame containing button(s)
  TGHorizontalFrame * hframe = new TGHorizontalFrame(this, 400, 20, kFixedWidth);
  TGTextButton * pr_one = new TGTextButton(hframe, "Print &One", 151);
  pr_one->Associate(this);
  sprintf(tempbuf, "Print this plot into %s_#.eps", prefix);
  pr_one->SetToolTipText(tempbuf);
  hframe->AddFrame(pr_one, new TGLayoutHints(kLHintsExpandX, 5, 5, 3, 4));
  TGTextButton * pr_all = new TGTextButton(hframe, "Print &All", 152);
  pr_all->Associate(this);
  sprintf(tempbuf, "Print all plots into %s_all.ps", prefix);
  pr_all->SetToolTipText(tempbuf);
  hframe->AddFrame(pr_all, new TGLayoutHints(kLHintsExpandX, 5, 5, 3, 4));
  TGTextButton * exit = new TGTextButton(hframe, "&Exit ", 153);
  exit->Associate(this);
  hframe->AddFrame(exit, new TGLayoutHints(kLHintsExpandX, 5, 5, 3, 4));
  AddFrame(hframe, new TGLayoutHints(kLHintsExpandX, 2, 2, 5, 1));

  // Set a name to the main frame
  SetWindowName("g12 Physics Monitoring Histograms");
  MapSubwindows();

  // Initialize the layout algorithm via Resize()
  Resize(GetDefaultSize());

  // Map main frame
  MapWindow();
  fListBox->Select(1);
}

MyMainFrame::~MyMainFrame()
{
  // Clean up main frame...
  delete fListBox;
}

/*************************************************************/
/*                     GUI functions                         */
/*...........................................................*/

Bool_t MyMainFrame::ProcessMessage(Long_t msg, Long_t parm1, Long_t)
{
//*** fprintf(stderr, "MSG %i  SUBMSG %i  parm %i\n", GET_MSG(msg), GET_SUBMSG(msg), parm1);
  switch (GET_MSG(msg)) {
  case kC_COMMAND:
    switch (GET_SUBMSG(msg)) {
    case kCM_BUTTON:
      switch(parm1) {
      case 151:
        PrintSelected();
        break;
      case 152:
        PrintAll();
        break;
      case 153:
        CloseWindow();
        break;
      default:
        fprintf(stderr, "ERROR: unknown parm1\n");	  
        break;
      }
      break;
    case kCM_LISTBOX:
      switch(parm1) {
      case 150:
        DrawSelected();
        break;
      default:
        fprintf(stderr, "ERROR: unknown parm1\n");	  
        break;
      }
      break;
    default:
      fprintf(stderr, "ERROR: unknown SUBMSG\n");
      break;
    }
    break;
  default:
    fprintf(stderr, "ERROR: unknown MSG\n");
    break;
  }
  return kTRUE;
}

void MyMainFrame::CloseWindow()
{
  gApplication->Terminate(0);
}

void MyMainFrame::DrawSpecific(int which)
{ 
  cc1->cd(1);
  if(which > 0 && which <= histList->GetSize())
    histList->At(which-1)->Draw();
  else {
    cc1->Clear();
    fprintf(stderr, "ERROR: DrawSelected(): Never should be here\n");
  }
  fListBox->Select(which);
  cc1->cd();
  cc1->Draw();
  cc1->Update(); 
}

void MyMainFrame::DrawSelected()
{
  DrawSpecific(fListBox->GetSelected());
}
 
void MyMainFrame::PrintSelected()
{
  sprintf(tempbuf, "%s_%s.eps", prefix, histDescList->At(fListBox->GetSelected()-1)->GetName());  
  while(char * s = strchr(tempbuf, ' '))
    *s = '_';
  cc1->Update();
  cc1->Print(tempbuf, "eps");
}

void MyMainFrame::PrintAll()
{
  int isel = fListBox->GetSelected();
  sprintf(tempbuf, "%s_all.ps[", prefix);
  cc1->Print(tempbuf);
  sprintf(tempbuf, "%s_all.ps", prefix);
  for(int i=0; i<histList->GetSize(); i++) {
    listWindow->DrawSpecific(i+1);
    cc1->Print(tempbuf);
  }
  sprintf(tempbuf, "%s_all.ps]", prefix);
  cc1->Print(tempbuf);
  DrawSpecific(isel);
}


/*************************************************************/
/*                  ROOT file functions                      */
/*...........................................................*/

static const char * skey1 = "Histograms";
static const char * skey2 = "Descriptions";

void SaveAll(void)
{
  if(outfile)
    sprintf(tempbuf, "%s", outfile);
  else
    sprintf(tempbuf, "%s_all.g12.root", prefix);  
  TFile f(tempbuf, "recreate");
  if(f.IsZombie()) {
    fprintf(stderr, "ERROR creating output root file %s\n", tempbuf);
    fprintf(stderr, "Check write permissions, free space, disk quota, etc.\n");
    return;
  }
  TObjString s(prefix);
  s.Write();
  f.WriteObject((const TList *)histList, skey1);
  f.WriteObject((const TList *)histDescList, skey2);
  f.Close();
  fprintf(stderr, "Output root file %s was saved\n", tempbuf);
}

void LoadAll(char * in)
{
  TFile * f = new TFile(in);
  
  if(f->IsZombie()) {
    fprintf(stderr, "ERROR opening file %s as .g12.root input file\n", in);
    exit(-1);
  }
  f->GetObject(skey1, histList);
  f->GetObject(skey2, histDescList);
  
  if(!histList || !histDescList || histList->GetSize() < 1 || histDescList->GetSize() < 1 ||
     histList->GetSize() != histDescList->GetSize()) {
    fprintf(stderr, "ERROR: Input file %s is either not a .g12.root file, or it is corrupted\n", in);
    exit(-1);
  }
  fprintf(stderr, "Found %i histograms in %s\n", histList->GetSize(), in); 

  TIter next(f->GetListOfKeys());
  TKey *key;
  while((key=(TKey *)next())) {
    if(strcmp(key->GetClassName(), "TObjString") == 0) {
      strcpy(prefix, key->GetName());
      break;
    }
  }

  // Lesson from ROOT: if you don't want to make memory copies of objects from root file, then
  // open file on the heap instead of on the stack, and don't close it! - A.O.
  // f->Close();
}


/*************************************************************/
/*                  Histogram functions                      */
/*...........................................................*/

static int counter_ttagmultprob;
static double norm_factor = -1.;

void adnH1F(char * tag, char * desc, char * title, int nbins, double xmin, double xmax)
{
  TH1F * h;
  TObjString * s;
  char normtag[200];
  
  if(norm_factor > 0.) {
    sprintf(normtag, "norm_%s", tag);
    sprintf(tempbuf, "NORM: %s", title);
    h = new TH1F(normtag, tempbuf, nbins, xmin, xmax);
  }
  else
    h = new TH1F(tag, title, nbins, xmin, xmax);
  s = new TObjString(desc);
  histList->Add(h);
  histDescList->Add(s);  
}

void addH1F(char * tag, char * desc, char * title, int nbins, double xmin, double xmax)
{
  TH1F * h;
  TObjString * s;
  
  h = new TH1F(tag, title, nbins, xmin, xmax);
  s = new TObjString(desc);
  histList->Add(h);
  histDescList->Add(s);  
}

void addH2F(char * tag, char * desc, char * title, int xnbins, double xmin, double xmax, int ynbins, double ymin, double ymax)
{
  TH2F * h;
  TObjString * s;
  
  h = new TH2F(tag, title, xnbins, xmin, xmax, ynbins, ymin, ymax);
  s = new TObjString(desc);
  histList->Add(h);
  histDescList->Add(s);  
}

void fillH1F(char * tag, double value)
{
  TH1F * h = NULL;

  if(norm_factor > 0.) {
    sprintf(tempbuf, "norm_%s", tag);
    h = (TH1F *)histList->FindObject(tempbuf);
  }
  if(!h)
    h = (TH1F *)histList->FindObject(tag);
  if(h)
    h->Fill(value);
  else
    fprintf(stderr, "ERROR: No such TH1F %s\n", tag);
}

void fillH2F(char * tag, double xvalue, double yvalue)
{
  TH2F * h = NULL;
  
  h = (TH2F *)histList->FindObject(tag);
  if(h)
    h->Fill(xvalue, yvalue);
  else
    fprintf(stderr, "ERROR: No such TH2F %s\n", tag);
}

void plotHistos(void)
{
  cc = new TCanvas("g12 monitor", "Hist", 425, 0, 600, 600);
  cc1 = new TPad("cc1", "hists", 0, 0, 1, 1);
  cc1->Draw();
  cc1->Divide(1, 1, 0.005, 0.005);
  cc1->cd(1);
  histList->At(0)->Draw();
  cc1->cd(); 
}

void histRatio(char * nom, char * denom, char * dest)
{
  TH1F * h1 = NULL;
  TH1F * h2 = NULL;
  TH1F * h3 = NULL;
  char name[200];

  if(norm_factor > 0.) {
    sprintf(tempbuf, "norm_%s", nom);
    h1 = (TH1F *)histList->FindObject(tempbuf);
    sprintf(tempbuf, "norm_%s", denom);
    h2 = (TH1F *)histList->FindObject(tempbuf);
    sprintf(tempbuf, "norm_%s", dest);
    h3 = (TH1F *)histList->FindObject(tempbuf); 
  }
  if(!h1)
    h1 = (TH1F *)histList->FindObject(nom);
  if(!h2)
    h2 = (TH1F *)histList->FindObject(denom);
  if(!h3)
    h3 = (TH1F *)histList->FindObject(dest);
  if(!h1 || !h2 || !h3) {
    fprintf(stderr, "ERROR: missing hist in ratio %s / %s = %s\n", nom, denom, dest);
    return;
  }
  strncpy(name, h3->GetName(), 199);
  strncpy(tempbuf, h3->GetTitle(), 1999);
  *h3 = *h1 / *h2;
  h3->SetNameTitle(name, tempbuf);
//***  h3->SetOption("E1");
}

void postProcessHistos(void)
{
  // Do postprocessing (normalization, ratio, etc.) after histos are filled
  TH1F * h;

  h = (TH1F *)histList->FindObject("ttagmultprob");
  if(h && (counter_ttagmultprob > 0))
    h->Scale(100./counter_ttagmultprob);
  histRatio("momentum_proton", "momentum_pip", "ratio_momentum_p_pip");
  histRatio("momentum_pim", "momentum_pip", "ratio_momentum_pim_pip");
  histRatio("proton_ebeam", "pip_ebeam", "ratio_ebeam_p_pip");
  histRatio("pim_ebeam", "pip_ebeam", "ratio_ebeam_pim_pip");
  histRatio("kp_ebeam", "pip_ebeam", "ratio_ebeam_kp_pip");
  histRatio("km_ebeam", "pim_ebeam", "ratio_ebeam_km_pim");
  
  histRatio("n_ebeam_pippim", "n_ebeam_ppippim", "ratio_pippim_ppippim");
  histRatio("n_ebeam_ppip", "n_ebeam_ppippim", "ratio_ppip_ppippim");
  histRatio("n_ebeam_ppim", "n_ebeam_ppippim", "ratio_ppim_ppippim");
  histRatio("n_ebeam_2pippim", "n_ebeam_ppippim", "ratio_2pippim_ppippim");
 
  if(norm_factor > 0.) { 
    for(int i=0; i<histList->GetSize(); i++) {
      h = (TH1F *)histList->At(i);
      if(!strncmp(h->GetName(), "norm_", 5))
        h->Scale(1./norm_factor);
    }
  }
}

void initHistos(void)
{
  histList = new TList();
  histDescList = new TList();
  TH2F * h;

  adnH1F("ttag", "Time of all tagger hits", "All t_{Tag}, ns", 100, -80., 160.);
  adnH1F("ttagmult", "Time difference between multiple tagger hits", "#Delta(t_{Tag}) for multiple TAG hits, ns", 150, 0., 300.);
  counter_ttagmultprob = 0;
  addH1F("ttagmultprob", "Probability for a TAG photon to have a neighbour per 2ns", "Probability of multiple TAG photons, \% per 2ns", 30, 0., 60.);
  adnH1F("tst1", "Time of all ST hits", "All t_{ST}, ns", 100, -20., 80.);
  adnH1F("ttag_tst", "Time difference between tagger and ST hits", "t_{Tag} - t_{ST}, ns", 100, -40., 40.);
  adnH1F("ttag_tst_intime", "Time difference between tagger and ST hits with t_tag, st cuts", "t_{Tag} - t_{ST} with t_{Tag} and t_{ST} cuts, ns", 100, -40., 40.);

  adnH1F("e_goodbeam", "Energy of good beam photons", "E_{beam}, in-time", 100, 0., 6.);
  adnH1F("e_badbeam", "Energy of out-of-time beam photons", "E_{beam}, out-of-time", 100, 0., 6.);
  adnH1F("n_goodbeam", "Number of good beam candidates per event", "Good beam candidates per event", 10, -0.5, 9.5);
  adnH1F("n_goodbeam_high", "Number of good high-E beam candidates per event", "Good beam candidates per event, high E_{beam}", 10, -0.5, 9.5);
  adnH1F("n_charged", "Number of charged tracks per event", "Charged tracks per event", 10, -0.5, 9.5);
  adnH1F("ncharged_hbtr", "Number of HBTR charged tracks per event", "Hit-based charged tracks per event", 11, -1.5, 9.5);
  adnH1F("ncharged_tbtr", "Number of TBTR charged tracks per event", "Time-based charged tracks per event", 11, -1.5, 9.5);
  adnH1F("ncharged_htdiff", "Difference of HBTR and TBTR tracks per event", "HBTR-TBTR charged tracks per event", 19, -9.5, 9.5);
  addH2F("hbtr_vs_tbtr", "HBTR tracks vs TBTR tracks", "HBTR tracks vs TBTR tracks", 11, -1.5, 9.5, 11, -1.5, 9.5);
  h = (TH2F *)histList->FindObject("hbtr_vs_tbtr");
  h->SetOption("BOX");

  adnH1F("vtime_stvtime", "Difference of vtime and stvtime in TBID", "TBID: vtime-ST_{vtime}", 100, -10., 10.);
  addH2F("mukesh1", "Tagger ID versus Ttag - Tpho", "TAG_{ID} vs. t_{tag}-t_{pho}", 121, 0.5, 121.5, 200, -1., 1.);
  addH2F("mukesh2", "Tagger ID versus Ttag - STvtime", "TAG_{ID} vs. t_{tag}-ST_{vtime}", 121, 0.5, 121.5, 1000, -10., 10.);
  addH2F("mukesh3", "ST ID versus vtime - STvtime", "ST_{ID} vs. vtime-ST_{vtime}", 24, 0.5, 24.5, 2000, -10., 10.);

  for(int i=1; i<=6; i++) {
    char tag[100];
    sprintf(tag,"craig_adcl_%i",i);
    sprintf(tempbuf,"TOF occupancy: ADC left, sector %i",i);
    addH1F(tag,tempbuf,tempbuf,57,0.5,57.5);
    sprintf(tag,"craig_adcr_%i",i);
    sprintf(tempbuf,"TOF occupancy: ADC right, sector %i",i);
    addH1F(tag,tempbuf,tempbuf,57,0.5,57.5);
    sprintf(tag,"craig_tdcl_%i",i);
    sprintf(tempbuf,"TOF occupancy: TDC left, sector %i",i);
    addH1F(tag,tempbuf,tempbuf,57,0.5,57.5);
    sprintf(tag,"craig_tdcr_%i",i);
    sprintf(tempbuf,"TOF occupancy: TDC right, sector %i",i);
    addH1F(tag,tempbuf,tempbuf,57,0.5,57.5);
  }
  for(int i=1; i<=6; i++) {
    char tag[100];
    sprintf(tag,"craig_p2p_%i",i);
    sprintf(tempbuf,"TOF paddle to paddle, sector %i",i);
    addH2F(tag,tempbuf,tempbuf,57,0.5,57.5, 400,-5.,5.);
  }
  addH2F("craig_dt_vs_id","Pion vtime - SC vtime vs SC id","TOF: Pion_{vtime} - SC_{vtime} vs SC_{id}",342, 0, 341, 2000, -10, 10);

  for(int i=1; i<=6; i++) {
    char tag[100];
    sprintf(tag,"ec_vtime_%i",i);
    sprintf(tempbuf,"vtime - EC_vtime for EC sector %i",i);
    addH1F(tag,tempbuf,tempbuf,100,-10.,10.);
  }
  addH1F("ec_beta", "Beta for EC photons", "#beta_{#gamma} for EC photons", 100, 0.9, 1.1);
  
  addH2F("lab_angles_pos_hbtr", "Lab angles for positive tracks from HBTR", "#theta_{lab} vs #phi_{lab} for positive tracks from HBTR", 90, 0., 90., 100, -180., 180.);
  addH2F("lab_angles_pos", "Lab angles for positive tracks from PART", "#theta_{lab} vs #phi_{lab} for positive tracks from PART", 90, 0., 90., 100, -180., 180.);
  addH2F("lab_angles_neg_hbtr", "Lab angles for negative tracks from HBTR", "#theta_{lab} vs #phi_{lab} for negative tracks from HBTR", 90, 0., 90., 100, -180., 180.);
  addH2F("lab_angles_neg", "Lab angles for negative tracks from PART", "#theta_{lab} vs #phi_{lab} for negative tracks from PART", 90, 0., 90., 100, -180., 180.);

//***  addH2F("vertex_xy_hbtr", "Vertex X-Y from HBTR", "X_{vertex} vs Y_{vertex} from HBTR", 100, -5., 5., 100, -5., 5.);
//***  addH2F("vertex_xy_part", "Vertex X-Y from PART", "X_{vertex} vs Y_{vertex} from PART", 100, -5., 5., 100, -5., 5.);
  addH2F("vertex_xy_mvrt", "Vertex X-Y from MVRT", "X_{vertex} vs Y_{vertex} from MVRT", 100, -5., 5., 100, -5., 5.);
  addH1F("vertex_z_hbtr", "Vertex Z from HBTR", "Z_{vertex} from HBTR", 100, -140., -40.);
  addH1F("vertex_z_hbtr_small", "Vertex Z from HBTR, small angle", "Z_{vertex} from HBTR, #theta < 20^{o}", 100, -140., -40.);
  addH1F("vertex_z_hbtr_large", "Vertex Z from HBTR, large angle", "Z_{vertex} from HBTR, #theta > 20^{o}", 100, -140., -40.);
  addH1F("vertex_z_part", "Vertex Z from PART", "Z_{vertex} from PART", 100, -140., -40.);
  addH1F("vertex_z_mvrt", "Vertex Z from MVRT", "Z_{vertex} from MVRT", 100, -140., -40.);
  
  addH2F("beta_hbid", "Momentum vs beta, hit-based", "Momentum vs. #beta (hit-based)", 100, 0., 6., 100, -0.2, 1.8);
  addH2F("beta_tbid", "Momentum vs beta, time-based", "Momentum vs. #beta (time-based)", 100, 0., 6., 100, -0.2, 1.8);
  h = (TH2F *)histList->FindObject("beta_hbid");
  h->SetOption("CONTZ");
  h = (TH2F *)histList->FindObject("beta_tbid");
  h->SetOption("CONTZ");
    
  adnH1F("momentum_proton", "Momentum of Proton", "Momentum of reconstructed protons", 100, 0., 4.);
  adnH1F("momentum_pip", "Momentum of Pi+", "Momentum of reconstructed #pi^{+}", 100, 0., 4.);
  adnH1F("momentum_pim", "Momentum of Pi-", "Momentum of reconstructed #pi^{-}", 100, 0., 4.);
  addH1F("ratio_momentum_p_pip", "Ratio of P/Pi+ yields at different momentum", "Ratio of p/#pi^{+} yields as function of momentum", 100, 0., 4.);
  addH1F("ratio_momentum_pim_pip", "Ratio of Pi-/Pi+ yields at different momentum", "Ratio of #pi^{-}/#pi^{+} yields as function of momentum", 100, 0., 4.);

  adnH1F("proton_ebeam", "Proton yield per beam energy", "Proton yield as function of E_{beam}", 100, 0., 6.);
  adnH1F("pip_ebeam", "Pi+ yield per beam energy", "#pi^{+} yield as function of E_{beam}", 100, 0., 6.);
  adnH1F("pim_ebeam", "Pi- yield per beam energy", "#pi^{-} yield as function of E_{beam}", 100, 0., 6.);
  adnH1F("kp_ebeam", "K+ yield per beam energy", "K^{+} yield as function of E_{beam}", 100, 0., 6.);
  adnH1F("km_ebeam", "K- yield per beam energy", "K^{-} yield as function of E_{beam}", 100, 0., 6.);
  addH1F("ratio_ebeam_p_pip", "Ratio of P/Pi+ yields at different beam energies", "Ratio of p/#pi^{+} yields as function of E_{beam}", 100, 0., 6.);
  addH1F("ratio_ebeam_pim_pip", "Ratio of Pi- to Pi+ yields at different beam energies", "Ratio of #pi^{-}/#pi^{+} yields as function of E_{beam}", 100, 0., 6.);
  addH1F("ratio_ebeam_kp_pip", "Ratio of K+ to Pi+ yields at different beam energies", "Ratio of K^{+}/#pi^{+} yields as function of E_{beam}", 100, 0., 6.);
  addH1F("ratio_ebeam_km_pim", "Ratio of K- to Pi- yields at different beam energies", "Ratio of K^{-}/#pi^{-} yields as function of E_{beam}", 100, 0., 6.);
  
  adnH1F("any_pipi_mass", "All events with Pi+ and Pi-: pi+pi- mass", "All high E_{beam} events: Mass of #pi^{+}#pi^{-}", 100, 0.2, 1.7);
  adnH1F("any_kk_mass", "All events with K+ and K-: K+K- mass", "All high E_{beam} events: Mass of K^{+}K^{-}", 100, 0.9, 1.9);

  adnH1F("single_pip_mm2", "Single Pi+: missing mass squared", "#gamma_{high} p #rightarrow #pi^{+}(n): MM^{2}", 100, -1., 5.);
  adnH1F("mass_2gamma", "Mass of 2 photons", "Mass of all photon pairs", 100, 0., 1.);
                  
  adnH1F("ppippim_pipi_mass", "P Pi+ Pi-: pi+pi- mass", "#gamma_{high} p #rightarrow #pi^{+}#pi^{-}p: Mass of #pi^{+}#pi^{-}", 100, 0.2, 1.7);
  adnH1F("ppippim_mm2", "P Pi+ Pi-: missing mass squared", "#gamma_{high} p #rightarrow #pi^{+}#pi^{-}p: MM^{2}", 100, -1., 2.);
  adnH1F("ppippim_pipi_mass_mmcut", "P Pi+ Pi-: pi+pi- mass with MM cut", "#gamma_{high} p #rightarrow #pi^{+}#pi^{-}p: Mass of #pi^{+}#pi^{-} with MM^{2} cut", 100, 0.2, 1.7);
  adnH1F("ppippim_ppip_mass_mmcut", "P Pi+ Pi-: p pi+ mass with MM cut", "#gamma_{high} p #rightarrow #pi^{+}#pi^{-}p: Mass of p#pi^{+} with MM^{2} cut", 100, 1.0, 3.0);
  adnH1F("ppippim_ppim_mass_mmcut", "P Pi+ Pi-: p pi- mass with MM cut", "#gamma_{high} p #rightarrow #pi^{+}#pi^{-}p: Mass of p#pi^{-} with MM^{2} cut", 100, 1.0, 3.0);

  adnH1F("pippim_pipi_mass", "Pi+ Pi-: pi+pi- mass", "#gamma_{high} p #rightarrow #pi^{+}#pi^{-}(p): Mass of #pi^{+}#pi^{-}", 100, 0.2, 1.7);
  adnH1F("pippim_mm2", "Pi+ Pi-: missing mass squared", "#gamma_{high} p #rightarrow #pi^{+}#pi^{-}(p): MM^{2}", 100, -1., 2.);
  adnH1F("pippim_pipi_mass_mmcut", "Pi+ Pi-: pi+pi- mass with MM cut", "#gamma_{high} p #rightarrow #pi^{+}#pi^{-}(p): Mass of #pi^{+}#pi^{-} with MM^{2} cut", 100, 0.2, 1.7);

  adnH1F("2pippim_pipipi_mass", "Pi+ Pi+ Pi-: 3pi mass", "#gamma_{high} p #rightarrow #pi^{+}#pi^{+}#pi^{-}(n): Mass of #pi^{+}#pi^{+}#pi^{-}", 100, 0.4, 2.4);
  adnH1F("2pippim_mm2", "Pi+ Pi+ Pi-: missing mass squared", "#gamma_{high} p #rightarrow #pi^{+}#pi^{+}#pi^{-}(n): MM^{2}", 100, -1., 5.);
  adnH1F("2pippim_mm", "Pi+ Pi+ Pi-: missing mass", "#gamma_{high} p #rightarrow #pi^{+}#pi^{+}#pi^{-}(n): MM", 100, 0., 2.5);
  addH2F("2pippim_mmpz_mmpt","Pi+ Pi+ Pi-: missing momentum Pz vs Pt","#gamma_{high} p #rightarrow #pi^{+}#pi^{+}#pi^{-}(n): missing momentum p_{z} vs p_{t}",100,-1.,5.,100,-0.5,1.5);
  adnH1F("2pippim_mm2_mmpt", "Pi+ Pi+ Pi-: missing mass squared with Pz,Pt cut", "#gamma_{high} p #rightarrow #pi^{+}#pi^{+}#pi^{-}(n): MM^{2} with MM p_{z},p_{t} cut", 100, -1., 5.);
  adnH1F("2pippim_mm_mmpt", "Pi+ Pi+ Pi-: missing mass with Pz,Pt cut", "#gamma_{high} p #rightarrow #pi^{+}#pi^{+}#pi^{-}(n): MM with MM p_{z},p_{t} cut", 100, 0., 2.5);
  adnH1F("2pippim_pipipi_mass_mmcut", "Pi+ Pi+ Pi-: 3pi mass with MM cut", "#gamma_{high} p #rightarrow #pi^{+}#pi^{+}#pi^{-}(n): Mass of #pi^{+}#pi^{+}#pi^{-} with MM^{2} cut", 100, 0.4, 2.4);
  adnH1F("2pippim_pipi_mass_mmcut", "Pi+ Pi+ Pi-: pi+pi- mass with MM cut", "#gamma_{high} p #rightarrow #pi^{+}#pi^{+}#pi^{-}(n): Mass of #pi^{+}#pi^{-} with MM^{2} cut", 100, 0.2, 1.7);
  adnH1F("2pippim_npip_mass_mmcut", "Pi+ Pi+ Pi-: n pi+ mass with MM cut", "#gamma_{high} p #rightarrow #pi^{+}#pi^{+}#pi^{-}(n): Mass of (n)#pi^{+} with MM^{2} cut", 100, 1.0, 3.0);
  adnH1F("2pippim_npim_mass_mmcut", "Pi+ Pi+ Pi-: n pi- mass with MM cut", "#gamma_{high} p #rightarrow #pi^{+}#pi^{+}#pi^{-}(n): Mass of (n)#pi^{-} with MM^{2} cut", 100, 1.0, 3.0);

  adnH1F("n_ebeam_ppippim", "Number of Proton Pi+ Pi- events per beam energy", "Number of #pi^{+}#pi^{-}p events as function of E_{beam}", 12, 0., 6.);
  adnH1F("n_ebeam_pippim", "Number of Pi+ Pi- events per beam energy", "Number of #pi^{+}#pi^{-} events as function of E_{beam}", 12, 0., 6.);
  adnH1F("n_ebeam_ppip", "Number of Proton Pi+ events per beam energy", "Number of #pi^{+}p events as function of E_{beam}", 12, 0., 6.0);
  adnH1F("n_ebeam_ppim", "Number of Proton Pi- events per beam energy", "Number of #pi^{-}p events as function of E_{beam}", 12, 0., 6.0);
  adnH1F("n_ebeam_2pippim", "Number of Pi+ Pi+ Pi- events per beam energy", "Number of #pi^{+}#pi^{+}#pi^{-} events as function of E_{beam}", 12, 0., 6.);

  addH1F("ratio_pippim_ppippim", "Ratio of Pi+Pi- to PPi+Pi- events per beam energy", "Ratio of #pi^{+}#pi^{-}/#pi^{+}#pi^{-}p events as function of E_{beam}", 12, 0., 6.);
  addH1F("ratio_ppip_ppippim", "Ratio of PPi+ to PPi+Pi- events per beam energy", "Ratio of p#pi^{+}/#pi^{+}#pi^{-}p events as function of E_{beam}", 12, 0., 6.);
  addH1F("ratio_ppim_ppippim", "Ratio of PPi- to PPi+Pi- events per beam energy", "Ratio of p#pi^{-}/#pi^{+}#pi^{-}p events as function of E_{beam}", 12, 0., 6.);
  addH1F("ratio_2pippim_ppippim", "Ratio of Pi+Pi+Pi- to PPi+Pi- events per beam energy", "Ratio of #pi^{+}#pi^{+}#pi^{-}/#pi^{+}#pi^{-}p events as function of E_{beam}", 12, 0., 6.);
}


/*************************************************************/
/*                    Event processing                       */
/*...........................................................*/

TLorentzVector Tgt;
TLorentzVector Beam;
TLorentzVector proton[10];
TLorentzVector piplus[10];
TLorentzVector piminus[10];
TLorentzVector kplus[10];
TLorentzVector kminus[10];
TLorentzVector M;
int npos, nneg, nneut, nprot, npip, npim, nkp, nkm;
double beamE[20];
int nbeam;
double beamcut;

// Newest g12 timing windows per 04/02/2009 calibrations
#define TPHO_MIN 0.
#define TPHO_MAX 35.
#define ST1_MIN 5.
#define ST1_MAX 40.
#define DELTA_TPHO_TST_MIN -10.
#define DELTA_TPHO_TST_MAX 5.
#define DEFAULT_BEAM_ENERGY_CUT 4.4

// g12 timing windows per 09/10/2008 calibrations
//#define TPHO_MIN 0.
//#define TPHO_MAX 25.
//#define ST1_MIN 5.
//#define ST1_MAX 25.
//#define DELTA_TPHO_TST_MIN -10.
//#define DELTA_TPHO_TST_MAX 5.
//#define DEFAULT_BEAM_ENERGY_CUT 4.4

//g12 timing windows
//#define TPHO_MIN 8.
//#define TPHO_MAX 32.
//#define ST1_MIN 10.
//#define ST1_MAX 40.
//#define DELTA_TPHO_TST_MIN -10.
//#define DELTA_TPHO_TST_MAX 5.
//#define DEFAULT_BEAM_ENERGY_CUT 4.4

//g6c & g9 timing windows
//#define TPHO_MIN -10.
//#define TPHO_MAX 40.
//#define ST1_MIN -10.
//#define ST1_MAX 40.
//#define DELTA_TPHO_TST_MIN -20.
//#define DELTA_TPHO_TST_MAX 10.

void ProcessEvent(void)
{
  clasTAGR_t * tagr;
  clasST1_t  * st1;
  clasSTR_t  * str;
  clasPART_t * part;
  clasHBID_t * hbid;
  clasTBID_t * tbid;
  clasHBTR_t * hbtr;
  clasTBTR_t * tbtr;
  clasMVRT_t * mvrt;
  clasHEVT_t * hevt;
  clasSC_t   * sc[6];
  clasSCRC_t * scrc;
  
  nbeam = npos = nneg = nneut = nprot = npip = npim = nkp = nkm = 0;
  part = (clasPART_t *)getGroup(&bcs_, "PART", 1);
  if(!part)
    part = (clasPART_t *)getGroup(&bcs_, "PART", 0);
  tagr = (clasTAGR_t *)getGroup(&bcs_, "TAGR", 1);
  if(!tagr)
    tagr = (clasTAGR_t *)getGroup(&bcs_, "TAGR", 0);
  st1  = (clasST1_t  *)getGroup(&bcs_, "ST1 ", 1);
  if(!st1)
    st1  = (clasST1_t  *)getGroup(&bcs_, "ST1 ", 0);
  str = (clasSTR_t *)getGroup(&bcs_, "STR ", 1);
  if(!str)
    str = (clasSTR_t *)getGroup(&bcs_, "STR ", 0);
  hbid = (clasHBID_t *)getGroup(&bcs_, "HBID", 1);
  if(!hbid)
    hbid = (clasHBID_t *)getGroup(&bcs_, "HBID", 0);
  tbid = (clasTBID_t *)getGroup(&bcs_, "TBID", 1);
  if(!tbid)
    tbid = (clasTBID_t *)getGroup(&bcs_, "TBID", 0);
  hbtr = (clasHBTR_t *)getGroup(&bcs_, "HBTR", 1);
  if(!hbtr)
    hbtr = (clasHBTR_t *)getGroup(&bcs_, "HBTR", 0);
  tbtr = (clasTBTR_t *)getGroup(&bcs_, "TBTR", 1);
  if(!tbtr)
    tbtr = (clasTBTR_t *)getGroup(&bcs_, "TBTR", 0);
  mvrt = (clasMVRT_t *)getGroup(&bcs_, "MVRT", 1);
  if(!mvrt)
    mvrt = (clasMVRT_t *)getGroup(&bcs_, "MVRT", 0);
  hevt = (clasHEVT_t *)getGroup(&bcs_, "HEVT", 1);
  if(!hevt)
    hevt = (clasHEVT_t *)getGroup(&bcs_, "HEVT", 0);
  for(int i=1; i<=6; i++)
    sc[i] = (clasSC_t *)getGroup(&bcs_,"SC  ",i); 

  if(tagr) {
    for(int i=0; i<tagr->bank.nrow; i++) {    
      if(tagr->tagr[i].stat != 7 && tagr->tagr[i].stat != 15)
        continue;  // not a reconstructed beam photon
      fillH1F("ttag", tagr->tagr[i].tpho);
      if(tagr->tagr[i].tpho < TPHO_MIN || tagr->tagr[i].tpho > TPHO_MAX)
        fillH1F("e_badbeam", tagr->tagr[i].erg); // out-of-time beam photons	

      float closest_photon = 1000.;
      for(int j=i+1; j<tagr->bank.nrow; j++) {
	if(tagr->tagr[j].stat == 7 || tagr->tagr[j].stat == 15) {
          float val = fabs(tagr->tagr[i].tpho - tagr->tagr[j].tpho);
	  fillH1F("ttagmult", val);
	  if(val < closest_photon)
	    closest_photon = val;
        }
      }
      counter_ttagmultprob++;         // for multi-photon probability normalization...
      while(closest_photon < 60.) {   // all bins from closest_photon to x-axis max(=60.) need to be increased
        fillH1F("ttagmultprob", closest_photon);
	closest_photon += 2.0;        // Hist is with 2ns per bin
      }

      if(st1) {
        int inbeamarray = 0;
        for(int j=0; j<st1->bank.nrow; j++) {
          float val = tagr->tagr[i].tpho - st1->st1[j].time_1;
          fillH1F("ttag_tst", val);
          if((tagr->tagr[i].tpho >= TPHO_MIN && tagr->tagr[i].tpho <= TPHO_MAX) &&
	      (st1->st1[j].time_1 >= ST1_MIN && st1->st1[j].time_1 <= ST1_MAX)) {
            fillH1F("ttag_tst_intime", val);
	    if(val >= DELTA_TPHO_TST_MIN && val <= DELTA_TPHO_TST_MAX && inbeamarray == 0) {
              fillH1F("e_goodbeam", tagr->tagr[i].erg);
 	      beamE[nbeam] =  tagr->tagr[i].erg;   // fill array of good beam photons for MM^2 use later
	      if(nbeam < 19)
	        nbeam++;
              inbeamarray = 1;
            }
          }
        }
      }
    }
  }

  fillH1F("n_goodbeam", nbeam);  

  int highbeam = 0;
  for(int j=0; j<nbeam; j++)
    if(beamE[j] >= beamcut)
      highbeam++;
  fillH1F("n_goodbeam_high", highbeam);  
  

  if(st1) {
    for(int i=0; i<st1->bank.nrow; i++) {
      fillH1F("tst1", st1->st1[i].time_1);
    }
  }

  if(tbid && tagr && str) {
    for(int j=0; j<tagr->bank.nrow; j++)  {
      fillH2F("mukesh1", tagr->tagr[j].t_id, tagr->tagr[j].ttag - tagr->tagr[j].tpho);
    }

    for(int i=0; i<tbid->bank.nrow; i++)  {
      int stsec, stpid, stid, strid, stnid;
      stid = tbid->tbid[i].st_id;
      if(stid > 0 && stid <= str->bank.nrow) {
        fillH1F("vtime_stvtime", tbid->tbid[i].vtime - tbid->tbid[i].st_vtime);
        for(int j=0; j<tagr->bank.nrow; j++)
          fillH2F("mukesh2", tagr->tagr[j].t_id, tagr->tagr[j].ttag - tbid->tbid[i].st_vtime);
	strid=str->str[stid-1].id;
        stsec=strid/100;
        stpid=(strid-100*stsec)/10;
        stnid=4*(stsec-1)+stpid;
        fillH2F("mukesh3", stnid, tbid->tbid[i].vtime - tbid->tbid[i].st_vtime);
      }
    }
  }

  if(tbid) {
    for(int i=0; i<tbid->bank.nrow; i++)  {
      if(tbid->tbid[i].ec_stat != 0 && fabs(tbid->tbid[i].ec_vtime) > 0.00001) {
        sprintf(tempbuf, "ec_vtime_%i",tbid->tbid[i].sec);
        fillH1F(tempbuf, tbid->tbid[i].vtime - tbid->tbid[i].ec_vtime);                   
      } 
      if(tbid->tbid[i].ec_beta > 0.5)
        fillH1F("ec_beta", tbid->tbid[i].ec_beta);       
    }              
  }
    
  for(int i=1; i<=6; i++) {
    if(sc[i]) {
      for(int j=0; j<sc[i]->bank.nrow; j++) {
        char tag[100];
        sprintf(tag,"craig_adcl_%i",i);
        float adcl = sc[i]->sc[j].adcl - sc_pedestals.left[sc_index(i,(sc[i]->sc[j].id)%0xFF)];
        if( (adcl > 0.) && (adcl < SC_ADC_MAX) )
          fillH1F(tag, (sc[i]->sc[j].id)%0xFF);
        sprintf(tag,"craig_adcr_%i",i);
        float adcr = sc[i]->sc[j].adcr - sc_pedestals.right[sc_index(i,(sc[i]->sc[j].id)%0xFF)];
        if( (adcr > 0.) && (adcr < SC_ADC_MAX) )
          fillH1F(tag, (sc[i]->sc[j].id)%0xFF);
        sprintf(tag,"craig_tdcl_%i",i);
        float tdcl = sc[i]->sc[j].tdcl;
        if( (tdcl > 0.) && (tdcl < SC_TDC_MAX) )
          fillH1F(tag, (sc[i]->sc[j].id)%0xFF);
        sprintf(tag,"craig_tdcr_%i",i);
        float tdcr = sc[i]->sc[j].tdcr;
        if( (tdcr > 0.) && (tdcr < SC_TDC_MAX) )
          fillH1F(tag, (sc[i]->sc[j].id)%0xFF);
      }
    }
  }

  if(tbid && tbtr && hevt && mvrt) {
    for(int i=0; i<tbid->bank.nrow; i++) {
      // Cut to use only pions, taken from sc_mon
      float dEdX = tbid2dedx((bid_t *)&tbid->tbid[i]);
      if(tbid->tbid[i].beta < 0)
        continue;
      if( dEdX < 4 || dEdX > 13.5)
        continue;
      int nSector = tbid->tbid[i].sec;
      scrc = (clasSCRC_t *)getGroup(&bcs_, "SCRC", nSector);
      if(!scrc)
        continue;
      int j = tbid->tbid[i].sc_id - 1;
      if(j < 0 || j >= scrc->bank.nrow)
        continue;
      int nPaddle = scrc->scrc[j].id;
      // Another cut to ensure pions, taken straight from sc_mon
      j = tbid->tbid[i].track-1;
      if(j < 0 || j >= tbtr->bank.nrow)
        continue;
      float p = v3mag(tbtr->tbtr[j].p);
      if(p < 0.3 || p > 0.735)
        continue;
      float pion_beta = p / sqrt(p*p + pow(PI_CHARGED_MASS,2));
      float vtx_t = tbid->tbid[i].sc_time - 1e9*pathlen2sc((bid_t *)&tbid->tbid[i],
                    (hdpl_t*)tbtr2tdpl(&tbtr->tbtr[tbid->tbid[i].track-1]))/(SPEED_OF_LIGHT*pion_beta);
      if(mvrt->bank.nrow <= 0)
        continue;
      float zVertexPosition = mvrt->mvrt[0].vert.z;
      float zTargetPosition = -90.; // hardcoded for now
      float start_t = hevt->hevt[0].stt + (zVertexPosition - zTargetPosition) / SPEED_OF_LIGHT;
      double dt = vtx_t - start_t;
      char tag[100];
      sprintf(tag,"craig_p2p_%i",nSector);
      fillH2F(tag, nPaddle, dt);
    }
  }

  if(tbid && tbtr) {
    for(int i=0; i<tbid->bank.nrow; i++) {
      if(tbid->tbid[i].track == 0)
        continue;
      float dEdX = tbid2dedx((bid_t *)&tbid->tbid[i]);
      if(dEdX < 3 || dEdX > 15)
        continue;
      float p = v3mag(tbtr->tbtr[tbid->tbid[i].track-1].p);
      if(tbtr->tbtr[tbid->tbid[i].track].q != -1)  
        if(p > 0.735)
          continue;
      scrc = (clasSCRC_t *)getGroup(&bcs_, "SCRC", tbid->tbid[i].sec);
      if(!scrc)
        continue;
      // sc paddle id 0 to 341. 
      int nGlobalIndex = sc_index(tbid->tbid[i].sec,(scrc->scrc[tbid->tbid[i].sc_id - 1].id)%0xFF);
      float pion_beta = p / sqrt(p*p + pow(PI_CHARGED_MASS,2));
      float pion_vtx_t = tbid->tbid[i].sc_time - 1e9*pathlen2sc((bid_t *)&tbid->tbid[i],
                    (hdpl_t*)tbtr2tdpl(&tbtr->tbtr[tbid->tbid[i].track-1]))/(SPEED_OF_LIGHT*pion_beta);
      fillH2F("craig_dt_vs_id", nGlobalIndex, pion_vtx_t - tbid->tbid[i].vtime);
    }
  }

  if(hbtr) {
    for(int i=0; i<hbtr->bank.nrow; i++) {
      float momentum = sqrt(hbtr->hbtr[i].p.x*hbtr->hbtr[i].p.x +
                            hbtr->hbtr[i].p.y*hbtr->hbtr[i].p.y +
			    hbtr->hbtr[i].p.z*hbtr->hbtr[i].p.z);
      if(hbtr->hbtr[i].q != 0) {
//***        fillH2F("vertex_xy_hbtr", hbtr->hbtr[i].vert.x, hbtr->hbtr[i].vert.y);
        fillH1F("vertex_z_hbtr", hbtr->hbtr[i].vert.z); 
        float ang_theta =  180.*acos(hbtr->hbtr[i].p.z/momentum)/M_PI;
        float ang_phi = 180.*atan2(hbtr->hbtr[i].p.y, hbtr->hbtr[i].p.x)/M_PI; 
        if(ang_theta < 20)
          fillH1F("vertex_z_hbtr_small", hbtr->hbtr[i].vert.z);
        else 
          fillH1F("vertex_z_hbtr_large", hbtr->hbtr[i].vert.z);
        if(hbtr->hbtr[i].q > 0)
          fillH2F("lab_angles_pos_hbtr", ang_theta, ang_phi);      
        else
          fillH2F("lab_angles_neg_hbtr", ang_theta, ang_phi);      
      }          
    }        
  }
    
  
  if(hbid && hbtr) {
    for(int i=0; i<hbid->bank.nrow; i++) {
      if(hbid->hbid[i].vtime > -1000.) {
        if(hbid->hbid[i].track <= hbtr->bank.nrow) {
	   int j = hbid->hbid[i].track - 1;
           float momentum = sqrt(hbtr->hbtr[j].p.x*hbtr->hbtr[j].p.x + 
	                         hbtr->hbtr[j].p.y*hbtr->hbtr[j].p.y +
				 hbtr->hbtr[j].p.z*hbtr->hbtr[j].p.z);
           fillH2F("beta_hbid", momentum, hbid->hbid[i].beta);
        }
      }
    }
  }

  if(tbid && tbtr) {
    for(int i=0; i<tbid->bank.nrow; i++) {
      if(tbid->tbid[i].vtime > -1000.) {
        if(tbid->tbid[i].track <= tbtr->bank.nrow) {
	   int j = tbid->tbid[i].track - 1;
           float momentum = sqrt(tbtr->tbtr[j].p.x*tbtr->tbtr[j].p.x + 
	                         tbtr->tbtr[j].p.y*tbtr->tbtr[j].p.y +
				 tbtr->tbtr[j].p.z*tbtr->tbtr[j].p.z);
           fillH2F("beta_tbid", momentum, tbid->tbid[i].beta);
        }
      }
    }
  }

  int ncharged_hbtr = -1;
  int ncharged_tbtr = -1;
  if(hbtr) {
    ncharged_hbtr = 0;
    for(int i=0; i<hbtr->bank.nrow; i++)
      if(hbtr->hbtr[i].q != 0)
        ncharged_hbtr++;
  }
  if(tbtr) {
    ncharged_tbtr = 0;
    for(int i=0; i<tbtr->bank.nrow; i++)
      if(tbtr->tbtr[i].q != 0)
        ncharged_tbtr++;
  }
  fillH1F("ncharged_hbtr", ncharged_hbtr);
  fillH1F("ncharged_tbtr", ncharged_tbtr);
  if(ncharged_hbtr >= 0 || ncharged_tbtr >= 0) {
    int ndiff;
    if(ncharged_hbtr < 0)
      ndiff = 0 - ncharged_tbtr;
    else if(ncharged_tbtr < 0)
      ndiff = ncharged_hbtr - 0;
    else
      ndiff = ncharged_hbtr - ncharged_tbtr;
    fillH1F("ncharged_htdiff",ndiff);
  }
  fillH2F("hbtr_vs_tbtr", ncharged_hbtr, ncharged_tbtr);
      
  if(mvrt) {
    for(int i=0; i<mvrt->bank.nrow; i++) {
      if(mvrt->mvrt[i].ntrk > 0) {
        fillH2F("vertex_xy_mvrt", mvrt->mvrt[i].vert.x, mvrt->mvrt[i].vert.y);
        fillH1F("vertex_z_mvrt", mvrt->mvrt[i].vert.z);                  
      }
    }        
  }

  if(part) {
    for(int i=0; i<part->bank.nrow; i++) {
      float momentum = sqrt(part->part[i].p.space.x*part->part[i].p.space.x +
                            part->part[i].p.space.y*part->part[i].p.space.y +
			    part->part[i].p.space.z*part->part[i].p.space.z);

      if(part->part[i].q != 0) {
//***        fillH2F("vertex_xy_part", part->part[i].vert.x, part->part[i].vert.y);
        fillH1F("vertex_z_part", part->part[i].vert.z);                     
      }       
      if(part->part[i].q > 0) {
        npos++;
        fillH2F("lab_angles_pos", 180.*acos(part->part[i].p.space.z/momentum)/M_PI, 
	                         180.*atan2(part->part[i].p.space.y, part->part[i].p.space.x)/M_PI);      
      }
      else if(part->part[i].q < 0) {
        nneg++;
        fillH2F("lab_angles_neg", 180.*acos(part->part[i].p.space.z/momentum)/M_PI, 
	                         180.*atan2(part->part[i].p.space.y, part->part[i].p.space.x)/M_PI);      
      }
      else
        nneut++;

      if(part->part[i].pid == 1) {
        for(int j=i+1; j<part->bank.nrow; j++) {
          if(part->part[j].pid == 1) {
             M.SetPx(part->part[i].p.space.x + part->part[j].p.space.x);  
             M.SetPy(part->part[i].p.space.y + part->part[j].p.space.y);  
             M.SetPz(part->part[i].p.space.z + part->part[j].p.space.z);  
             M.SetE(part->part[i].p.t + part->part[j].p.t);
             fillH1F("mass_2gamma",  M.Mag());
          }
        }  
      }
      
      if(part->part[i].pid == 14 && nprot < 10) {
        fillH1F("momentum_proton", momentum);
        for(int j=0; j<nbeam; j++)
	  fillH1F("proton_ebeam", beamE[j]);
        proton[nprot].SetPx(part->part[i].p.space.x);
        proton[nprot].SetPy(part->part[i].p.space.y);
        proton[nprot].SetPz(part->part[i].p.space.z);
        proton[nprot].SetE(part->part[i].p.t);
        nprot++;
      }	
      if(part->part[i].pid == 8 && npip < 10) {
        fillH1F("momentum_pip", momentum);
        for(int j=0; j<nbeam; j++)
	  fillH1F("pip_ebeam", beamE[j]);
        piplus[npip].SetPx(part->part[i].p.space.x);
        piplus[npip].SetPy(part->part[i].p.space.y);
        piplus[npip].SetPz(part->part[i].p.space.z);
        piplus[npip].SetE(part->part[i].p.t);
        npip++;
      }	
      if(part->part[i].pid == 9 && npim < 10) {
        fillH1F("momentum_pim", momentum);
        for(int j=0; j<nbeam; j++)
	  fillH1F("pim_ebeam", beamE[j]);
        piminus[npim].SetPx(part->part[i].p.space.x);
        piminus[npim].SetPy(part->part[i].p.space.y);
        piminus[npim].SetPz(part->part[i].p.space.z);
        piminus[npim].SetE(part->part[i].p.t);
	npim++;
      }
      if(part->part[i].pid == 11 && nkp < 10) {        
        for(int j=0; j<nbeam; j++)
	  fillH1F("kp_ebeam", beamE[j]);
        kplus[nkp].SetPx(part->part[i].p.space.x);
        kplus[nkp].SetPy(part->part[i].p.space.y);
        kplus[nkp].SetPz(part->part[i].p.space.z);
        kplus[nkp].SetE(part->part[i].p.t);
	nkp++;
      }
      if(part->part[i].pid == 12 && nkm < 10) {        
        for(int j=0; j<nbeam; j++)
	  fillH1F("km_ebeam", beamE[j]);
        kminus[nkm].SetPx(part->part[i].p.space.x);
        kminus[nkm].SetPy(part->part[i].p.space.y);
        kminus[nkm].SetPz(part->part[i].p.space.z);
        kminus[nkm].SetE(part->part[i].p.t);
	nkm++;
      }
    }
  }	

  fillH1F("n_charged", npos+nneg);

  // any event with pi+pi-
  if(highbeam > 0 && npip > 0 && npim > 0) {
    for(int j=0; j<npip; j++) {
      for(int k=0; k<npim; k++) {
        M = piplus[j] + piminus[k];
        fillH1F("any_pipi_mass", M.Mag());
      }    
    }
  }

  // any event with K+K-  
  if(highbeam > 0 && nkp > 0 && nkm > 0) {
    for(int j=0; j<nkp; j++) {
      for(int k=0; k<nkm; k++) {
        M = kplus[j] + kminus[k];
        fillH1F("any_kk_mass", M.Mag());
      }    
    }
  }

  // gamma p -> pi+ (n)  
  if(npos == 1 && nneg == 0 && npip == 1) {
    for(int j=0; j<nbeam; j++) {
      if(beamE[j] >= beamcut) {
        Beam.SetPz(beamE[j]);
        Beam.SetE(beamE[j]);
        M = Beam + Tgt - piplus[0];
        fillH1F("single_pip_mm2", M.Mag2());
      }   
    }
  }  
  	
  // gamma p -> p pi+ pi-
  if(npos == 2 && nneg == 1 && nprot == 1 && npip == 1 && npim == 1) {
    float bestMM2 = 1000.;
    for(int j=0; j<nbeam; j++) {
      fillH1F("n_ebeam_ppippim", beamE[j]);
      if(beamE[j] >= beamcut) {
        Beam.SetPz(beamE[j]);
        Beam.SetE(beamE[j]);
        M = Beam + Tgt - proton[0] - piplus[0] - piminus[0];
        if(fabs(M.Mag2()) < fabs(bestMM2))
          bestMM2 = M.Mag2();
      }
    }
    if(fabs(bestMM2) < 99.) {
      fillH1F("ppippim_mm2", bestMM2);                  
      M = piplus[0] + piminus[0];
      fillH1F("ppippim_pipi_mass", M.Mag());
    }        
    if(fabs(bestMM2) < 0.1) {
      M = piplus[0] + piminus[0];
      fillH1F("ppippim_pipi_mass_mmcut", M.Mag()); 
      M = proton[0] + piplus[0];
      fillH1F("ppippim_ppip_mass_mmcut", M.Mag());
      M = proton[0] + piminus[0];
      fillH1F("ppippim_ppim_mass_mmcut", M.Mag()); 
    }
  }

  // gamma p -> pi+ pi- (p)
  if(npos == 1 && nneg == 1 && npip == 1 && npim == 1) {
    float bestMM2 = 1000.;
    for(int j=0; j<nbeam; j++) {
      fillH1F("n_ebeam_pippim", beamE[j]);
      if(beamE[j] >= beamcut) {
        Beam.SetPz(beamE[j]);
        Beam.SetE(beamE[j]);
        M = Beam + Tgt - piplus[0] - piminus[0];
        if(fabs(M.Mag2() - MPROTON2) < fabs(bestMM2 - MPROTON2))
          bestMM2 = M.Mag2();
      }
    }
    if(fabs(bestMM2) < 99.) {      
      fillH1F("pippim_mm2", bestMM2);
      M = piplus[0] + piminus[0];
      fillH1F("pippim_pipi_mass", M.Mag());
    }      
    if(fabs(bestMM2 - MPROTON2) < 0.1) {
      M = piplus[0] + piminus[0];
      fillH1F("pippim_pipi_mass_mmcut", M.Mag()); 
    }
  }
  
  // gamma p -> p pi+ (pi-)
  if(npos == 2 && nneg == 0 && nprot == 1 && npip == 1) {
    for(int j=0; j<nbeam; j++)
      fillH1F("n_ebeam_ppip", beamE[j]);
  }

  // gamma p -> p pi- (pi+)
  if(npos == 1 && nneg == 1 && nprot == 1 && npim == 1) {
    for(int j=0; j<nbeam; j++)
      fillH1F("n_ebeam_ppim", beamE[j]);
  }

  // gamma p -> pi+ pi+ pi- (n)
  if(npos == 2 && nneg == 1 && npip == 2 && npim == 1) {
    float bestMM2 = 1000.;
    int ibest;
    for(int j=0; j<nbeam; j++) {
      fillH1F("n_ebeam_2pippim", beamE[j]);
      if(beamE[j] >= beamcut) {
        Beam.SetPz(beamE[j]);
        Beam.SetE(beamE[j]);
        M = Beam + Tgt - piplus[0] - piplus[1] - piminus[0];
        if(fabs(M.Mag2() - MPROTON2) < fabs(bestMM2 - MPROTON2)) {
	  bestMM2 = M.Mag2();      
          ibest = j;
        }
      }
    }
    int mmpzptcut = 0;
    if(fabs(bestMM2) < 99.) {
      fillH1F("2pippim_mm2", bestMM2);
      if(bestMM2 >= 0)
        fillH1F("2pippim_mm", sqrt(fabs(bestMM2)));
      Beam.SetPz(beamE[ibest]);
      Beam.SetE(beamE[ibest]);
      M = Beam + Tgt - piplus[0] - piplus[1] - piminus[0];
      fillH2F("2pippim_mmpz_mmpt",M.Pz(),M.Pt());
      if(M.Pz() > 0.1 && M.Pt() > 0.1) {
        fillH1F("2pippim_mm2_mmpt", M.Mag2());
        if(M.Mag2() >= 0)
          fillH1F("2pippim_mm_mmpt", sqrt(fabs(M.Mag2())));
        mmpzptcut = 1;
      }
      M = piplus[0] + piplus[1] + piminus[0];
      fillH1F("2pippim_pipipi_mass", M.Mag());      
    }
    if(fabs(bestMM2 - MPROTON2) < 0.1 && mmpzptcut > 0) {
      M = piplus[0] + piplus[1] + piminus[0];
      fillH1F("2pippim_pipipi_mass_mmcut", M.Mag());      
      M = piplus[0] + piminus[0];
      fillH1F("2pippim_pipi_mass_mmcut", M.Mag());      
      M = piplus[1] + piminus[0];
      fillH1F("2pippim_pipi_mass_mmcut", M.Mag());
      Beam.SetPz(beamE[ibest]);
      Beam.SetE(beamE[ibest]);
      M = Beam + Tgt - piplus[1] - piminus[0];
      fillH1F("2pippim_npip_mass_mmcut", M.Mag());
      M = Beam + Tgt - piplus[0] - piminus[0];
      fillH1F("2pippim_npip_mass_mmcut", M.Mag());
      M = Beam + Tgt - piplus[0] - piplus[1];
      fillH1F("2pippim_npim_mass_mmcut", M.Mag());
    } 
  }
	
// ...and other histograms  
}


/*************************************************************/
/*                      Misc functions                       */
/*...........................................................*/

void PrintUsage(char * processName)
{
  fprintf(stderr, "Usage: %s file1 [file2] etc....\n\n", processName);
  fprintf(stderr, "  Options:\n");
  fprintf(stderr, "\t-M#\tUse only # number of events\n");
  fprintf(stderr, "\t-S#\tSkip the first # number of events\n");
  fprintf(stderr, "\t-T#\tUse number as trigger mask, eg: -T0x4 only use trigger bit 3\n");
  fprintf(stderr, "\t-G#\tNormalization factor to normalize yield histograms to\n");
  fprintf(stderr, "\t   \t(for example, photon flux from gflux, or beam_time*beam_current, etc.)\n");
  fprintf(stderr, "\t-E#\tMinimum beam energy cut for high-energy photons (default is 4.4 GeV)\n");
  fprintf(stderr, "\t-Pstr\tPrefix 'str' for Postscript and Root output file names (default is 'g12mon')\n");
  fprintf(stderr, "\t-Ostr\tSet output Root file name to 'str' and ignore -P option\n");
  fprintf(stderr, "\t-Lfile\tLoad existing g12mon Root file into GUI (all other options are ignored)\n");
  fprintf(stderr, "\t-N\tDisable default creation of output Root file in the current dir\n");
  fprintf(stderr, "\t-B\tRun in batch mode: produce output Root file and exit without opening GUI\n");
  fprintf(stderr, "\t-h\tPrint this help\n\n");
  exit(0);
}

static int realexit = 0;

void ctrlCHandle(int x)
{
  signal(SIGINT, ctrlCHandle);
  signal(SIGHUP, ctrlCHandle);
  if(realexit > 0) {
    fprintf(stderr, "\n\n\t\t\t***  INTERRUPTED!!!  Exiting... ***\n\n");
    exit(1);
  }
  realexit++;
  fprintf(stderr, "\n\n\t\t\t***  INTERRUPTED!!!  Showing accumulated plots... ***\n\n");
}

/*************************************************************/
/*                           Main                            */
/*...........................................................*/

int main(int argc, char * argv[])
{
  int max = 0;
  int skip_evt = 0;;
  clasHEAD_t * HEAD;
  char * argptr;
  int maxbanks = 1000;
  int Nevents = 0;
  unsigned int triggerMask = 0;
  int runc = -1;
  int batchMode = 0;
  char * loadFile = NULL;
  int saveroot = 1;
 
  signal(SIGINT, ctrlCHandle);
  signal(SIGHUP, ctrlCHandle);
  
  tempbuf = (char *)malloc(10000);
  prefix = (char *)malloc(5000);
  strcpy(prefix, "g12mon");
  beamcut = DEFAULT_BEAM_ENERGY_CUT;
                                    
  if(argc < 2)
    PrintUsage(argv[0]);
  for(int i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      argptr++;
      switch (*(argptr-1)) {
      case 'B':
      case 'b':
        batchMode = 1;
        break;
      case 'E':
      case 'e':            
        beamcut = atof(argptr);
	if(beamcut < -0.1 || beamcut > 6.) {
	  fprintf(stderr, "Beam energy cut should be between 0 and 6 GeV\n");
	  exit(1);
	}
	else
	  fprintf(stderr, "High end of photon beam energy starts at %f GeV\n", beamcut);
        break;
      case 'G':
      case 'g':            
        norm_factor = atof(argptr);
	if(norm_factor <= 0.0) {
	  fprintf(stderr, "Normalization factor should be a positive number\n");
	  exit(1);
	}
	else
	  fprintf(stderr, "Using yield normalization factor %lf\n", norm_factor);
        break;
      case 'H':
      case 'h':
	PrintUsage(argv[0]);
	break;
      case 'L':
      case 'l':
        loadFile = argptr;
	if(strlen(loadFile) < 2)
	  PrintUsage(argv[0]);
	break;
      case 'M':   
      case 'm':   
        max = atoi(argptr);
	if(max <= 0 || max > 100000000)
          PrintUsage(argv[0]);	  
        break;
      case 'N':
      case 'n':
        saveroot = 0;
        break;
      case 'O':
      case 'o':
	if(strlen(argptr) < 1)
          PrintUsage(argv[0]);
	outfile = (char *)malloc(strlen(argptr)+1);
        strcpy(outfile, argptr);
        strcpy(prefix, outfile);	  
	break;
      case 'P':
      case 'p':
        strcpy(prefix, argptr);
	if(strlen(prefix) < 1)
          PrintUsage(argv[0]);	  
	break;
      case 'S':
      case 's':
        skip_evt = atoi(argptr);
	if(skip_evt <= 0 || skip_evt > 100000000)
          PrintUsage(argv[0]);	  
        break;
      case 'T':
      case 't':
        triggerMask = strtoul(argptr, NULL, 0);
        break;
      default:
	fprintf(stderr, "Unrecognized argument: [-%s]\n\n", argptr);
	PrintUsage(argv[0]);
	break;
      }
    }
  }
  if(batchMode == 1 && saveroot == 0)
    fprintf(stderr, "Warning: using both -b and -n switches are silly\n");
  
  theApp = new TApplication("g12 monitor", 0, NULL);
  gROOT->Reset();
  if(!strstr(gSystem->GetLibraries(), "libPhysics"))
    gSystem->Load("libPhysics");
  gStyle->SetOptStat(1110);
  gStyle->SetPaperSize(20, 24);

  if(loadFile) {
    realexit++;
    fprintf(stderr, "Loading existing root file %s ...\n", loadFile);
    LoadAll(loadFile);
  }
  else {
    if(triggerMask != 0) {
      sprintf(tempbuf, "_trig0x%X", triggerMask);
      strcat(prefix, tempbuf);
    }
    strcat(prefix, "_run");

    fprintf(stderr,"Time window for in-time beam photons cut in this version:\n");
    fprintf(stderr,"TAGGER time  : from %.1fns to %.1fns (use hist #1 to verify)\n",TPHO_MIN,TPHO_MAX);
    fprintf(stderr,"ST time      : from %.1fns to %.1fns (use hist #4 to verify)\n",ST1_MIN,ST1_MAX);
    fprintf(stderr,"TAG - ST time: from %.1fns to %.1fns (use hist #5 to verify)\n",DELTA_TPHO_TST_MIN,DELTA_TPHO_TST_MAX);
                            
    bnames_(&maxbanks);
    initbos();

    initHistos();
    Tgt.SetPx(0.);
    Tgt.SetPy(0.);
    Tgt.SetPz(0.);
    Tgt.SetE(0.938262);
    Beam.SetPx(0.);
    Beam.SetPy(0.);

    for(int i = 1;i < argc; ++i) {
      if(realexit > 0)
        break;
      argptr = argv[i];
      if (*argptr != '-') {
        sprintf(tempbuf, "OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", argptr);
       if (!fparm_c(tempbuf)) {
	  fprintf(stderr, "%s: Unable to open file \'%s\': %s\n\n", argv[0], argptr, strerror(errno));
        }
        else {
	  while ((max ? Nevents < max : 1) && (getBOS(&bcs_, 1, "E") != 0) ) { 
            if(realexit > 0)
	      break;
	    if(skip_evt>0){
	      skip_evt--;
	      dropAllBanks(&bcs_, "E");
	      cleanBanks(&bcs_);
	      continue;
	    }

	    if (HEAD = (clasHEAD_t *) getBank(&bcs_, "HEAD")) {
	      if (triggerMask ? (triggerMask & HEAD->head[0].trigbits) : 1) {
	        Nevents++;
	        if (Nevents % 100 == 0) {
		  fprintf(stderr, "%d\r", Nevents);
		  fflush(stderr);
	        }
	        if(abs(HEAD->head[0].nrun) != runc) {
                  int found = 0;
		  char * s = rindex(prefix, 'r');
		  sprintf(tempbuf, "run%i,", abs(HEAD->head[0].nrun));
                  if(s && strncmp(s, tempbuf, strlen(tempbuf)) == 0)
		    found = 1;
		  sprintf(tempbuf, "run%i", abs(HEAD->head[0].nrun));
		  if(s && strcmp(s, tempbuf) == 0)
		    found = 1;		    
		  sprintf(tempbuf, ",%i,", abs(HEAD->head[0].nrun));
		  if(s && strstr(s+3, tempbuf))
		    found = 1;
                  s = rindex(prefix, ',');
		  sprintf(tempbuf, ",%i", abs(HEAD->head[0].nrun));
		  if(s && strcmp(s, tempbuf) == 0)
		    found = 1;
		  if(found == 0) {
		    if(!strstr(prefix+strlen(prefix)-3, "run"))
		      strcat(prefix, ",");
		    sprintf(tempbuf, "%i", abs(HEAD->head[0].nrun));
		    strcat(prefix, tempbuf);  
		  }
	          runc = abs(HEAD->head[0].nrun);
                  initialize_tof(runc);
	        }
                ProcessEvent();
	      }
	    }
	    else
	      fprintf(stderr, "UNABLE TO DECIPHER HEAD BANK!\n");
	    dropAllBanks(&bcs_, "E");
	    cleanBanks(&bcs_);
	  }
	  fprintf(stderr, "#  of events: %d\n", Nevents);
	  fparm_c("CLOSE BOSINPUT UNIT=1");
        }
      }
    }

    postProcessHistos();
    if(saveroot && Nevents > 0)
      SaveAll();
    if(batchMode)
      exit(0);
  } // not loadFile
      
  plotHistos();
  listWindow = new MyMainFrame(gClient->GetRoot(), 400, 400);
  listWindow->SetWMPosition(0, 0);
  theApp->Run();
  exit(0);
}
