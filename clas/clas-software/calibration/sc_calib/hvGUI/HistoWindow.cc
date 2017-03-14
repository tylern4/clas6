// GUI application to calibrate TOF High Voltages
// This is the histogram window
/* File : HistoWindow.C
   Initial Author :  Joe Santoro (VPI)
   Mod Author: Paul Mattione (Rice)
   Creation Date  :  9/2004
*/
//*******************************************

#include "HistoWindow.h"
#include "MainWindow.h"
#include "doHV.cc"

int locMaxLA = 1000000, locMaxFA = 10000000;
//int locMaxLA = 1000, locMaxFA = 1000;
double locHistMax = 2500.0;
int Get_MaxLA();
int Get_MaxFA();
int Get_MaxLA(){
  return locMaxLA;
}
int Get_MaxFA(){
  return locMaxFA;
}

HistoWindow::HistoWindow(const TGWindow *p, UInt_t w, UInt_t h, Bool_t LargeAng) : TGMainFrame(p, w, h)
{

 TGTab *fTab = new TGTab(this, 0, 0);

 //Large Angles
 if(LargeAng==kTRUE){
 across = 7;
 down   = 5;
 isFA   = 0;
 }

 //Forward Angles
 if(LargeAng==kFALSE){
 across = 5;
 down   = 5;
 isFA   = 1;
 }

 TGCompositeFrame *tab1  = fTab->AddTab("Sector 1");
 tab1->SetLayoutManager(new TGHorizontalLayout(tab1));
 CanADC1 = new TRootEmbeddedCanvas("ECanvas", tab1, 400, 400);
 CanADC1->GetCanvas()->SetFillColor(0);
 CanADC1->GetCanvas()->Divide(across,down);
 CanLog1 = new TRootEmbeddedCanvas("ECanvas", tab1, 400, 400);
 CanLog1->GetCanvas()->SetFillColor(0);
 CanLog1->GetCanvas()->Divide(across,down);
 tab1->AddFrame(CanADC1, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 10, 10, 10,10));
 tab1->AddFrame(CanLog1, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 10, 10, 10,10));

 TGCompositeFrame *tab2  = fTab->AddTab("Sector 2");
 tab2->SetLayoutManager(new TGHorizontalLayout(tab2));
 CanADC2 = new TRootEmbeddedCanvas("ECanvas", tab2, 400, 400);
 CanADC2->GetCanvas()->SetFillColor(0);
 CanADC2->GetCanvas()->Divide(across,down);
 CanLog2 = new TRootEmbeddedCanvas("ECanvas", tab2, 400, 400);
 CanLog2->GetCanvas()->SetFillColor(0);
 CanLog2->GetCanvas()->Divide(across,down);
 tab2->AddFrame(CanADC2, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 10, 10, 10,10));
 tab2->AddFrame(CanLog2, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 10, 10, 10,10));

 TGCompositeFrame *tab3  = fTab->AddTab("Sector 3");
 tab3->SetLayoutManager(new TGHorizontalLayout(tab3));
 CanADC3 = new TRootEmbeddedCanvas("ECanvas", tab3, 400, 400);
 CanADC3->GetCanvas()->SetFillColor(0);
 CanADC3->GetCanvas()->Divide(across,down);
 CanLog3 = new TRootEmbeddedCanvas("ECanvas", tab3, 400, 400);
 CanLog3->GetCanvas()->SetFillColor(0);
 CanLog3->GetCanvas()->Divide(across,down);
 tab3->AddFrame(CanADC3, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 10, 10, 10,10));
 tab3->AddFrame(CanLog3, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 10, 10, 10,10));

 TGCompositeFrame *tab4  = fTab->AddTab("Sector 4");
 tab4->SetLayoutManager(new TGHorizontalLayout(tab4));
 CanADC4 = new TRootEmbeddedCanvas("ECanvas", tab4, 400, 400);
 CanADC4->GetCanvas()->SetFillColor(0);
 CanADC4->GetCanvas()->Divide(across,down);
 CanLog4 = new TRootEmbeddedCanvas("ECanvas", tab4, 400, 400);
 CanLog4->GetCanvas()->SetFillColor(0);
 CanLog4->GetCanvas()->Divide(across,down);
 tab4->AddFrame(CanADC4, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 10, 10, 10,10));
 tab4->AddFrame(CanLog4, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 10, 10, 10,10));

 TGCompositeFrame *tab5  = fTab->AddTab("Sector 5");
 tab5->SetLayoutManager(new TGHorizontalLayout(tab5));
 CanADC5 = new TRootEmbeddedCanvas("ECanvas", tab5, 400, 400);
 CanADC5->GetCanvas()->SetFillColor(0);
 CanADC5->GetCanvas()->Divide(across,down);
 CanLog5 = new TRootEmbeddedCanvas("ECanvas", tab5, 400, 400);
 CanLog5->GetCanvas()->SetFillColor(0);
 CanLog5->GetCanvas()->Divide(across,down);
 tab5->AddFrame(CanADC5, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 10, 10, 10,10));
 tab5->AddFrame(CanLog5, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 10, 10, 10,10));

 TGCompositeFrame *tab6  = fTab->AddTab("Sector 6");
 tab6->SetLayoutManager(new TGHorizontalLayout(tab6));
 CanADC6 = new TRootEmbeddedCanvas("ECanvas", tab6, 400, 400);
 CanADC6->GetCanvas()->SetFillColor(0);
 CanADC6->GetCanvas()->Divide(across,down);
 CanLog6 = new TRootEmbeddedCanvas("ECanvas", tab6, 400, 400);
 CanLog6->GetCanvas()->SetFillColor(0);
 CanLog6->GetCanvas()->Divide(across,down);
 tab6->AddFrame(CanADC6, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 10, 10, 10,10));
 tab6->AddFrame(CanLog6, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 10, 10, 10,10));

 TGCompositeFrame *histos2D = fTab->AddTab("2-Dimensional Histograms");
 st1 = new TStyle("st1","my style");
 st1->SetPalette(1);
 st1->SetFrameFillColor(19);
 st1->SetNumberContours(30);
 st1->SetOptStat(0);
 st1->cd();
 histos2D->SetLayoutManager(new TGHorizontalLayout(histos2D));
 twoDADC = new TRootEmbeddedCanvas("ECanvas", histos2D, w/2, h);
 twoDADC->GetCanvas()->SetFillColor(0);
 twoDADC->GetCanvas()->Divide(2,3);
 twoDLog = new TRootEmbeddedCanvas("ECanvas", histos2D, w/2, h);
 twoDLog->GetCanvas()->SetFillColor(0);
 twoDLog->GetCanvas()->Divide(2,3);
 histos2D->AddFrame(twoDADC, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 10, 10, 10,10));
 histos2D->AddFrame(twoDLog, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 10, 10, 10,10));

 TGCompositeFrame *getANDdoHV = fTab->AddTab("Calculate New HV");
 getANDdoHV->SetLayoutManager(new TGHorizontalLayout(getANDdoHV));
 hframehv1 = new TGHorizontalFrame(getANDdoHV,w,h);
 hframehv2 = new TGHorizontalFrame(getANDdoHV,w,h);
 hframehv3 = new TGHorizontalFrame(getANDdoHV,w,h);

 vframehv1 = new TGVerticalFrame(hframehv1,w,h);
 vframehv2 = new TGVerticalFrame(hframehv2,w,h);
 vframehv3 = new TGVerticalFrame(hframehv3,w,h);

 goHV     = new TGTextButton(vframehv1, "&Calculate New HV's");
 goHV->Connect("Clicked()","HistoWindow",this,"CalHV()");
 vframehv1->AddFrame(goHV,new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 2, 2, 2, 2));
 close    = new TGTextButton(vframehv1, "&Close Window");
 close->Connect("Clicked()","HistoWindow",this,"CloseWindow()");
 vframehv1->AddFrame(close,new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 2, 2, 2, 2));
 quit     = new TGTextButton(vframehv1, "&Exit","gApplication->Terminate(0)");
 vframehv1->AddFrame(quit,new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 2, 2, 2, 2));
 hframehv1->AddFrame(vframehv1, new TGLayoutHints(kLHintsNormal, 0, 0, 0, 0) );

 InVals = new TGTextView(vframehv2,w/2,h,"");
 vframehv2->AddFrame(InVals,new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 0, 0, 0, 0));
 hframehv2->AddFrame(vframehv2, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 0, 0, 0, 0) );

 CalVals = new TGTextView(vframehv3,w/2,h,"COUNTER              OLD HV  NEW HV   DELTA V");
 CalVals->AdjustWidth();
 vframehv3->AddFrame(CalVals,new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 0, 0, 0, 0));
 hframehv3->AddFrame(vframehv3, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 0, 0, 0, 0) );

 getANDdoHV->AddFrame(hframehv1, new TGLayoutHints(kLHintsNormal, 0, 0, 0, 0) );
 getANDdoHV->AddFrame(hframehv2, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 2, 2, 0, 0) );
 getANDdoHV->AddFrame(hframehv3, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 2, 2, 0, 0) );

 this->AddFrame(fTab,new TGLayoutHints(kLHintsExpandY | kLHintsExpandX,10,10,10,10));

 //PROGRESS BAR
 TGHorizontalFrame *ProgFrame = new TGHorizontalFrame(this,w,h);
 HistoProgress = new TGHProgressBar(ProgFrame, TGProgressBar::kFancy, w);
 HistoProgress->SetBarColor("lightblue");
 HistoProgress->ShowPosition(kTRUE, kTRUE, "%.0f Events Processed");
 ProgFrame->AddFrame(HistoProgress,new TGLayoutHints(kLHintsExpandY | kLHintsExpandX,10,10,10,10) );
 this->AddFrame(ProgFrame,new TGLayoutHints(kLHintsExpandX,10,10,10,10) );

 if(LargeAng==kTRUE) {SetWindowName("Large-Angle TOF Histogram Viewer and Fitter");}
 if(LargeAng==kFALSE){SetWindowName("Forward-Angle TOF Histogram Viewer and Fitter");}
 MapSubwindows();
 Resize(GetDefaultSize());
 MapWindow();

 if(LargeAng==kTRUE){
 makeLAHistos();
 }
 if(LargeAng==kFALSE){
 makeForwardHistos();
 }

 }

//*********************************
void HistoWindow::makeLAHistos()
{
event_t dataA;
parmfits.open("fit_parametersLA.dat");

string locFileName = (string)((char *)Get_OUTPUTDIR_name()->GetText());
locFileName += "/LargeAngleCounters.root";
TFile* LAfile     = new TFile(locFileName.c_str(),"UPDATE","");

TTree *GAINTREEaS = (TTree*)LAfile->Get("GAINTREEaS");

//****** get branch from data tree ********
TBranch *A = GAINTREEaS->GetBranch("gainA");
A->SetAddress(&dataA);

cout<<"Tree with As on"<<endl;
GAINTREEaS->Print();

histname = new char[30];

//******* Create 2-D histograms *************************
LA_2D_histosADC[1] = new TH2D("ADC vs. COUNTER Sector 1", "", int(floor(locHistMax/15.0)), 5., locHistMax, 50, 1, 50);
LA_2D_histosLog[1] = new TH2D("ln(ADCR/ADCL) vs. COUNTER Sector 1", "", 100, -4., 4., 50, 1, 50);

LA_2D_histosADC[2] = new TH2D("ADC vs. COUNTER Sector 2", "", int(floor(locHistMax/15.0)), 5., locHistMax, 50, 1, 50);
LA_2D_histosLog[2] = new TH2D("ln(ADCR/ADCL) vs. COUNTER Sector 2", "", 100, -4., 4.,50, 1, 50);

LA_2D_histosADC[3] = new TH2D("ADC vs. COUNTER Sector 3", "", int(floor(locHistMax/15.0)), 5., locHistMax, 50, 1, 50);
LA_2D_histosLog[3] = new TH2D("ln(ADCR/ADCL) vs. COUNTER Sector 3", "", 100, -4., 4.,50, 1, 50);

LA_2D_histosADC[4] = new TH2D("ADC vs. COUNTER Sector 4", "", int(floor(locHistMax/15.0)), 5., locHistMax, 50, 1, 50);
LA_2D_histosLog[4] = new TH2D("ln(ADCR/ADCL) vs. COUNTER Sector 4", "", 100, -4., 4.,50, 1, 50);

LA_2D_histosADC[5] = new TH2D("ADC vs. COUNTER Sector 5", "", int(floor(locHistMax/15.0)), 5., locHistMax, 50, 1, 50);
LA_2D_histosLog[5] = new TH2D("ln(ADCR/ADCL) vs. COUNTER Sector 5", "", 100, -4., 4.,50, 1, 50);

LA_2D_histosADC[6] = new TH2D("ADC vs. COUNTER Sector 6", "", int(floor(locHistMax/15.0)), 5., locHistMax, 50, 1, 50);
LA_2D_histosLog[6] = new TH2D("ln(ADCR/ADCL) vs. COUNTER Sector 6", "", 100, -4., 4.,50, 1, 50);

//*** Create histograms for single paddles *********
for(int i=1;i<=34;i++){
//Sector 1
sprintf(histname, "B_hv_SC_S1_%d", i+23);
LAhistosSec1ADC[i]=new TH1D(histname,"", int(floor(locHistMax/15.0)), 2., locHistMax);
sprintf(histname, "B_hv_SC_S1_log%d", i+23);
LAhistosSec1Log[i]=new TH1D(histname,"", 100, -6., 6.);
//Sector 2
sprintf(histname, "B_hv_SC_S2_%d", i+23);
LAhistosSec2ADC[i]=new TH1D(histname,"", int(floor(locHistMax/15.0)), 2., locHistMax);
sprintf(histname, "B_hv_SC_S2_log%d", i+23);
LAhistosSec2Log[i]=new TH1D(histname,"", 100,-6.,6.);
//Sector 3
sprintf(histname, "B_hv_SC_S3_%d", i+23);
LAhistosSec3ADC[i]=new TH1D(histname,"", int(floor(locHistMax/15.0)), 2., locHistMax);
sprintf(histname, "B_hv_SC_S3_log%d", i+23);
LAhistosSec3Log[i]=new TH1D(histname,"", 100,-6.,6.);
//Sector 4
sprintf(histname, "B_hv_SC_S4_%d", i+23);
LAhistosSec4ADC[i]=new TH1D(histname,"", int(floor(locHistMax/15.0)), 2., locHistMax);
sprintf(histname, "B_hv_SC_S4_log%d", i+23);
LAhistosSec4Log[i]=new TH1D(histname,"", 100,-6.,6.);
//Sector 5
sprintf(histname, "B_hv_SC_S5_%d", i+23);
LAhistosSec5ADC[i]=new TH1D(histname,"", int(floor(locHistMax/15.0)), 2., locHistMax);
sprintf(histname, "B_hv_SC_S5_log%d", i+23);
LAhistosSec5Log[i]=new TH1D(histname,"", 100,-6.,6.);
//Sector 6
sprintf(histname, "B_hv_SC_S6_%d", i+23);
LAhistosSec6ADC[i]=new TH1D(histname,"", int(floor(locHistMax/15.0)), 2., locHistMax);
sprintf(histname, "B_hv_SC_S6_log%d", i+23);
LAhistosSec6Log[i]=new TH1D(histname,"", 100,-6.,6.);
}

//******* Loop over Tree with As on and FILL SINGLE-PADDLE HISTOS *********************
Int_t nentries = (Int_t)GAINTREEaS->GetEntries();
int locMaxEntries = Get_MaxLA();
for (Int_t i=0;i<=nentries;i++){
//  if(i == locMaxEntries)
//    break;
GAINTREEaS->GetEntry(i);
if(i%50000 == 0)
cout << "filling entry number = " << (i + 1) << endl;

if(dataA.COUNTER>=24 && dataA.COUNTER<=57){
paddle=dataA.COUNTER-23; // Reset counter index to 1 - 34

//Sector 1
if(dataA.SECTOR==1){
LAhistosSec1ADC[paddle]->Fill(dataA.ADC);
LAhistosSec1Log[paddle]->Fill(dataA.log_ADCL_ADCR);
LA_2D_histosADC[1]->Fill(dataA.ADC, dataA.COUNTER);
LA_2D_histosLog[1]->Fill(dataA.log_ADCL_ADCR, dataA.COUNTER);
}

//Sector 2
if(dataA.SECTOR==2){
LAhistosSec2ADC[paddle]->Fill(dataA.ADC);
LAhistosSec2Log[paddle]->Fill(dataA.log_ADCL_ADCR);
LA_2D_histosADC[2]->Fill(dataA.ADC, dataA.COUNTER);
LA_2D_histosLog[2]->Fill(dataA.log_ADCL_ADCR, dataA.COUNTER);
}

//Sector 3
if(dataA.SECTOR==3){
LAhistosSec3ADC[paddle]->Fill(dataA.ADC);
LAhistosSec3Log[paddle]->Fill(dataA.log_ADCL_ADCR);
LA_2D_histosADC[3]->Fill(dataA.ADC, dataA.COUNTER);
LA_2D_histosLog[3]->Fill(dataA.log_ADCL_ADCR, dataA.COUNTER);
}

//Sector 4
if(dataA.SECTOR==4){
LAhistosSec4ADC[paddle]->Fill(dataA.ADC);
LAhistosSec4Log[paddle]->Fill(dataA.log_ADCL_ADCR);
LA_2D_histosADC[4]->Fill(dataA.ADC, dataA.COUNTER);
LA_2D_histosLog[4]->Fill(dataA.log_ADCL_ADCR, dataA.COUNTER);
}

//Sector 5
if(dataA.SECTOR==5){
LAhistosSec5ADC[paddle]->Fill(dataA.ADC);
LAhistosSec5Log[paddle]->Fill(dataA.log_ADCL_ADCR);
LA_2D_histosADC[5]->Fill(dataA.ADC, dataA.COUNTER);
LA_2D_histosLog[5]->Fill(dataA.log_ADCL_ADCR, dataA.COUNTER);
}

//Sector 6
if(dataA.SECTOR==6){
LAhistosSec6ADC[paddle]->Fill(dataA.ADC);
LAhistosSec6Log[paddle]->Fill(dataA.log_ADCL_ADCR);
LA_2D_histosADC[6]->Fill(dataA.ADC, dataA.COUNTER);
LA_2D_histosLog[6]->Fill(dataA.log_ADCL_ADCR, dataA.COUNTER);
}
} //end of if(counter) check.  still looping over entries

gSystem->ProcessEvents();
} //end of entries loop

// **** Draw single Paddle histos (uses file with A's on but this doesn't matter since these are the same for both files *******
for(int i=1;i<=34;i++){
HistoProgress->SetRange(1,34);
HistoProgress->SetPosition(i);

CanADC1->GetCanvas()->cd(i);
fitGains(LAhistosSec1ADC[i]);
CanADC1->GetCanvas()->Update();
CanLog1->GetCanvas()->cd(i);
fitLogs(LAhistosSec1Log[i]);
CanLog1->GetCanvas()->Update();

CanADC2->GetCanvas()->cd(i);
fitGains(LAhistosSec2ADC[i]);
CanADC2->GetCanvas()->Update();
CanLog2->GetCanvas()->cd(i);
fitLogs(LAhistosSec2Log[i]);
CanLog2->GetCanvas()->Update();

CanADC3->GetCanvas()->cd(i);
fitGains(LAhistosSec3ADC[i]);
CanADC3->GetCanvas()->Update();
CanLog3->GetCanvas()->cd(i);
fitLogs(LAhistosSec3Log[i]);
CanLog3->GetCanvas()->Update();

CanADC4->GetCanvas()->cd(i);
fitGains(LAhistosSec4ADC[i]);
CanADC4->GetCanvas()->Update();
CanLog4->GetCanvas()->cd(i);
fitLogs(LAhistosSec4Log[i]);
CanLog4->GetCanvas()->Update();

CanADC5->GetCanvas()->cd(i);
fitGains(LAhistosSec5ADC[i]);
CanADC5->GetCanvas()->Update();
CanLog5->GetCanvas()->cd(i);
fitLogs(LAhistosSec5Log[i]);
CanLog5->GetCanvas()->Update();

CanADC6->GetCanvas()->cd(i);
fitGains(LAhistosSec6ADC[i]);
CanADC6->GetCanvas()->Update();
CanLog6->GetCanvas()->cd(i);
fitLogs(LAhistosSec6Log[i]);
CanLog6->GetCanvas()->Update();

gSystem->ProcessEvents();
}

for(int i=1;i<=6;i++){
twoDADC->GetCanvas()->cd(i);
LA_2D_histosADC[i]->Draw("COL");
LA_2D_histosADC[i]->GetXaxis()->SetTitle("#sqrt{(ADC_{L}*ADC_{R})}");
LA_2D_histosADC[i]->GetXaxis()->SetTitleOffset(1.2);
LA_2D_histosADC[i]->GetYaxis()->SetTitle("Paddle");
char *sector_title = new char[50];
sprintf(sector_title,"SECTOR %d",i);
LA_2D_histosADC[i]->SetTitle(sector_title);
twoDADC->GetCanvas()->Update();

twoDLog->GetCanvas()->cd(i);
LA_2D_histosLog[i]->Draw("COL");
LA_2D_histosLog[i]->GetXaxis()->SetTitle("ln#frac{ADC_{R}}{ADC_{L}}");
LA_2D_histosLog[i]->GetXaxis()->SetTitleOffset(1.2);
LA_2D_histosLog[i]->GetYaxis()->SetTitle("Paddle");
LA_2D_histosLog[i]->SetTitle(sector_title);
twoDLog->GetCanvas()->Update();
}

CanADC1->GetCanvas()->Print("SECTOR1ADC.ps");
CanADC2->GetCanvas()->Print("SECTOR2ADC.ps");
CanADC3->GetCanvas()->Print("SECTOR3ADC.ps");
CanADC4->GetCanvas()->Print("SECTOR4ADC.ps");
CanADC5->GetCanvas()->Print("SECTOR5ADC.ps");
CanADC6->GetCanvas()->Print("SECTOR6ADC.ps");
CanLog1->GetCanvas()->Print("SECTOR1LOG.ps");
CanLog2->GetCanvas()->Print("SECTOR2LOG.ps");
CanLog3->GetCanvas()->Print("SECTOR3LOG.ps");
CanLog4->GetCanvas()->Print("SECTOR4LOG.ps");
CanLog5->GetCanvas()->Print("SECTOR5LOG.ps");
CanLog6->GetCanvas()->Print("SECTOR6LOG.ps");
twoDADC->GetCanvas()->Print("LA2DHISTOSADCs.ps");
twoDLog->GetCanvas()->Print("LA2DHISTOSLOGs.ps");

LAfile->Write();

parmfits.close();

}

//***********************************
void HistoWindow::makeForwardHistos()
{
event_t dataForward;
parmfits.open("fit_parametersFA.dat");

string locFileName = (string)((char *)Get_OUTPUTDIR_name()->GetText());
locFileName += "/ForwardCounters.root";
TFile* FAfile          = new TFile(locFileName.c_str(),"UPDATE","");

TTree *GAINTREEforward = (TTree*)FAfile->Get("GAINTREEforward");
TBranch *F             = GAINTREEforward->GetBranch("gainForward");
F->SetAddress(&dataForward);
cout<<"Forward Tree"<<endl;
GAINTREEforward->Print();

histname = new char[30];

//******* Create 2-D histograms *************************
FA_2D_histosADC[1] = new TH2D("ADC vs. COUNTER Sector 1", "",int(floor(locHistMax/15.0)), 5., locHistMax, 50, 1., 50.);
FA_2D_histosLog[1] = new TH2D("ln(ADCR/ADCL) vs. COUNTER Sector 1", "", 100, -4., 4.,50, 1, 50);

FA_2D_histosADC[2] = new TH2D("ADC vs. COUNTER Sector 2", "",int(floor(locHistMax/15.0)), 5., locHistMax, 50, 1, 50);
FA_2D_histosLog[2] = new TH2D("ln(ADCR/ADCL) vs. COUNTER Sector 2", "", 100, -4., 4.,50, 1, 50);

FA_2D_histosADC[3] = new TH2D("ADC vs. COUNTER Sector 3", "", int(floor(locHistMax/15.0)), 5., locHistMax, 50, 1, 50);
FA_2D_histosLog[3] = new TH2D("ln(ADCR/ADCL) vs. COUNTER Sector 3", "", 100, -4., 4.,50, 1, 50);

FA_2D_histosADC[4] = new TH2D("ADC vs. COUNTER Sector 4", "", int(floor(locHistMax/15.0)), 5., locHistMax, 50, 1, 50);
FA_2D_histosLog[4] = new TH2D("ln(ADCR/ADCL) vs. COUNTER Sector 4", "", 100, -4., 4.,50, 1, 50);

FA_2D_histosADC[5] = new TH2D("ADC vs. COUNTER Sector 5", "", int(floor(locHistMax/15.0)), 5., locHistMax, 50, 1, 50);
FA_2D_histosLog[5] = new TH2D("ln(ADCR/ADCL) vs. COUNTER Sector 5", "", 100, -4., 4.,50, 1, 50);

FA_2D_histosADC[6] = new TH2D("ADC vs. COUNTER Sector 6", "", int(floor(locHistMax/15.0)), 5., locHistMax, 50, 1, 50);
FA_2D_histosLog[6] = new TH2D("ln(ADCR/ADCL) vs. COUNTER Sector 6", "", 100, -4., 4.,50, 1, 50);

for(int i=0;i<=22;i++){
//Sector 1
if(i>=0&&i<=8){
sprintf(histname, "B_hv_SC_S1_0%d", i+1);
FAhistosSec1ADC[i]=new TH1D(histname,"", int(floor(locHistMax/15.0)), 2., locHistMax);
sprintf(histname, "B_hv_SC_S1_log0%d", i+1);
FAhistosSec1Log[i]=new TH1D(histname,"", 100,-6.,6.);
//Sector 2
sprintf(histname, "B_hv_SC_S2_0%d", i+1);
FAhistosSec2ADC[i]=new TH1D(histname,"", int(floor(locHistMax/15.0)), 2., locHistMax);
sprintf(histname, "B_hv_SC_S2_log0%d", i+1);
FAhistosSec2Log[i]=new TH1D(histname,"", 100,-6.,6.);
//Sector 3
sprintf(histname, "B_hv_SC_S3_0%d", i+1);
FAhistosSec3ADC[i]=new TH1D(histname,"", int(floor(locHistMax/15.0)), 2., locHistMax);
sprintf(histname, "B_hv_SC_S3_log0%d", i+1);
FAhistosSec3Log[i]=new TH1D(histname,"", 100,-6.,6.);
//Sector 4
sprintf(histname, "B_hv_SC_S4_0%d", i+1);
FAhistosSec4ADC[i]=new TH1D(histname,"", int(floor(locHistMax/15.0)), 2., locHistMax);
sprintf(histname, "B_hv_SC_S4_log0%d", i+1);
FAhistosSec4Log[i]=new TH1D(histname,"", 100,-6.,6.);
//Sector 5
sprintf(histname, "B_hv_SC_S5_0%d", i+1);
FAhistosSec5ADC[i]=new TH1D(histname,"", int(floor(locHistMax/15.0)), 2., locHistMax);
sprintf(histname, "B_hv_SC_S5_log0%d", i+1);
FAhistosSec5Log[i]=new TH1D(histname,"", 100,-6.,6.);
//Sector 6
sprintf(histname, "B_hv_SC_S6_0%d", i+1);
FAhistosSec6ADC[i]=new TH1D(histname,"", int(floor(locHistMax/15.0)), 2., locHistMax);
sprintf(histname, "B_hv_SC_S6_log0%d", i+1);
FAhistosSec6Log[i]=new TH1D(histname,"", 100,-6.,6.);
}
else{
sprintf(histname, "B_hv_SC_S1_%d", i+1);
FAhistosSec1ADC[i]=new TH1D(histname,"", int(floor(locHistMax/15.0)), 2., locHistMax);
sprintf(histname, "B_hv_SC_S1_log%d", i+1);
FAhistosSec1Log[i]=new TH1D(histname,"", 100,-6.,6.);
//Sector 2
sprintf(histname, "B_hv_SC_S2_%d", i+1);
FAhistosSec2ADC[i]=new TH1D(histname,"", int(floor(locHistMax/15.0)), 2., locHistMax);
sprintf(histname, "B_hv_SC_S2_log%d", i+1);
FAhistosSec2Log[i]=new TH1D(histname,"", 100,-6.,6.);
//Sector 3
sprintf(histname, "B_hv_SC_S3_%d", i+1);
FAhistosSec3ADC[i]=new TH1D(histname,"", int(floor(locHistMax/15.0)), 2., locHistMax);
sprintf(histname, "B_hv_SC_S3_log%d", i+1);
FAhistosSec3Log[i]=new TH1D(histname,"", 100,-6.,6.);
//Sector 4
sprintf(histname, "B_hv_SC_S4_%d", i+1);
FAhistosSec4ADC[i]=new TH1D(histname,"", int(floor(locHistMax/15.0)), 2., locHistMax);
sprintf(histname, "B_hv_SC_S4_log%d", i+1);
FAhistosSec4Log[i]=new TH1D(histname,"", 100,-6.,6.);
//Sector 5
sprintf(histname, "B_hv_SC_S5_%d", i+1);
FAhistosSec5ADC[i]=new TH1D(histname,"", int(floor(locHistMax/15.0)), 2., locHistMax);
sprintf(histname, "B_hv_SC_S5_log%d", i+1);
FAhistosSec5Log[i]=new TH1D(histname,"", 100,-6.,6.);
//Sector 6
sprintf(histname, "B_hv_SC_S6_%d", i+1);
FAhistosSec6ADC[i]=new TH1D(histname,"", int(floor(locHistMax/15.0)), 2., locHistMax);
sprintf(histname, "B_hv_SC_S6_log%d", i+1);
FAhistosSec6Log[i]=new TH1D(histname,"", 100,-6.,6.);
}
}

Int_t nentries = (Int_t)GAINTREEforward->GetEntries();
cout << "numEntries is: " << nentries << endl;
int locMaxEntries = Get_MaxFA();
for (Int_t i=0;i<nentries;i++){
//  if(i == locMaxEntries)
//    break;
GAINTREEforward->GetEntry(i);

if(i%50000 == 0)
cout << "filling entry number = " << (i + 1) << endl;

if(dataForward.COUNTER>=1 && dataForward.COUNTER<=23){
paddle = dataForward.COUNTER-1;

//Sector 1
if(dataForward.SECTOR==1){
FAhistosSec1ADC[paddle]->Fill(dataForward.ADC);
FAhistosSec1Log[paddle]->Fill(dataForward.log_ADCL_ADCR);
FA_2D_histosADC[1]->Fill(dataForward.ADC, dataForward.COUNTER);
FA_2D_histosLog[1]->Fill(dataForward.log_ADCL_ADCR, dataForward.COUNTER);
}
//Sector 2
if(dataForward.SECTOR==2){
FAhistosSec2ADC[paddle]->Fill(dataForward.ADC);
FAhistosSec2Log[paddle]->Fill(dataForward.log_ADCL_ADCR);
FA_2D_histosADC[2]->Fill(dataForward.ADC, dataForward.COUNTER);
FA_2D_histosLog[2]->Fill(dataForward.log_ADCL_ADCR, dataForward.COUNTER);
}
//Sector 3
if(dataForward.SECTOR==3){
FAhistosSec3ADC[paddle]->Fill(dataForward.ADC);
FAhistosSec3Log[paddle]->Fill(dataForward.log_ADCL_ADCR);
FA_2D_histosADC[3]->Fill(dataForward.ADC, dataForward.COUNTER);
FA_2D_histosLog[3]->Fill(dataForward.log_ADCL_ADCR, dataForward.COUNTER);
}
//Sector 4
if(dataForward.SECTOR==4){
FAhistosSec4ADC[paddle]->Fill(dataForward.ADC);
FAhistosSec4Log[paddle]->Fill(dataForward.log_ADCL_ADCR);
FA_2D_histosADC[4]->Fill(dataForward.ADC, dataForward.COUNTER);
FA_2D_histosLog[4]->Fill(dataForward.log_ADCL_ADCR, dataForward.COUNTER);
}
//Sector 5
if(dataForward.SECTOR==5){
FAhistosSec5ADC[paddle]->Fill(dataForward.ADC);
FAhistosSec5Log[paddle]->Fill(dataForward.log_ADCL_ADCR);
FA_2D_histosADC[5]->Fill(dataForward.ADC, dataForward.COUNTER);
FA_2D_histosLog[5]->Fill(dataForward.log_ADCL_ADCR, dataForward.COUNTER);
}
//Sector 6
if(dataForward.SECTOR==6){
FAhistosSec6ADC[paddle]->Fill(dataForward.ADC);
FAhistosSec6Log[paddle]->Fill(dataForward.log_ADCL_ADCR);
FA_2D_histosADC[6]->Fill(dataForward.ADC, dataForward.COUNTER);
FA_2D_histosLog[6]->Fill(dataForward.log_ADCL_ADCR, dataForward.COUNTER);
}
}

gSystem->ProcessEvents();
}

for(int i=1;i<=23;i++){
int k=i-1;
HistoProgress->SetRange(1,23);
HistoProgress->SetPosition(i);

CanADC1->GetCanvas()->cd(i);
fitGains(FAhistosSec1ADC[k]);
CanADC1->GetCanvas()->Update();
CanLog1->GetCanvas()->cd(i);
fitLogs(FAhistosSec1Log[k]);
CanLog1->GetCanvas()->Update();

CanADC2->GetCanvas()->cd(i);
fitGains(FAhistosSec2ADC[k]);
CanADC2->GetCanvas()->Update();
CanLog2->GetCanvas()->cd(i);
fitLogs(FAhistosSec2Log[k]);
CanLog2->GetCanvas()->Update();

CanADC3->GetCanvas()->cd(i);
fitGains(FAhistosSec3ADC[k]);
CanADC3->GetCanvas()->Update();
CanLog3->GetCanvas()->cd(i);
fitLogs(FAhistosSec3Log[k]);
CanLog3->GetCanvas()->Update();

CanADC4->GetCanvas()->cd(i);
fitGains(FAhistosSec4ADC[k]);
CanADC4->GetCanvas()->Update();
CanLog4->GetCanvas()->cd(i);
fitLogs(FAhistosSec4Log[k]);
CanLog4->GetCanvas()->Update();

CanADC5->GetCanvas()->cd(i);
fitGains(FAhistosSec5ADC[k]);
CanADC5->GetCanvas()->Update();
CanLog5->GetCanvas()->cd(i);
fitLogs(FAhistosSec5Log[k]);
CanLog5->GetCanvas()->Update();

CanADC6->GetCanvas()->cd(i);
fitGains(FAhistosSec6ADC[k]);
CanADC6->GetCanvas()->Update();
CanLog6->GetCanvas()->cd(i);
fitLogs(FAhistosSec6Log[k]);
CanLog6->GetCanvas()->Update();

gSystem->ProcessEvents();
}

for(int i=1;i<=6;i++){
twoDADC->GetCanvas()->cd(i);
FA_2D_histosADC[i]->Draw("COL");
FA_2D_histosADC[i]->GetXaxis()->SetTitle("#sqrt{(ADC_{L}*ADC_{R})}");
FA_2D_histosADC[i]->GetXaxis()->SetTitleOffset(1.2);
FA_2D_histosADC[i]->GetYaxis()->SetTitle("Paddle");
char *sector_title = new char[50];
sprintf(sector_title,"SECTOR %d",i);
FA_2D_histosADC[i]->SetTitle(sector_title);
twoDADC->GetCanvas()->Update();

twoDLog->GetCanvas()->cd(i);
FA_2D_histosLog[i]->Draw("COL");
FA_2D_histosLog[i]->GetXaxis()->SetTitle("ln#frac{ADC_{R}}{ADC_{L}}");
FA_2D_histosLog[i]->GetXaxis()->SetTitleOffset(1.2);
FA_2D_histosLog[i]->GetYaxis()->SetTitle("Paddle");
FA_2D_histosLog[i]->SetTitle(sector_title);
twoDLog->GetCanvas()->Update();
cout<<"SECTOR:"<<i<<endl;
}

CanADC1->GetCanvas()->Print("SECTOR1ADCf.ps");
CanADC2->GetCanvas()->Print("SECTOR2ADCf.ps");
CanADC3->GetCanvas()->Print("SECTOR3ADCf.ps");
CanADC4->GetCanvas()->Print("SECTOR4ADCf.ps");
CanADC5->GetCanvas()->Print("SECTOR5ADCf.ps");
CanADC6->GetCanvas()->Print("SECTOR6ADCf.ps");
CanLog1->GetCanvas()->Print("SECTOR1LOGf.ps");
CanLog2->GetCanvas()->Print("SECTOR2LOGf.ps");
CanLog3->GetCanvas()->Print("SECTOR3LOGf.ps");
CanLog4->GetCanvas()->Print("SECTOR4LOGf.ps");
CanLog5->GetCanvas()->Print("SECTOR5LOGf.ps");
CanLog6->GetCanvas()->Print("SECTOR6LOGf.ps");
twoDADC->GetCanvas()->Print("FA2DHISTOSADCs.ps");
twoDLog->GetCanvas()->Print("FA2DHISTOSLOGs.ps");

FAfile->Write();

parmfits.close();

}

// Fit the gains histograms
void HistoWindow::fitGains(TH1D *gainhisto){
double par[5];
//double par[6];
Title         = new char[50];
pk_val_string = new char[50];
//TF1 *pol2     = new TF1("pol2","pol2");
TF1 *expo     = new TF1("expo","expo");
TF1 *landau   = new TF1("landau","landau");
//The Fit
gainhisto->Fit("expo","","");
//gainhisto->Fit("pol2","","");
expo->GetParameters(&par[0]);
//pol2->GetParameters(&par[0]);
gainhisto->Fit("landau","","");
landau->GetParameters(&par[2]);
//landau->GetParameters(&par[3]);
TF1 *gainfit  = new TF1("gainfit","expo(0)+landau(2)");
//TF1 *gainfit  = new TF1("gainfit","pol2(0)+landau(3)");
gainfit->SetLineColor(8);
gainfit->SetLineWidth(1);
gainfit->SetParameters(par);
Entries=gainhisto->GetEntries();

if(Entries>1000){
gainhisto->Fit("gainfit","","",0.,2000.);
peak_value = gainfit->GetParameter(3);
//peak_value = gainfit->GetParameter(4);
}
else{
gainhisto->Draw("");
gainfit->SetParameter(3,600.);
//gainfit->SetParameter(4,600.);
peak_value = gainfit->GetParameter(3);
//peak_value = gainfit->GetParameter(4);
}

gainhisto->GetXaxis()->SetTitle("#sqrt{(ADC_{L}*ADC_{R})}");
gainhisto->GetXaxis()->SetTitleOffset(1.2);
Title      = gainhisto->GetName();

if(peak_value<=150.0){
//gainfit->SetParameter(4,600.);
//peak_value = gainfit->GetParameter(4);
gainfit->SetParameter(3,600.);
peak_value = gainfit->GetParameter(3);
}

//Label
TText *Label = new TText();
Label->DrawTextNDC(0.5,0.8,Title);
sprintf(pk_val_string,"%4.1f",peak_value);
Label->DrawTextNDC(0.5,0.7,pk_val_string);
string s1(Title);
string s2(pk_val_string);
Cat_string1 = s1+" \t ";
Cat_string1 = Cat_string1+s2;
}

//Fit the ln(ADCR/ADCL) histograms
void HistoWindow::fitLogs(TH1D *loghisto){
pk_val_string = new char[50];
out_string    = new char[300];
TF1 *logfit   = new TF1("logfit","[2]/(1.0+TMath::Exp((TMath::Abs(x-[0])-[3])/[1]))",-10.,10.);
logfit->SetLineColor(9);
logfit->SetLineWidth(1);
log_mean = loghisto->GetMean();
getLogRange(loghisto,min,max);
logfit->SetParameter(0,log_mean);
loghisto->Fit("logfit","Q","",min,max);
loghisto->GetXaxis()->SetTitle("ln#frac{ADC_{R}}{ADC_{L}}");
loghisto->GetXaxis()->SetTitleOffset(1.1);
peak_value = logfit->GetParameter(0);
sprintf(pk_val_string,"%f",peak_value);
//Label
TText *Label = new TText();
Label->DrawTextNDC(0.5,0.8,Title);
sprintf(pk_val_string,"%2.4f",peak_value);
Label->DrawTextNDC(0.5,0.7,pk_val_string);
string s2(pk_val_string);
Cat_string2 = Cat_string1+" \t ";
Cat_string2 = Cat_string2+s2;
out_string  = Cat_string2.c_str();
InVals->AddLine(out_string);
InVals->AdjustWidth();
cout<<Cat_string2<<endl;
parmfits<<Cat_string2<<endl;
}

//Get data range of histogram in order to fit
void HistoWindow::getLogRange(TH1D *loghisto, double &min, double &max){
int i,nbin,min_bin,max_bin,BinMax;
Stat_t bin_val;
Bool_t OnOff=kFALSE;
double binMax_val,limit,log_mean,diff;

nbin       = loghisto->GetNbinsX();
log_mean   = loghisto->GetMean();
BinMax     = loghisto->GetMaximumBin();
binMax_val = loghisto->GetBinContent(BinMax);
limit      = (0.05*binMax_val);

for(i=0;i<=nbin;i++){
bin_val    = loghisto->GetBinContent(i);
if(bin_val>limit && OnOff==kFALSE){
min_bin=i;
OnOff=kTRUE;
}
if(bin_val<limit && OnOff==kTRUE){
max_bin=i;
OnOff=kFALSE;
}
}
min = loghisto->GetXaxis()->GetBinCenter(min_bin);
max = loghisto->GetXaxis()->GetBinCenter(max_bin);
diff = max-min;
if((max<min)||(max==min)||(diff<0.75)){
min=-3.0;
max= 3.0;
}

}

//Calculate HV Values
void HistoWindow::CalHV()
{
cout<<"Calculating New High Voltages"<<endl;
int locAngleFlag = Get_AngleFlag();
cout << "angleflag = " << locAngleFlag << endl;
doHV(locAngleFlag, CalVals);
}

void HistoWindow::CloseWindow()
{
this->DeleteWindow();
}

//Destructor
HistoWindow::~HistoWindow()
{
Cleanup();
}








