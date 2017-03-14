//***************************
// Header file for GUI application to calibrate TOF High Voltages
// This header is for the secondary window to make and fit histograms
/* File : Histograms.h
   Initial Author :  Joe Santoro (VPI)
   Mod Author: Paul Mattione (Rice)
   Creation Date  :  9/2004
*/
//*****************************************
// class definition of the main window
#ifndef __HistoWindow__
#define __HistoWindow__

#include <TGTab.h>
#include <TStyle.h>
#include <TGTextView.h>
#include <TText.h>

class HistoWindow : public TGMainFrame {

private:
int paddle, across, down;
char *histname;
char *pk_val_string;
const char *Title;
const char *out_string;
string Cat_string1,Cat_string2;

TGTab *fTab;
TRootEmbeddedCanvas *CanADC1, *CanLog1, *CanADC2, *CanLog2, *CanADC3, *CanLog3;
TRootEmbeddedCanvas *CanADC4, *CanLog4, *CanADC5, *CanLog5, *CanADC6, *CanLog6;
TRootEmbeddedCanvas *twoDADC, *twoDLog;
TGVerticalFrame   *vframehv1,*vframehv2,*vframehv3;
TGHorizontalFrame *hframehv1,*hframehv2,*hframehv3;
TGTextButton *goHV,*close,*quit;
TGTextView *InVals;
TGTextView *CalVals;
TH1D   *LAhistosSec1ADC[34], *LAhistosSec1Log[34];
TH1D   *LAhistosSec2ADC[34], *LAhistosSec2Log[34];
TH1D   *LAhistosSec3ADC[34], *LAhistosSec3Log[34];
TH1D   *LAhistosSec4ADC[34], *LAhistosSec4Log[34];
TH1D   *LAhistosSec5ADC[34], *LAhistosSec5Log[34];
TH1D   *LAhistosSec6ADC[34], *LAhistosSec6Log[34];
TH1D   *FAhistosSec1ADC[23], *FAhistosSec1Log[23];
TH1D   *FAhistosSec2ADC[23], *FAhistosSec2Log[23];
TH1D   *FAhistosSec3ADC[23], *FAhistosSec3Log[23];
TH1D   *FAhistosSec4ADC[23], *FAhistosSec4Log[23];
TH1D   *FAhistosSec5ADC[23], *FAhistosSec5Log[23];
TH1D   *FAhistosSec6ADC[23], *FAhistosSec6Log[23];
TH2D   *LA_2D_histosADC[6],  *LA_2D_histosLog[6];
TH2D   *FA_2D_histosADC[6],  *FA_2D_histosLog[6];

TStyle *st1;
TGHProgressBar *HistoProgress;
double peak_value;
double log_mean;
double max,min;
double Entries;

public:
ofstream parmfits;
int isFA;

HistoWindow(const TGWindow *p, UInt_t w, UInt_t h, Bool_t LargeAng);
void makeLAHistos();
void makeForwardHistos();
void fitGains(TH1D *gainhisto);
void fitLogs(TH1D *loghisto);
void getLogRange(TH1D *loghisto, double &min, double &max);
void CalHV();
void CloseWindow();
virtual ~HistoWindow();
};
#endif

