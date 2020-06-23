//***************************
// Header file for GUI application to calibrate TOF High Voltages
/* File : MainWindow.h
   Initial Author :  Joe Santoro (VPI)
   Mod Author: Paul Mattione (Rice)
   Creation Date  :  9/2004
*/
//*****************************************
// class definition of the main window
#ifndef __MainWindow__
#define __MainWindow__

#include <cstdio>
#include <string>
#include <cstring>
#include <cmath>
#include <ctime>
#include <cstdlib>
#include <csignal>
#include <cerrno>
#include <iostream>
#include <iomanip>
#include <fstream>

using namespace std;

//*** ROOT INCLUDES ****************
#include <TGFrame.h>
#include <TRootEmbeddedCanvas.h>
#include <TGFileDialog.h>
#include <TGButtonGroup.h>
#include <TGButton.h>
#include <TGProgressBar.h>
#include <TSystem.h>
#include <TGTextEntry.h>
#include <TROOT.h>
#include <TApplication.h>
#include <TGClient.h>
#include <TCanvas.h>
#include <TF1.h>
#include <TImage.h>
#include <TRandom.h>
#include <TGMsgBox.h>
#include <TGImageMap.h>
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TTree.h"

typedef struct event_t {
    int ADCL;
    int ADCR;
    double ADC;
    double log_ADCL_ADCR;
    int SECTOR;
    int COUNTER;
    int runno;
    int eventnumber;
    };

//*************************************************
class MainWindow : public TGMainFrame {

public:
MainWindow(const TGWindow *p, UInt_t w, UInt_t h);
virtual ~MainWindow();
void OpenFileA();
void OpenFileC();
void MainProcessData();
void HistWindow();
int   ProcessLargeAngle();
int   ProcessForwardAngle();
TArrayF getSCpedestals(int runno, const char *item, TGTextEntry *RunIndexName);

TGTextEntry *A_file_name, *C_file_name;
string fileAString, fileCString;
TGFileInfo fileinfo;
TGRadioButton *CalType[6];
EButtonState LAfile, FAfile, BOTHfiles;
TArrayF LEFT_PED, RIGHT_PED;
TGTextBuffer  *datafile_createA,*datafile_createC,*RUNINDEXNAME;
TGTextEntry *RUNINDEX_name;
};

//***************************************************
class Progress : public TGTransientFrame {

private:
   TGHorizontalFrame *fHframe;
   TGLayoutHints       *fHint;

public:
Progress(const TGWindow *p, const TGWindow *main,
              UInt_t w, UInt_t h, const char *title);
virtual ~Progress();
TGHProgressBar *TheBar;

};

TGTextEntry* Get_OUTPUTDIR_name();
int Get_AngleFlag();

#endif




