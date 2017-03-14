// GUI application to calibrate TOF High Voltages
// This is the main window
/* File : MainWindow.C
   Initial Author :  Joe Santoro (VPI)
   Mod Author: Paul Mattione (Rice)
   Creation Date  :  9/2004
*/
//*******************************************

// my includes
#include "MainWindow.h"
#include "ProcessData.cc"

//****************************************************************


TGTextEntry *OUTPUTDIR_name;
int AngleFlag;

TGTextEntry* Get_OUTPUTDIR_name(){
  return OUTPUTDIR_name;
}
int Get_AngleFlag(){
  return AngleFlag;
}

MainWindow::MainWindow(const TGWindow *p, UInt_t w, UInt_t h) : TGMainFrame(p, w, h)
{

TGLayoutHints *DasWindow = new TGLayoutHints(kLHintsExpandX | kLHintsExpandY, 0, 0, 0, 0);

//Put Picture into container window
TGHorizontalFrame *hframe1   = new TGHorizontalFrame(this, w, h);
const TGPicture   *fPic      = gClient->GetPicture("TOFscinC5.xpm");
TGImageMap        *fImageMap = new TGImageMap(this,fPic);

hframe1->AddFrame(fImageMap, DasWindow);
AddFrame(hframe1,DasWindow);

//make frame for buttons
//********************************************************************************************
TGVerticalFrame    *vframe1 = new TGVerticalFrame(this, w, h);

//make the buttons and make them do something & add it to the GUI
//********************************************************************************************
TGTextButton *openAs = new TGTextButton(vframe1, "&Open Large-Angle Counter Datafile");
openAs->Connect("Clicked()","MainWindow",this,"OpenFileA()");
vframe1->AddFrame(openAs, new TGLayoutHints(kLHintsCenterY, 5, 5, 5, 4));
A_file_name = new TGTextEntry(vframe1,datafile_createA = new TGTextBuffer(50));
vframe1->AddFrame(A_file_name, new TGLayoutHints(kLHintsCenterY, 5, 5, 5, 4));

TGTextButton *openCs = new TGTextButton(vframe1,"&Open Forward Counter Datafile");
openCs->Connect("Clicked()","MainWindow",this,"OpenFileC()");
vframe1->AddFrame(openCs, new TGLayoutHints(kLHintsCenterY, 5, 5, 5, 4));
C_file_name = new TGTextEntry(vframe1,datafile_createC = new TGTextBuffer(50));
vframe1->AddFrame(C_file_name, new TGLayoutHints(kLHintsCenterY, 5, 5, 5, 4));

RUNINDEX_name = new TGTextEntry(vframe1,RUNINDEXNAME = new TGTextBuffer(50));
RUNINDEX_name->SetTitle("RunIndex");
vframe1->AddFrame(RUNINDEX_name, new TGLayoutHints(kLHintsCenterY, 5, 5, 5, 4));

OUTPUTDIR_name = new TGTextEntry(vframe1,RUNINDEXNAME = new TGTextBuffer(50));
OUTPUTDIR_name->SetTitle("Output Directory");
vframe1->AddFrame(OUTPUTDIR_name, new TGLayoutHints(kLHintsCenterY, 5, 5, 5, 4));

//*******************************************************************************************
TGHorizontalFrame *hSUBframe1 = new TGHorizontalFrame(vframe1, w/2, 200);
TGTextButton *process = new TGTextButton(hSUBframe1, "&Process Data");
process->Connect("Clicked()","MainWindow", this, "MainProcessData()");
hSUBframe1->AddFrame(process, new TGLayoutHints(kLHintsLeft, 5, 5, 5, 4));

TGTextButton *histograms = new TGTextButton(hSUBframe1, "&Make Histograms");
histograms->Connect("Clicked()","MainWindow", this, "HistWindow()");
hSUBframe1->AddFrame(histograms, new TGLayoutHints(kLHintsCenterY, 5, 5, 5, 4));
vframe1->AddFrame(hSUBframe1, new TGLayoutHints(kLHintsLeft, 5, 5, 5, 4));

//*******************************************************************************************

//add exit button and make it exit
TGTextButton *exit = new TGTextButton(vframe1, "&Exit","gApplication->Terminate(0)");
vframe1->AddFrame(exit, new TGLayoutHints(kLHintsCenterY, 5, 5, 5, 4));

//Radio Buttons to select between a Forward and Large Angle Calibration or both
TGButtonGroup *bg1 = new TGButtonGroup(vframe1, "Calibration Type Selection", kVerticalFrame);
vframe1->AddFrame(bg1, new TGLayoutHints(kLHintsTop,5,5,1,1));

CalType[0] = new TGRadioButton(bg1 ,new TGHotString("&Large-Angle Counters Only"));
CalType[1] = new TGRadioButton(bg1 ,new TGHotString("&Forward-Angle Counters Only"));
CalType[2] = new TGRadioButton(bg1 ,new TGHotString("&Both Forward and Large-Angle Counters"));
CalType[0]->SetState(kButtonDown);

//Add Radio and push buttons to Container
hframe1->AddFrame(vframe1, new TGLayoutHints(kLHintsNormal, 3, 3, 3, 3) );

//Set Name of Main Window
SetWindowName("TOF HV Calibration");
//make all the widgets visible
MapSubwindows();
Resize(GetDefaultSize());
MapWindow();
}

//******* Define the file-opening slots **************
void MainWindow::OpenFileA()
{
TGFileDialog *openA = new TGFileDialog(gClient->GetRoot(), this, kFDOpen, &fileinfo);
fileAString = fileinfo.fFilename;
datafile_createA->AddText(0, fileAString.c_str());
fClient->NeedRedraw(A_file_name);
}

void MainWindow::OpenFileC()
{
TGFileDialog *openF = new TGFileDialog(gClient->GetRoot(), this, kFDOpen, &fileinfo);
fileCString = fileinfo.fFilename;
datafile_createC->AddText(0,fileCString.c_str());
fClient->NeedRedraw(C_file_name);
}

//*******************************************
void MainWindow::MainProcessData()
{

//*** SOME ERROR-HANDLING
//Get Button State
LAfile       =  CalType[0]->GetState();
FAfile       =  CalType[1]->GetState();
BOTHfiles = CalType[2]->GetState();

fileAString = datafile_createA->GetString();
fileCString = datafile_createC->GetString();

if(FAfile == kButtonDown)
  AngleFlag = 0;
if(LAfile == kButtonDown)
  AngleFlag = 1;
if(BOTHfiles == kButtonDown)
  AngleFlag = 2;

if( (LAfile == kButtonDown) || (BOTHfiles == kButtonDown) ){
if(fileAString == ""){
TGMsgBox *HALT = new TGMsgBox(fClient->GetRoot(), this, "Process Error", " NO INPUT FILE!", kMBIconStop);
return;
}

}

else
if(FAfile == kButtonDown){
if(fileCString == ""){
TGMsgBox *HALT = new TGMsgBox(fClient->GetRoot(), this, "Process Error", " NO FORWARD COUNTER INPUT FILES!", kMBIconStop);
return;
}
}

else
if(BOTHfiles == kButtonDown){
if(fileAString == ""){
TGMsgBox *HALT = new TGMsgBox(fClient->GetRoot(), this, "Process Error", " NO LARGE COUNTER INPUT FILE!", kMBIconStop);
return;
 }
if(fileCString == ""){
TGMsgBox *HALT = new TGMsgBox(fClient->GetRoot(), this, "Process Error", " NO FORWARD COUNTER INPUT FILES!", kMBIconStop);
return;
 }
}

if(LAfile == kButtonDown){
ProcessLargeAngle();
}

if(FAfile == kButtonDown){
ProcessForwardAngle();
}

if(BOTHfiles == kButtonDown){
ProcessForwardAngle();
ProcessLargeAngle();
}

if( (LAfile == kButtonUp) && (FAfile == kButtonUp) && (BOTHfiles == kButtonUp) ){
TGMsgBox *HALT = new TGMsgBox(fClient->GetRoot(), this, "Process Error", " NO FILE PROCESSING OPTIONS WERE SELECTED", kMBIconStop);
}

}

//******* Clean up *********
MainWindow::~MainWindow()
{
Cleanup();
}


//**** Pop the Main Window Up ***************
void mainTOFHVGUI()
{
new MainWindow(gClient->GetRoot(), 900, 900);
}

//******* Pop the histogram GUI window up *****************
void MainWindow::HistWindow()
{
//Get Button State
LAfile       = CalType[0]->GetState();
FAfile       = CalType[1]->GetState();
BOTHfiles    = CalType[2]->GetState();

if( (LAfile == kButtonDown) && (BOTHfiles != kButtonDown) ){
new HistoWindow(gClient->GetRoot(),GetDefaultWidth(),GetDefaultHeight(),kTRUE);
}
if( (FAfile == kButtonDown) && (BOTHfiles != kButtonDown) ){
new HistoWindow(gClient->GetRoot(),GetDefaultWidth(),GetDefaultHeight(), kFALSE);
}
if(BOTHfiles == kButtonDown){
new HistoWindow(gClient->GetRoot(),GetDefaultWidth(),GetDefaultHeight(),kTRUE);
new HistoWindow(gClient->GetRoot(),GetDefaultWidth(),GetDefaultHeight(),kFALSE);
}

}



//********* MAIN **************
int main(int argc, char **argv)
{
TApplication HV("App", &argc, argv);
mainTOFHVGUI();
HV.Run();
return 0;
}

