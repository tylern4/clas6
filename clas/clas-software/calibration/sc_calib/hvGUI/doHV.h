//***************************
// Header file for GUI application to calibrate TOF High Voltages
// This header is for the file management associated with calculation
// of new HV values as well as the calculation of those values
// File : doHV.h
// Initial Author :  Joe Santoro (Catholic U.)
//   Mod Author: Paul Mattione (Rice U.)
// Creation Date  :  11/29/2004
//*****************************************

#ifndef __doHV__
#define __doHV__

#include "TNamed.h"
#include "TTimeStamp.h"

//TNamed object that stores BURT
//FILE info for calculation of
//HV values
//****************************
class BurtNamedObj : public TNamed {
int i,beg_loop,end_loop,local_loop;
TString local_array[5];
TString VALLINE;
TString DUM;

public:
TString lineONOFF,lineVAL,lineRAMPDOWN,lineRAMPUP,lineREQ;
TString Paddle,Counter,Counter2,Counter3,lineVAL2;
int    PAD_NUM;
int    SECTOR;
double VOLTAGE;
double NEW_VOLTAGE;
int    LEFT_RIGHT; // 1=LEFT;0=RIGHT
BurtNamedObj(int mult,TString burtArray[3500]);
};

//TNamed object that stores PARM FILE info
//for calculation of HV values
//****************************
class ParmNamedObj : public TNamed {

public:
double Gain,Centroid;
TString Paddle;
ParmNamedObj(char Paddle[100],double GAIN,double CENTROID);
};

//Main class that performs file I/O and HV calculation
//****************************************************
class doHV {

int i,j,k;
char line[200];
char *burtFile;
TString in_from_burt;
TString burtArray[3500];
TString parmArray[3500];
TString header_line,header[11];
TString WindowInfo;

ifstream BURTFILE,PARMFILE;
ofstream NEW_BURTFILE;
char Paddle[100];
double GAIN,CENTROID,OLD_HV,NEW_HV,DELTA_V;
TGFileInfo fileinfo;
BurtNamedObj *COUNTER[684];
ParmNamedObj *PADDLE_FA[138];
ParmNamedObj *PADDLE_LA[204];
TString name_comp1,name_comp2;
TString Date,Filename;
TString DAY,DATE,MONTH,YEAR,TIME;

public:
doHV(int locAngleFlag, TGTextView *CalVals);
double CalNewHV(double old_HV,double GAIN,double centroid,int LEFT_RIGHT);
TString DateString(TTimeStamp *Time);
void ReturnInOrder(TGTextView *CalVals);
};

#endif




