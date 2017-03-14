#ifndef __J_GLOBAL_H
#define __J_GLOBAL_H

#include "TGraphErrors.h"
#include "TH1.h"
#include "TF1.h"
#include "Calibration.h"
#include "SConstants.h"
#include "JSurvey.h"
#include "ShowSector.h"
#include "SingleStripe.h"
#include "NeedInput.h"
#include "gamecock.h"
#include "jdefine.h"

#ifdef DEFINE_GLOBAL
Calibration*   gCalib;
TGraphErrors*  gGraph[2][N_CHANNEL];
TH1F*          gHisto[2][N_CHANNEL];
TF1*           gFitFn[5][N_CHANNEL];
SConstants*    gConst[5];
JSurvey*       gSurvey;
ShowSector*    gSector;
SingleStripe*  gStripe;
NeedConstants* gNeed;
GoGamecock*    gCock;
#else
extern Calibration*   gCalib;
extern TGraphErrors*  gGraph[2][N_CHANNEL];
extern TH1F*          gHisto[2][N_CHANNEL];
extern TF1*           gFitFn[5][N_CHANNEL];
extern SConstants*    gConst[5];
extern JSurvey*       gSurvey;
extern ShowSector*    gSector;
extern SingleStripe*  gStripe;
extern NeedConstants* gNeed;
extern GoGamecock*    gCock;
#endif

#endif
