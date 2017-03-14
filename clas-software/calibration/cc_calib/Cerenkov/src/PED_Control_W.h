#ifndef PED_Control_WIDGET_H
#define PED_Control_WIDGET_H

// %%%%%%%%%%
// Qt headers
// %%%%%%%%%%
#include <QApplication>
#include <QSplashScreen>
#include <QListWidget>
#include <QTableWidget>
#include <QProgressDialog>
#include <QLCDNumber>

// %%%%%%%%%%%%%
// ROOT headers
// %%%%%%%%%%%%%
#include "TCanvas.h"
#include "TLatex.h"


// %%%%%%%%%%%%%%%%
// Cerenkov headers
// %%%%%%%%%%%%%%%%
#include "cc_calib.h"
#include "usage.h"

class PED_Control_W : public QWidget
{
 // metaobject required for non-qt slots
 Q_OBJECT

 public:
   PED_Control_W(QWidget*, QSplashScreen*, QApplication*, cc_opts*, cc_calib*, TCanvas*, QLabel*);
  ~PED_Control_W(){;}

   QSplashScreen *splash;
   QApplication  *cc_gui;
   cc_opts       *ccOpt;
   cc_calib      *CC_calib;
   TCanvas       *Spe;
   TLatex        me;
   QLabel        *wlog;

   QLCDNumber  *minRun;      ///< Database min Run Number
   QLCDNumber  *maxRun;      ///< Database max Run Number
   QLabel      *runIndex;    ///< Run Index name

 private:
   QString WLOG;
   int     PRINT;

 public slots:
   void show_peds();                                       ///> Shows Pedestals Summary
   void print_peds() { PRINT=1; show_peds(); PRINT=0; };   ///> Print Pedestals Summary
   void write_pars();                                      ///< Write Parameters to file

};

#endif
