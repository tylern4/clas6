#ifndef SPE_Control_WIDGET_H
#define SPE_Control_WIDGET_H

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

class SPE_Control_W : public QWidget
{
 // metaobject required for non-qt slots
 Q_OBJECT

 public:
   SPE_Control_W(QWidget*, QSplashScreen*, QApplication*, cc_opts*, cc_calib*, TCanvas*, QLabel*);
  ~SPE_Control_W(){;}

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
   void show_spes();                                      ///< Show SPEs Graphs
   void print_spes() { PRINT=1; show_spes(); PRINT=0; }   ///< Show SPEs Graphs
   void write_pars();                                     ///< Write Parameters to file

};

#endif
