#ifndef HV_Control_WIDGET_H
#define HV_Control_WIDGET_H

// %%%%%%%%%%
// Qt headers
// %%%%%%%%%%
#include <QApplication>
#include <QSplashScreen>
#include <QListWidget>
#include <QTableWidget>
#include <QProgressDialog>

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

class HV_Control_W : public QWidget
{
 // metaobject required for non-qt slots
 Q_OBJECT

 public:
   HV_Control_W(QWidget*, QSplashScreen*, QApplication*, cc_opts*, cc_calib*, TCanvas*, QLabel*);
  ~HV_Control_W(){;}

   QSplashScreen *splash;
   QApplication  *cc_gui;
   cc_opts       *ccOpt;
   cc_calib      *CC_calib;
   TCanvas       *Spe;
   QLabel        *wlog;

 private:

   int current_channel;    ///< starting channel to be displayed on the ROOT canvas
   int sector;             ///< sector to display on the ROOT canvas
   int PRINT;              ///< if set to 1 will print the canvas

   QTableWidget  *table;

 public slots:
 void show_hv(int);        ///< Shows HV. On the table: 0: old. 1: new. 2: difference. On the graphs: 0,1: both, 2: difference
 void show_old_hv()      { show_hv(0); }
 void show_new_hv()      { show_hv(1); }
 void show_diff_hv()     { show_hv(2); }
 void print_hv()         { PRINT = 1; show_hv(1); PRINT = 0;}
 void update_table(int);   ///< updates table values. 0: old. 1: new. 2: difference
 void write_hv_snap(){ CC_calib->write_hv_snap();}


};

#endif
