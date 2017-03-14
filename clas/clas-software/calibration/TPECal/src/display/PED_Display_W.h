#ifndef PED_Display_WIDGET_H
#define PED_Display_WIDGET_H

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
#include "tpecal_calib.h"
#include "usage.h"

class PED_Display_W : public QWidget
{
 // metaobject required for non-qt slots
 Q_OBJECT

 public:
   PED_Display_W(QWidget*, QSplashScreen*, QApplication*, tpecal_opts*, tpecal_calib*, TCanvas*, QLabel*);
  ~PED_Display_W(){;}

   QSplashScreen *splash;
   QApplication  *tpecal_gui;
   tpecal_opts   *tpecalOpt;
   tpecal_calib  *TPECal_calib;
   TCanvas       *Spe;
   QLabel        *wlog;

 private:

   int current_channel;    ///< starting channel to be displayed on the ROOT canvas
   int sector;             ///< sector to display on the ROOT canvas
   int PRINT;              ///< if set to 1 will print the canvas

   QTableWidget  *table;
   QString        WLOG;


 public slots:

   void find_pedestals();                                       ///< fit all PED channels
   void print_fits() { PRINT = 1; find_pedestals(); PRINT=0; }  ///< fit all SPE channels
   void display_channels();                                     ///< display the channels
   void display_channel(int);                                   ///< shows one channel + fit
   void update_table_display();                                 ///< display current 9 histos
   void update_table();                                         ///< updates table values

};

#endif
