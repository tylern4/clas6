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
#include "cc_calib.h"
#include "usage.h"

class PED_Display_W : public QWidget
{
 // metaobject required for non-qt slots
 Q_OBJECT

 public:
   PED_Display_W(QWidget*, QSplashScreen*, QApplication*, cc_opts*, cc_calib*, TCanvas*, QLabel*);
  ~PED_Display_W(){;}

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
   QString        WLOG;


 public slots:

   void find_pedestals();                                       ///< fit all PED channels
   void print_fits() { PRINT = 1; find_pedestals(); PRINT=0; }  ///< fit all SPE channels
   void add9();                                                 ///< browse next 9 channels
   void sub9();                                                 ///< browse previous 9 channels
   void table_clicked(int row, int col);                        ///< select channels to be displayed by clicking on the table
   void display_channels();                                     ///< display the channels
   void display_channel(int, int);                              ///< shows one channel + fit
   void update_table_display();                                 ///< display current 9 histos
   void update_table();                                         ///< updates table values

};

#endif
