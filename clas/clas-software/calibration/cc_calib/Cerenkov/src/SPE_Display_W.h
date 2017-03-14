#ifndef SPE_Display_WIDGET_H
#define SPE_Display_WIDGET_H

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

class SPE_Display_W : public QWidget
{
 // metaobject required for non-qt slots
 Q_OBJECT

 public:
   SPE_Display_W(QWidget*, QSplashScreen*, QApplication*, cc_opts*, cc_calib*, TCanvas*, QLabel*);
  ~SPE_Display_W(){;}

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
   int show_formula;       ///< if set to 1, show Poisson Formula and parameters

   QTableWidget  *table;
   QPushButton   *display4;
   QPushButton   *fit_again;
   QSlider       *Min_m, *Max_m, *Min_b, *Max_b;
   QString        WLOG;

   TLatex         lat;

 public slots:

   void fit_spe(int);                                        ///< fit SPE channel(s)
   void fit_single_spe(){ fit_spe(1);                     }  ///< fit one SPE channel
   void fit_all_spe()   { fit_spe(0);                     }  ///< fit all SPE channels
   void print_fits()    { PRINT = 1;
									fit_spe(0);
									print_all_channels();
									PRINT=0; }                        ///< print all SPE channels in a postscript and gif file
	void print_all_channels();                                ///< print all SPE channels in separate gif files
	void add4();                                              ///< browse next 4 channels
   void sub4();                                              ///< browse previous 4 channels
   void table_clicked(int row, int col);                     ///< select channels to be displayed by clicking on the table
   void display_channels();                                  ///< display the channels
   void display_channel(int, int);                           ///< shows one channel + fit
   void update_table_display();                              ///< display current 4 histos
   void update_table();                                      ///< updates table values
   void table_changed(int row, int col);                     ///< when table is manually changed
   void Reset_SPE_sliders();                                 ///< Reset fit parameters to default
   void load_gui_fit_parameters();                           ///< Loads GUI Fit parameters

};

#endif
