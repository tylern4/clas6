#ifndef SPE_Display_WIDGET_H
#define SPE_Display_WIDGET_H

// %%%%%%%%%%
// Qt headers
// %%%%%%%%%%
#include <QApplication>
#include <QSplashScreen>
#include <QListWidget>
#include <QProgressDialog>
#include <QRadioButton>
#include <QTableWidget>

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

class SPE_Display_W : public QWidget
{
	// metaobject required for non-qt slots
	Q_OBJECT
	
public:
	SPE_Display_W(QWidget*, QSplashScreen*, QApplication*, tpecal_opts*, tpecal_calib*, TCanvas*, QLabel*);
  ~SPE_Display_W(){;}
	
	QSplashScreen *splash;
	QApplication  *tpecal_gui;
	tpecal_opts   *tpecalOpt;
	tpecal_calib  *TPECal_calib;
	TCanvas       *Spe;
	QLabel        *wlog;
	
private:
	
	int current_channel;    ///< starting channel to be displayed on the ROOT canvas
	int PRINT;              ///< if set to 1 will print the canvas
	int show_formula;       ///< if set to 1, show Poisson Formula and parameters
	int which_histos;       ///<  0: all ADC
	                        ///<  1: Single ADC (with no signals in neighbors)
													///<  2: All TDC
	
	QTableWidget  *table;
	QPushButton   *display6;
	QPushButton   *fit_again;
	QSlider       *Min_m, *Max_m, *Min_b, *Max_b;
	QString        WLOG;
	

	QRadioButton  *all_adc, *single_adc, *all_tdc;

	
	TLatex         lat;
	
	public slots:
	
	void fit_spe(int);                                        ///< fit MIP channel(s)
	void fit_single_spe(){ fit_spe(1);                     }  ///< fit one MIP channel
	void fit_all_spe()   { fit_spe(0);                     }  ///< fit all MIP channels
	void print_fits()    { PRINT = 1;
		                     fit_spe(0);
	               	       print_all_channels(); 
												 PRINT=0; }                         ///< print all MIP channels in a postscript and gif file
	void print_tdcs()    { which_histos = 2;
												 PRINT = 1;
		                     fit_spe(0);
		                     print_all_channels(); 
		                     PRINT=0; }                         ///< print all MIP channels in a postscript and gif file
	void print_all_channels();                                ///< print all MIP channels in separate gif files
	void add6();                                              ///< browse next 4 channels
	void sub6();                                              ///< browse previous 4 channels
	void table_clicked(int row, int col);                     ///< select channels to be displayed by clicking on the table
	void display_channels();                                  ///< display the channels
	void display_channel(int);                                ///< shows one channel + fit
	void update_table_display();                              ///< display current 4 histos
	void update_table();                                      ///< updates table values
	void table_changed(int row, int col);                     ///< when table is manually changed
	void Reset_SPE_sliders();                                 ///< Reset fit parameters to default
	void load_gui_fit_parameters();                           ///< Loads GUI Fit parameters
	
	void hist_choice();
	
	
};

#endif
