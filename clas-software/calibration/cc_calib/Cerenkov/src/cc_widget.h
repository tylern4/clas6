#ifndef CCWIDGET_H
#define CCWIDGET_H

// %%%%%%%%%%
// Qt headers
// %%%%%%%%%%
#include <QApplication>
#include <QButtonGroup>
#include <QGroupBox>
#include <QFileDialog>
#include <QLabel>
#include <QLCDNumber>
#include <QListWidget>
#include <QMenuBar>
#include <QProgressDialog>
#include <QPushButton>
#include <QRadioButton>
#include <QSlider>
#include <QSplashScreen>
#include <QTableWidget>
#include <QWidget>
#include <QMainWindow>

// %%%%%%%%%%%%%
// ROOT headers
// %%%%%%%%%%%%%
#include "TLatex.h"
#include "TPostScript.h"
#include "TQtWidget.h"
#include "TStyle.h"

// %%%%%%%%%%%%%%%%
// Cerenkov headers
// %%%%%%%%%%%%%%%%
#include "GetADC_W.h"
#include "HV_Control_W.h"
#include "PED_Control_W.h"
#include "PED_Display_W.h"
#include "SPE_Control_W.h"
#include "SPE_Display_W.h"
#include "cc_calib.h"
#include "usage.h"


class CCWidget : public QMainWindow
{
	// metaobject required for non-qt slots
	Q_OBJECT

	public:

		CCWidget(QSplashScreen*, QApplication*, cc_opts*, cc_calib*);
		~CCWidget();

		QSplashScreen *splash;
		QApplication  *cc_gui;
		cc_opts       *ccOpt;
		cc_calib      *CC_calib;

		// Main Widgets
		QPushButton *MainButtons[10];
		int          MainButtons_C[10];
		QLCDNumber  *RN;      ///< Database Run Number
		QLabel      *wlog;    ///< Log messages
		QLabel      *run_i;   ///< Run Index

		// ROOT GUI objects
		TQtWidget    *CCROOTWidget;
		TCanvas      *Spe;

		// Various initializations
		void Set_Style();                ///< ROOT Style
		void Init_Root_System();         ///< ROOT Application and Canvas
		void Init_MenuBar();             ///< Application Menu
		void Init_MainWidgets();         ///< Main Window Widgets

		void display_control(int opt);   ///< Controls Widget show/hide


	private:

		// Dialog Widgets
		GetADC_W       *GetADC;        ///< Dialog to get ADC from bos files
		HV_Control_W   *HV_Control;    ///< HV Control
		PED_Display_W  *PED_Display;   ///< Display PED histos
		PED_Control_W  *PED_Control;   ///< PED Control
		SPE_Display_W  *SPE_Display;   ///< Display SPE histos/performs single fit
		SPE_Control_W  *SPE_Control;   ///< SPE Control


	public slots:
		void openCC();                         ///< Loads bos file list
		void getADCs();                        ///< Get ADCs from bos file(s)
		void openROOT();                       ///< Loads ROOT file name
		void print_current();                  ///< Snapshot of current Canvas

		// Various Dialog Wrappers
		void find_pedestals() { PED_Display->find_pedestals(); }   ///< fits all PED channels
		void print_all_peds() { PED_Display->print_fits();     }   ///< Print all PEDs Fits
		void fit_all_spe()    { SPE_Display->fit_all_spe();    }   ///< fits all SPE channels
		void print_fits()     { SPE_Display->print_fits();     }   ///< Print all SPEs Fits

		// dialog display
		void SPE_Display_control() { display_control(2); SPE_Display->update_table_display(); }  ///< Shows SPE Display GUI
		void PED_Display_control() { display_control(5); PED_Display->update_table_display(); }  ///< Shows PED Display GUI
		void spe_control()         { display_control(3);                              }          ///< Shows SPE buttons
		void ped_control()         { display_control(4);                              }          ///< Shows PED buttons
		void hv_control()          { display_control(6);                              }          ///< Shows PED buttons

		// database i/o
		void change_run_index();         ///< Changes Run Index
		void change_min_run_number();    ///< Changes Database Min Run Number
		void change_max_run_number();    ///< Changes Database Max Run Number
		void read_hv_db();               ///< Reads HV voltages from DB
		void write_hv_db();              ///< Writes HV voltages to DB
		void read_hv_snap();             ///< Reads HV voltages from snap file
		void write_hv_snap();            ///< Writes HV voltages to snap file
		void write_spe_db();             ///< Writes SPE Constants to database
		void write_ped_db();             ///< Writes PED Constants to database

		// help
		void how_to_std_values();
		void how_to_spe_daq();
		void how_to_ped_daq();



};
#endif


















