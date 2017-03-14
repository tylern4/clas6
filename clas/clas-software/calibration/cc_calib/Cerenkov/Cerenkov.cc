/// \mainpage Cerenkov
/// \section overview Overview
/// Cerenkov is a calibration
/// software based on <a href="http://root.cern.ch/"> ROOT </a>,
/// <a href="http://www.qtsoftware.com/"> QT </a> written in C++.\n
/// Current capabilities:\n
/// \section HV PMT High Voltages Matching
/// - based on self-triggered data: CC_CALIB
/// - aligns all signals to ADC=200
/// - writes snap file to be used with epics and set new HV
/// - writes HV in caldb calibration database
/// \section PED Pedestal Analysis:
/// - based on CODA trigger: PEDS_ALL
/// - gaussian fits of the pedestal signal
/// - writes calc in caldb calibration database
/// \section SPE Single Photoelectron Peak Analysis:
/// - based on self-triggered data: CC_CALIB
/// - poissonian + exponential fits of the SPE signal signal
/// - identifies of bad/dead signals
/// - writes calc in caldb calibration database
/// \section platforms Platforms Supported:
/// - Linux (32, 64)
/// \section docs Documentation:
/// - <a href="http://www.jlab.org/~ungaro/maureepage/proj/Cerenkov/main.html"> Cerenkov Software Web Page </a>
/// - <a href="http://www.jlab.org/~ungaro/maureepage/proj/Cerenkov/note/Cerenkov_cc.pdf"> User Guide (pdf) </a>
/// - <a href="http://www.jlab.org/~ungaro/maureepage/proj/Cerenkov/note/Cerenkov_cc.html"> User Guide (html) </a>
/// - <a href="https://jlabsvn.jlab.org/svnroot/clas/trunk/calibration/cc_calib/Cerenkov"> Subversion Repository </a>

/// \file Cerenkov.cc
/// Defines the Cerenkov main( int argc, char **argv )
/// \author \n Maurizio Ungaro
/// \author mail: ungaro@jlab.org\n\n\n

// %%%%%%%%%%
// Qt headers
// %%%%%%%%%%
#include <QApplication>
#include <QSplashScreen>
#include <QCleanlooksStyle>

// %%%%%%%%%%%%%%%%
// Cerenkov headers
// %%%%%%%%%%%%%%%%
#include "cc_calib.h"
#include "cc_widget.h"
#include "icon_Cerenkov.h"
#include "usage.h"

// %%%%%%%%%%%
// C++ headers
// %%%%%%%%%%%
#include <iostream>
using namespace std;

int main( int argc, char **argv )
{
	cc_opts ccOpt;
	ccOpt.Set(argc, argv);
	string hd_msg    = ccOpt.args["LOG_MSG"].args + " Init: >> " ;

	cout  << hd_msg  << " Cerenkov Calibration" << endl;
	double use_qt = ccOpt.args["USE_QT"].arg;

	QApplication cc_gui( argc, argv, (bool) use_qt );
	QSplashScreen *splash = NULL;

	/// Initializing Splash Screen
	if(use_qt)
	{
		cc_gui.setStyle(new QCleanlooksStyle);
		splash = new QSplashScreen(QPixmap(Cerenkov));
		splash->show();
		cc_gui.processEvents();
	}
	string msg;
	msg = " Initializing Application...";
	if(use_qt) splash->showMessage(msg.c_str()); cc_gui.processEvents(); cout << hd_msg << msg << endl;

	cc_calib CC_calib(&ccOpt);

	if(use_qt)
	{
		CCWidget cc_widget(splash, &cc_gui, &ccOpt, &CC_calib);
		cc_widget.setWindowTitle( "CLAS Cerenkov Calibration" );

		cc_widget.show();
		splash->finish(&cc_widget);

		if(ccOpt.ifiles.size())
			cc_widget.display_control(1);
		
		if(CC_calib.rfile.size() > 1)
		{
			if(CC_calib.open_spe_b) cc_widget.display_control(2);
			if(CC_calib.open_ped_b) cc_widget.display_control(5);
		}
		if(ccOpt.args["PROCESS_ROOT"].arg)
			cout <<  hd_msg << " Use PROCESS_ROOT only in batch mode. Press the Fit SPE Channels in GUI mode." << endl;
		return cc_gui.exec();
	}
	else
	{
    // Process BOS, ROOT files only in batch mode
		if(ccOpt.ifiles.size())
		{
			if(ccOpt.args["PROCESS_SPE"].arg == 1) CC_calib.getADCs(1);
			if(ccOpt.args["PROCESS_PED"].arg == 1) CC_calib.getADCs(0);
			if(ccOpt.args["PROCESS_ROOT"].arg == 1 && CC_calib.open_spe_b) CC_calib.fit_all_spe();
			if(ccOpt.args["PROCESS_ROOT"].arg == 1 && CC_calib.open_ped_b) CC_calib.fit_all_ped();
		}
		if(ccOpt.args["PROCESS_ROOT"].arg)
		{
			if(CC_calib.open_spe_b) CC_calib.fit_all_spe();
			if(CC_calib.open_ped_b) CC_calib.fit_all_ped();
		}
	}
	return 1;
}









