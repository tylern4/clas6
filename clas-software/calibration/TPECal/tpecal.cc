/// \mainpage TPE Calorimeter
/// \section overview Overview
/// tpecal is a calibration
/// software based on <a href="http://root.cern.ch/"> ROOT </a>,
/// <a href="http://www.qtsoftware.com/"> QT </a> written in C++.\n
/// Current capabilities:\n
/// \section HV PMT High Voltages Matching
/// - based on self-triggered data: TPECal_CALIB
/// - aligns all signals to ADC=200
/// - writes snap file to be used with epics and set new HV
/// - writes HV in caldb calibration database
/// \section PED Pedestal Analysis:
/// - based on CODA trigger: PEDS_ALL
/// - gaussian fits of the pedestal signal
/// - writes calc in caldb calibration database
/// \section SPE Single Photoelectron Peak Analysis:
/// - based on self-triggered data: TPECal_CALIB
/// - poissonian + exponential fits of the SPE signal signal
/// - identifies of bad/dead signals
/// - writes calc in caldb calibration database
/// \section platforms Platforms Supported:
/// - Linux (32, 64)
/// \section docs Documentation:
/// - <a href="http://www.jlab.org/~ungaro/maureepage/proj/Cerenkov/main.html"> Cerenkov Software Web Page </a>
/// - <a href="http://www.jlab.org/~ungaro/maureepage/proj/Cerenkov/note/Cerenkov_cc.pdf"> User Guide (pdf) </a>
/// - <a href="http://www.jlab.org/~ungaro/maureepage/proj/Cerenkov/note/Cerenkov_cc.html"> User Guide (html) </a>
/// - <a href="https://jlabsvn.jlab.org/svnroot/clas/trunk/calibration/tpecal_calib/Cerenkov"> Subversion Repository </a>

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
#include "tpecal_calib.h"
#include "tpecal_widget.h"
#include "calorimeter.h"
#include "usage.h"

// %%%%%%%%%%%
// C++ headers
// %%%%%%%%%%%
#include <iostream>
using namespace std;

int main( int argc, char **argv )
{
	tpecal_opts tpecalOpt;
	tpecalOpt.Set(argc, argv);
	
	string hd_msg    = tpecalOpt.args["LOG_MSG"].args + " Init: >> " ;

	cout  << hd_msg  << " TPE Calorimeter Calibration" << endl;
	double use_qt = tpecalOpt.args["USE_QT"].arg;

	QApplication tpecal_gui( argc, argv, (bool) use_qt );
	QSplashScreen *splash = NULL;

	/// Initializing Splash Screen
	if(use_qt)
	{
		tpecal_gui.setStyle(new QCleanlooksStyle);
		splash = new QSplashScreen(QPixmap(calorimeter));
		splash->show();
		tpecal_gui.processEvents();
	}
	
	string msg;
	msg = " Initializing Application...";
	if(use_qt) splash->showMessage(msg.c_str()); tpecal_gui.processEvents(); cout << hd_msg << msg << endl;

	tpecal_calib TPECal_calib(&tpecalOpt);
	TPECal_calib.init_db();
	
	if(use_qt)
	{
		TPEWidget tpecal_widget(splash, &tpecal_gui, &tpecalOpt, &TPECal_calib);
		tpecal_widget.setWindowTitle( "TPE Calorimeter Calibration" );

		tpecal_widget.show();
		splash->finish(&tpecal_widget);

		if(tpecalOpt.ifiles.size())
			tpecal_widget.display_control(1);
		
		if(TPECal_calib.rfile.size() > 1)
		{
			if(TPECal_calib.open_mip_b) tpecal_widget.display_control(2);
			if(TPECal_calib.open_ped_b) tpecal_widget.display_control(5);
		}
		if(tpecalOpt.args["PROCESS_ROOT"].arg)
			cout <<  hd_msg << " Use PROCESS_ROOT only in batch mode. Press the Fit SPE Channels in GUI mode." << endl;
		return tpecal_gui.exec();
	}
	else
	{
    // Process BOS, ROOT files only in batch mode
		if(tpecalOpt.ifiles.size())
		{
			if(tpecalOpt.args["PROCESS_MIP"].arg  == 1) TPECal_calib.getADCs(1);
			if(tpecalOpt.args["PROCESS_BEAM"].arg == 1) TPECal_calib.getADCs(1);
			if(tpecalOpt.args["PROCESS_PED"].arg  == 1) TPECal_calib.getADCs(0);
			if(tpecalOpt.args["PROCESS_ROOT"].arg == 1 && TPECal_calib.open_mip_b) TPECal_calib.fit_all_spe();
			if(tpecalOpt.args["PROCESS_ROOT"].arg == 1 && TPECal_calib.open_ped_b) TPECal_calib.fit_all_ped();
		}
		if(tpecalOpt.args["PROCESS_ROOT"].arg)
		{
			if(TPECal_calib.open_mip_b) TPECal_calib.fit_all_spe();
			if(TPECal_calib.open_ped_b) TPECal_calib.fit_all_ped();
		}
	}
	return 1;
}









