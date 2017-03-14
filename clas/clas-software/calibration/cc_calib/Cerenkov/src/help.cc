// %%%%%%%%%%
// Qt headers
// %%%%%%%%%%
#include <QMessageBox>

// %%%%%%%%%%%%%%%%
// Cerenkov headers
// %%%%%%%%%%%%%%%%
#include "cc_widget.h"


// how to change standard values in the monitoring histos
void CCWidget::how_to_std_values()
{
	QMessageBox msgBox;
	QString help = "How to change the CC Standard Values in the Monitoring Histograms\n\n";
	help.append("1) Log in clasrun@clonmon1\n");
	help.append("2) Run monb, histos from Disk File (choose a good run), View on screen, Online Monitor, Electron Histos\n");
	help.append("3) cd cc\n");
	help.append("4) exe ../cc/cc_expert\n");
	help.append("5) exe ../cc/cc_stand_data\n\n");
	help.append("This will produce a file: ~/cc_stand_values.dat.\nMove this file to $CLON_KUMAC location to change the standard values.\n");
	help.append("Remember to also save the file with a convenient name, such as cc_stand_values_<run>.dat in $CLON_KUMAC\n\n");
	help.append("The kumac location is in ~clasrun/monitor/kumac/standard/cc_mon.kumac\n");

	msgBox.setText(help);
	msgBox.exec();
}

// how to take a CC calibration run
void CCWidget::how_to_spe_daq()
{
	QMessageBox msgBox;
	QString help = "How totake a CC calibration run \n\n";
	help.append("1) Go to the counting house. DAQ should be running in clon03\n");
	help.append("2) Abort Run. Configure: CC_CALIB. Download. Trigger: calib > cc_calib_LUT\n");
	help.append("3) Prestart: fill proper informations. Please add the Torus Current value in the comments box.\n");
	help.append("4) Go. Stop the run at 40M events.\n");
	help.append("5) The output can be located in clondaq2 in /raid/stage_in\n\n");

	msgBox.setText(help);
	msgBox.exec();
}

// how to take a pedestal run
void CCWidget::how_to_ped_daq()
{
	QMessageBox msgBox;
	QString help = "How totake a pedestal run \n\n";
	help.append("1) Go to the counting house. DAQ should be running in clon03\n");
	help.append("2) Abort Run. Configure: PEDS_ALL. Download. Trigger: calib > peds_all\n");
	help.append("3) Prestart: fill proper informations.\n");
	help.append("4) Go. Stop the run at 20K events.\n");
	help.append("5) The output can be located in clondaq2 in /raid/stage_in\n\n");

	msgBox.setText(help);
	msgBox.exec();
}







