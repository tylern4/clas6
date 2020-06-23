// %%%%%%%%%%
// Qt headers
// %%%%%%%%%%
#include <QMessageBox>

// %%%%%%%%%%%%%%%%
// Cerenkov headers
// %%%%%%%%%%%%%%%%
#include "tpecal_widget.h"


// how to change standard values in the monitoring histos
void TPEWidget::how_change_HV()
{
	QMessageBox msgBox;
	QString help = "How to change High Voltages\n";
	help.append("1) Log in clasrun@clonmon1\n");
	help.append("2) telnet  hvec2 1527\n");
	help.append("3) user, password: user user\n");

	msgBox.setText(help);
	msgBox.exec();
}

// how to take a TPECal calibration run
void TPEWidget::how_to_tpecal_daq()
{
	QMessageBox msgBox;
	QString help = "How totake a TPECal calibration run \n\n";
	help.append("1) Go to the counting house. DAQ should be running in clon03\n");
	help.append("2) Abort Run. Configure: TPE. Download. \n");
	help.append("3) Before GO: set the trigger bit to 9:  s_trigbits 9\n");
	help.append("4) Go. \n");

	msgBox.setText(help);
	msgBox.exec();
}







