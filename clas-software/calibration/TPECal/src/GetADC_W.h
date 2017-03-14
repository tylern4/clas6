#ifndef GetADC_WIDGET_H
#define GetADC_WIDGET_H

// %%%%%%%%%%
// Qt headers
// %%%%%%%%%%
#include <QApplication>
#include <QSplashScreen>
#include <QListWidget>
#include <QRadioButton>

// %%%%%%%%%%%%%
// ROOT headers
// %%%%%%%%%%%%%
#include "TCanvas.h"

// %%%%%%%%%%%%%%%%
// Cerenkov headers
// %%%%%%%%%%%%%%%%
#include "tpecal_calib.h"
#include "usage.h"

class GetADC_W : public QWidget
{
	// metaobject required for non-qt slots
	Q_OBJECT

	public:
		GetADC_W(QWidget*, QSplashScreen*, QApplication*, tpecal_opts*, tpecal_calib*, TCanvas*);
		~GetADC_W(){;}

		QSplashScreen *splash;
		QApplication  *tpecal_gui;
		tpecal_opts   *tpecalOpt;
		tpecal_calib  *TPECal_calib;
		TCanvas       *Spe;

		QListWidget  *bosfiles;
		QRadioButton *pedn, *pedy;

	public slots:
		void cancelTPECal()            { this->hide();}  ///< Cancels bos file reading
		void subtract_pedestal()       { TPECal_calib->open_ped_b = false; TPECal_calib->open_mip_b = true; }
		void donot_subtract_pedestal() { TPECal_calib->open_mip_b = false; TPECal_calib->open_ped_b = true; }

};

#endif
