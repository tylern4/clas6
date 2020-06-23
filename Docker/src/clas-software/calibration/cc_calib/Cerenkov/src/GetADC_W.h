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
#include "cc_calib.h"
#include "usage.h"

class GetADC_W : public QWidget
{
	// metaobject required for non-qt slots
	Q_OBJECT

	public:
		GetADC_W(QWidget*, QSplashScreen*, QApplication*, cc_opts*, cc_calib*, TCanvas*);
		~GetADC_W(){;}

		QSplashScreen *splash;
		QApplication  *cc_gui;
		cc_opts       *ccOpt;
		cc_calib      *CC_calib;
		TCanvas       *Spe;

		QListWidget  *bosfiles;
		QRadioButton *pedn, *pedy;

	public slots:
		void cancelCC()                {this->hide();}  ///< Cancels bos file reading
		void subtract_pedestal()       { CC_calib->open_ped_b = false; CC_calib->open_spe_b = true; }
		void donot_subtract_pedestal() { CC_calib->open_spe_b = false; CC_calib->open_ped_b = true; }

};

#endif
