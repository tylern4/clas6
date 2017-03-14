// %%%%%%%%%%
// Qt headers
// %%%%%%%%%%
#include <QVBoxLayout>
#include <QPushButton>
#include <QLabel>

// %%%%%%%%%%%%%%%%
// Cerenkov headers
// %%%%%%%%%%%%%%%%
#include "GetADC_W.h"
#include "icon_explana2.h"

GetADC_W :: GetADC_W(QWidget *mom, QSplashScreen* spl, QApplication *ccg, cc_opts *op, cc_calib *ccc, TCanvas *Sp) : QWidget(mom)
{
	splash   = spl;
	cc_gui   = ccg;
	ccOpt    = op;
	CC_calib = ccc;
	Spe      = Sp;

	// Setting Geometry
	QPalette palette ;
	palette.setBrush(backgroundRole(), QBrush(QPixmap(explana2)));
	setGeometry(160, 134, 520, 340);
	setPalette(palette);
	setAutoFillBackground(true);

	splash->showMessage(" Initializing Dialog for ADC retrieval...\n");
	cc_gui->processEvents();

	QPalette bpalette ;
	bpalette.setColor(QPalette::Normal, QPalette::WindowText, Qt::yellow);
	bpalette.setColor(QPalette::Normal, QPalette::Window,     Qt::transparent);

	// Pedestal Widget
	QWidget *peds = new QWidget(this);
	peds->setGeometry(30, 220, 200, 80);

	QVBoxLayout *vpeds = new QVBoxLayout;

	pedy = new QRadioButton("Subtract Pedestals");
	pedy->setChecked( TRUE );
	pedy->setPalette(bpalette);

	pedn = new QRadioButton("Do not Subtract Pedestals");
	pedn->setPalette(bpalette);

	vpeds->addWidget(pedy);
	vpeds->addWidget(pedn);
	peds->setLayout(vpeds);

	// List of Files Widget
	bosfiles = new QListWidget(this);
	bosfiles->setGeometry(10, 20, 300, 160);
	for(unsigned int i=0; i<ccOpt->ifiles.size(); i++)
		bosfiles->addItem(ccOpt->ifiles[i].c_str());

	QPushButton *GO_get_adc = new QPushButton( "Get ADCs", this);
	GO_get_adc->setGeometry(370, 220, 100 , 36);

	QPushButton *CANCEL_get_adc = new QPushButton( "Cancel", this);
	CANCEL_get_adc->setGeometry(370, 270 , 100 , 36);

	connect ( CANCEL_get_adc, SIGNAL( clicked() ), this, SLOT( cancelCC()                ) );
	connect ( pedy,           SIGNAL( pressed() ), this, SLOT( subtract_pedestal()       ) );
	connect ( pedn,           SIGNAL( pressed() ), this, SLOT( donot_subtract_pedestal() ) );
	connect ( GO_get_adc,     SIGNAL( clicked() ), mom,  SLOT( getADCs()                 ) );


	QLabel *explan = new QLabel(this);
	explan->setGeometry(340, 20, 200, 120);
	explan->setText(" For Pedestal analysis:\n do not Subract Pedestals");
	explan->setPalette(bpalette);
}




