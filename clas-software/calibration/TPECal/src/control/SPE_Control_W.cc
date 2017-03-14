// %%%%%%%%%%
// Qt headers
// %%%%%%%%%%
#include <QVBoxLayout>
#include <QRadioButton>
#include <QPushButton>
#include <QLabel>
#include <QGroupBox>

// %%%%%%%%%%%%%%%%
// Cerenkov headers
// %%%%%%%%%%%%%%%%
#include "SPE_Control_W.h"
#include "icon_explana2.h"

// %%%%%%%%%%%%%
// ROOT headers
// %%%%%%%%%%%%%
#include "TLine.h"

// %%%%%%%%%%%
// C++ headers
// %%%%%%%%%%%
#include <cmath>

SPE_Control_W :: SPE_Control_W(QWidget *mom, QSplashScreen* spl, QApplication *ecg, 
                              tpecal_opts *op, tpecal_calib *tec, TCanvas *Sp, QLabel* La) : QWidget(mom)
{
	splash       = spl;
	tpecal_gui   = ecg;
	tpecalOpt    = op;
	TPECal_calib = tec;
	Spe          = Sp;
	wlog         = La;
	
	PRINT = 0;
	
	me.SetTextAlign(0);
	me.SetNDC();
	
	splash->showMessage(" Initializing PED Display GUI...\n");
	tpecal_gui->processEvents();
	
	// Setting Geometry
	QPalette palette ;
	palette.setBrush(backgroundRole(), QBrush(QPixmap(explana2)));
	setGeometry(160, 134, 520, 340);
	setPalette(palette);
	setAutoFillBackground(true);
	
	QPushButton *MainButtons[6];
	string buttons[6] =
	{
		"Show MIP",
		"Print MIP",
		"Print all ADC Histos",
		"Print all TDC Histos",
		"Write pars to file",
		"Put MIP in db"
	};
	
	QWidget *MButtonW = new QWidget(this);
	MButtonW->setGeometry(2, 2, 160, 300);
	QVBoxLayout *VMainButtons = new QVBoxLayout();
	for(int i=0; i<6; i++)
	{
		MainButtons[i] = new QPushButton(buttons[i].c_str());
		VMainButtons->addWidget(MainButtons[i]);
	}
	MButtonW->setLayout(VMainButtons);
	
	connect ( MainButtons[0], SIGNAL( clicked() ), this,  SLOT( show_spes()    ) );
	connect ( MainButtons[1], SIGNAL( clicked() ), this,  SLOT( print_spes()   ) );
	connect ( MainButtons[2], SIGNAL( clicked() ), mom,   SLOT( print_fits()   ) );
	connect ( MainButtons[3], SIGNAL( clicked() ), mom,   SLOT( print_tdcs()   ) );
	connect ( MainButtons[4], SIGNAL( clicked() ), this,  SLOT( write_pars()   ) );
	connect ( MainButtons[5], SIGNAL( clicked() ), mom,   SLOT( write_spe_db() ) );
	
	QPalette Palette ;
	Palette.setColor(QPalette::Normal, QPalette::WindowText, Qt::black);
	Palette.setColor(QPalette::Normal, QPalette::Window,     Qt::transparent);
	
	QGroupBox *RunInfo = new QGroupBox(this);
	RunInfo->setGeometry(180, 20, 300, 260);
	RunInfo->setTitle("Run Index Information");
	RunInfo-> setFont( QFont( "lucida", 12) );
	
	minRun = new QLCDNumber(RunInfo);
	minRun->setGeometry(20, 60, 90, 30);
	minRun->setFrameStyle(0);
	minRun->setPalette(Palette);
	minRun->setAutoFillBackground(true);
	minRun->setSegmentStyle( QLCDNumber::Filled );
	minRun->display(TPECal_calib->min_runno);
	
	maxRun = new QLCDNumber(RunInfo);
	maxRun->setGeometry(170, 60, 90, 30);
	maxRun->setFrameStyle(0);
	maxRun->setPalette(Palette);
	maxRun->setAutoFillBackground(true);
	maxRun->setSegmentStyle( QLCDNumber::Filled );
	maxRun->display(TPECal_calib->max_runno);
	
	
	QLabel *lminRun = new QLabel(RunInfo);
	lminRun->setGeometry(20, 20, 120, 40);
	lminRun->setPalette(Palette);
	lminRun->setAutoFillBackground(true);
	lminRun->setText("Minimum Run");
	
	QLabel *lmaxRun = new QLabel(RunInfo);
	lmaxRun->setGeometry(170, 20, 120, 40);
	lmaxRun->setPalette(Palette);
	lmaxRun->setAutoFillBackground(true);
	lmaxRun->setText("Maximum Run");
	
	
	// Run Index Label
	QPalette Palette3 ;
	Palette3.setColor(QPalette::Normal, QPalette::WindowText, Qt::yellow);
	Palette3.setColor(QPalette::Normal, QPalette::Window,     QColor(0,   0, 0, 60));
	runIndex = new QLabel(this);
	runIndex->setGeometry(200, 150, 250, 40);
	runIndex-> setFont( QFont( "lucida", 12) );
	runIndex->setAutoFillBackground(true);
	runIndex->setPalette(Palette3);
	runIndex->setFrameStyle( QFrame::Panel | QFrame::Sunken );
	runIndex->setAlignment(Qt::AlignCenter);
	runIndex->setText(tpecalOpt->args["RUN_INDEX"].args.c_str());
	
}

void SPE_Control_W::show_spes()
{
	int runno       = TPECal_calib->runno;
	string filename = Form("spe_positions_%d.gif", runno);
	
	TH1F *hr ;
	Spe->Clear();
	TPad *spad = new TPad("spad", "spad", 0.02, 0.02, 0.98, 0.9);
	spad->Divide(1, 1);
	spad->Draw();
	
	TLine *Line = new TLine(-1, 200, 37, 200);
	Line->SetLineColor(2);
	TPECal_calib->spe_graphs(1);  ///< builds  TGraphErrors
	
	// determining min, max of graphs
	float max = 0;
	for(int ss=0; ss<30; ss++)
		if(fabs(TPECal_calib->mip_value[ss]) > max) max = fabs(TPECal_calib->mip_value[ss]);
	max = max*1.2;
	
	float min = 100;
	for(int ss=0; ss<30; ss++)
		if(fabs(TPECal_calib->mip_value[ss]) < min) min = fabs(TPECal_calib->mip_value[ss]);
	min = min*0.8;
	
	spad->cd(1);
	hr = gPad->DrawFrame(0, min, 30, max);
	hr->SetXTitle(Form("TPE ADC          "));
	hr->SetTitleOffset(1.0);
	hr->SetTitleSize(.09);
	hr->SetLabelSize(.065, "XY");
	TPECal_calib->MeanO->Draw("P");
	TPECal_calib->Meanu->Draw("P");
	TPECal_calib->MeanD->Draw("P");
	TPECal_calib->MeanM->Draw("P");
	Line->Draw("same");
	
	
	Spe->cd();
	
	me.SetTextColor(1);
	me.SetTextSize(0.034);
	me.DrawLatex(.03, .94, "Minimum Ionizing Peak");
	me.DrawLatex(.17, .90, Form("Run %d", TPECal_calib->runno));
	me.SetTextSize(0.029);
	me.SetTextColor(2);
	me.DrawLatex(.55, .95, "#bullet DB Values ");
	me.SetTextColor(4);
	me.DrawLatex(.55, .91, "#diamond Fit Values ");
	me.SetTextColor(1);
	me.DrawLatex(.75, .95, "#diamond Dead Channels ");
	me.SetTextColor(3);
	me.DrawLatex(.75, .91, "#diamond Bad Channels ");
	
	Spe->Update();
	
	if(PRINT)
	{
		Spe->Print(filename.c_str());
	}
	
	TPECal_calib->spe_graphs(0); ///< deletes  TGraphErrors
	delete Line;
	delete hr;
	delete spad;
	
	QString WLOG = "";
	if(!PRINT) WLOG += "Showing SPE position...\nBlue squares: Current Spe Value.\nRed squares: original fit values. ";
	if(PRINT)
	{
		WLOG += "Printing SPE position in file ";
		WLOG += filename.c_str();
		WLOG += "\n";
	}
	
	wlog->setText(WLOG);
}

void SPE_Control_W::write_pars()
{
	QString WLOG = "";
	WLOG += "\n Writing parameters to ROOT file ";
	WLOG +=  TPECal_calib->rfile.c_str();
	WLOG += "\n Writing means to dat file ";
	WLOG +=  TPECal_calib->datfile.c_str();
	wlog->setText(WLOG);
	
	TPECal_calib->write_pars();
	
}




