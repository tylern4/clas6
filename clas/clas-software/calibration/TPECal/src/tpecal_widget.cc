// %%%%%%%%%%
// Qt headers
// %%%%%%%%%%
#include <QInputDialog>
#include <QMessageBox>
#include <QVBoxLayout>

// %%%%%%%%%%%%%%%%
// Cerenkov headers
// %%%%%%%%%%%%%%%%
#include "tpecal_widget.h"
#include "icon_background.h"

// %%%%%%%%%%%
// C++ headers
// %%%%%%%%%%%
#include <iostream>
using namespace std;

// Main Window Constructor
TPEWidget::TPEWidget(QSplashScreen* spl, QApplication *ecg, tpecal_opts *op, tpecal_calib *tec)
{
	splash       = spl;
	tpecal_gui   = ecg;
	tpecalOpt    = op;
	TPECal_calib = tec;

	splash->showMessage(" Initializing GUI...\n");
	tpecal_gui->processEvents();

	setGeometry(400, 400, 700, 480);
	setMaximumSize(700, 480);
	setMinimumSize(700, 480);
	setFont( QFont( "lucida", 10) );

	// qt4 for setting background widgets
	QPalette bpalette;
	bpalette.setBrush(backgroundRole(), QBrush(QPixmap(background)));
	this->setPalette(bpalette);

	Set_Style();               ///< ROOT Style
	Init_Root_System();        ///< ROOT Application and Canvas

	Init_MenuBar();            ///< Initializes Menubar
	Init_MainWidgets();        ///< Builds Main Widgets

	GetADC      = new GetADC_W(     this, splash, tpecal_gui, tpecalOpt, TPECal_calib, Spe);
	HV_Control  = new HV_Control_W( this, splash, tpecal_gui, tpecalOpt, TPECal_calib, Spe, wlog);
	PED_Display = new PED_Display_W(this, splash, tpecal_gui, tpecalOpt, TPECal_calib, Spe, wlog);
	PED_Control = new PED_Control_W(this, splash, tpecal_gui, tpecalOpt, TPECal_calib, Spe, wlog);
	SPE_Display = new SPE_Display_W(this, splash, tpecal_gui, tpecalOpt, TPECal_calib, Spe, wlog);
	SPE_Control = new SPE_Control_W(this, splash, tpecal_gui, tpecalOpt, TPECal_calib, Spe, wlog);

	display_control(0);   ///< hide all widgets when program starts

	RN->display(TPECal_calib->runno);
	Set_Style();
}

// Menubar
void TPEWidget::Init_MenuBar()
{
	QPalette Palette ;
	Palette.setColor(QPalette::Normal, QPalette::WindowText, Qt::blue);
	Palette.setColor(QPalette::Normal, QPalette::Window,     Qt::white);
	splash->showMessage(" Initializing Menubar...\n");
	tpecal_gui->processEvents();

	// File Menu
	QAction *openTPECal_A = new QAction(tr("&Open bos data file"), this);
	QAction *openROOT_A   = new QAction(tr("&Open root file"),     this);
	QAction *snap_A       = new QAction(tr("&Gif Snapshot"),       this);
	QAction *quit_A       = new QAction(tr("&Quit"),               this);
	connect(openTPECal_A, SIGNAL(triggered()), this, SLOT(openTPECal()));
	connect(openROOT_A,   SIGNAL(triggered()), this, SLOT(openROOT()));
	connect(snap_A,       SIGNAL(triggered()), this, SLOT(print_current()));
	connect(quit_A,       SIGNAL(triggered()), qApp, SLOT(quit()));
	
	QMenu *file = menuBar()->addMenu(tr("&File"));
	file->addAction(openTPECal_A);
	file->addAction(openROOT_A);
	file->addSeparator();
	file->addAction(snap_A);
	file->addSeparator();
	file->addAction(quit_A);
	
	// Process Menu
	QAction *find_ped_A      = new QAction(tr("&Fit Pedestals"),    this);
	QAction *ped_control_A   = new QAction(tr("&Pedestal Control"), this);
	QAction *find_spe_A      = new QAction(tr("&Fit MIP Spectra"),  this);
	QAction *spe_d_control_A = new QAction(tr("&Show MIP Histos"),  this);
	QAction *spe_control_A   = new QAction(tr("&SPE Control"),      this);
	
	connect(find_ped_A,      SIGNAL(triggered()), this, SLOT(find_pedestals()));
	connect(ped_control_A,   SIGNAL(triggered()), this, SLOT(ped_control()));
	connect(find_spe_A,      SIGNAL(triggered()), this, SLOT(fit_all_spe()));
	connect(spe_d_control_A, SIGNAL(triggered()), this, SLOT(MIP_Display_control()));
	connect(spe_control_A,   SIGNAL(triggered()), this, SLOT(spe_control()));
	
	QMenu *process = menuBar()->addMenu(tr("&Process"));
	process->addAction(find_ped_A);
	process->addAction(ped_control_A);
	process->addSeparator();
	process->addAction(find_spe_A);
	process->addAction(spe_d_control_A);
	process->addAction(spe_control_A);
	
	// Database Menu
	QAction *change_min_run_number_A = new QAction(tr("&Change Min Run Number"),  this);
	QAction *change_max_run_number_A = new QAction(tr("&Change Max Run Number"),  this);
	QAction *write_spe_db_A          = new QAction(tr("&Write MIP values in db"), this);
	QAction *write_ped_db_A          = new QAction(tr("&Write PED values in db"), this);
	
	connect(change_min_run_number_A, SIGNAL(triggered()), this, SLOT(change_min_run_number()));
	connect(change_max_run_number_A, SIGNAL(triggered()), this, SLOT(change_max_run_number()));
	connect(write_spe_db_A,          SIGNAL(triggered()), this, SLOT(write_spe_db()));
	connect(write_ped_db_A,          SIGNAL(triggered()), this, SLOT(write_ped_db()));
	
	QMenu *database = menuBar()->addMenu(tr("&Process"));
	database->addAction(change_min_run_number_A);
	database->addAction(change_max_run_number_A);
	database->addAction(write_spe_db_A);
	database->addAction(write_ped_db_A);
	
	
	// HV Menu
	QAction *read_hv_db_A    = new QAction(tr("&Read HV from database"),  this);
	QAction *write_hv_db_A   = new QAction(tr("&Write HV to database"),   this);
	QAction *read_hv_snap_A  = new QAction(tr("&Read HV from snap file"), this);
	QAction *write_hv_snap_A = new QAction(tr("&Write HV to snap file"),  this);
	
	connect(read_hv_db_A,    SIGNAL(triggered()), this, SLOT(read_hv_db()));
	connect(write_hv_db_A,   SIGNAL(triggered()), this, SLOT(write_hv_db()));
	connect(read_hv_snap_A,  SIGNAL(triggered()), this, SLOT(read_hv_snap()));
	connect(write_hv_snap_A, SIGNAL(triggered()), this, SLOT(write_hv_snap()));
	
	QMenu *hvs  = menuBar()->addMenu(tr("&High Voltages"));
	hvs->addAction(read_hv_db_A);
	hvs->addAction(write_hv_db_A);
	hvs->addAction(read_hv_snap_A);
	hvs->addAction(write_hv_snap_A);
	

	// Help Menu
	QAction *how_to_change_hv_A   = new QAction(tr("&How to change High Voltages"),       this);
	QAction *how_to_tpecal_daq_A  = new QAction(tr("&How to take a TPE calibration run"), this);
	
	connect(how_to_change_hv_A,  SIGNAL(triggered()), this, SLOT(how_change_HV()));
	connect(how_to_tpecal_daq_A, SIGNAL(triggered()), this, SLOT(how_to_tpecal_daq()));
	
	QMenu *help = menuBar()->addMenu(tr("&Help"));
	help->addAction(how_to_change_hv_A);
	help->addAction(how_to_tpecal_daq_A);
	
	
	QMenuBar *menubar=  menuBar();
	menubar->addMenu(file);
	menubar->addMenu(process);
	menubar->addMenu(database);
	menubar->addMenu(hvs);
	menubar->addSeparator();
	menubar->addMenu(help);
	menubar->setPalette(Palette);
	menubar->setAutoFillBackground(true);


}

// Main Widget
void TPEWidget::Init_MainWidgets()
{
	splash->showMessage(" Initializing Main GUI Widgets...\n");
	tpecal_gui->processEvents();

	// Main Buttons
	string buttons[8] = 
	{
		"Display MIP",
		"Fit MIP Channels",
		"MIP Control",
		"Display PEDS",
		"Fit PED Channels",
		"Ped Control",
		"HV Control",
		"Exit"
	};

	QWidget *MButtonW = new QWidget(this);
	MButtonW->setGeometry(6, 32, 142, 400);
	QVBoxLayout *VMainButtons = new QVBoxLayout();
	for(int i=0; i<8; i++)
	{
		MainButtons[i] = new QPushButton(buttons[i].c_str());
		VMainButtons->addWidget(MainButtons[i]);
	}

	MButtonW ->setLayout(VMainButtons);
	MButtonW->show();

	connect ( MainButtons[0], SIGNAL( clicked() ), this, SLOT( MIP_Display_control() ) );
	connect ( MainButtons[1], SIGNAL( clicked() ), this, SLOT( fit_all_spe()         ) );
	connect ( MainButtons[2], SIGNAL( clicked() ), this, SLOT( spe_control()         ) );
	connect ( MainButtons[3], SIGNAL( clicked() ), this, SLOT( PED_Display_control() ) );
	connect ( MainButtons[4], SIGNAL( clicked() ), this, SLOT( find_pedestals()      ) );
	connect ( MainButtons[5], SIGNAL( clicked() ), this, SLOT( ped_control()         ) );
	connect ( MainButtons[6], SIGNAL( clicked() ), this, SLOT( hv_control()          ) );
	connect ( MainButtons[7], SIGNAL( clicked() ), qApp, SLOT( quit()              ) );


	// Run Number
	QPalette Palette ;
	Palette.setColor(QPalette::Normal, QPalette::WindowText, Qt::blue);
	Palette.setColor(QPalette::Normal, QPalette::Window,     Qt::transparent);

	QGroupBox *RunNo = new QGroupBox(this);
	RunNo->setGeometry(560, 30, 120, 90);
	RunNo->setTitle("Run Number");

	RN = new QLCDNumber(RunNo);
	RN->setGeometry(20, 30, 80, 40);
	RN->setFrameStyle(0);
	RN->setPalette(Palette);
	RN->setAutoFillBackground(true);

	RN->setSegmentStyle( QLCDNumber::Filled );
	RN->display(tpecalOpt->args["RUN_INDEX_RUN"].arg);

	 // Label used for log messages
	QPalette Palette2 ;
	Palette2.setColor(QPalette::Normal, QPalette::WindowText, Qt::green);
	Palette2.setColor(QPalette::Normal, QPalette::Window,     Qt::black);

	wlog = new QLabel(this);
	wlog->setGeometry(160, 27, 390, 100);
	wlog->setPalette(Palette2);
	wlog->setAutoFillBackground(true);
	wlog->setFrameStyle( QFrame::Panel | QFrame::Sunken );
	wlog->setAlignment(Qt::AlignBottom);
	QString WLOG =  "Ready\n";
	wlog->setText(WLOG);

}



// Opens BOS file dialog
void TPEWidget::openTPECal()
{
	display_control(0);
	QString selectedFilter = "tpe*.A*";
	QString openFilesPath = ".";
	QStringList  files = QFileDialog::getOpenFileNames(
															this,
															tr("Select one or more BOS files to open"),
															openFilesPath,
															tr("Bos data files (peds_all* tpecal_calib*)")
					 										);
															
	QStringList::Iterator items = files.begin();
	while( items++ != files.end() )
	{	
		cout <<      items->toStdString() << " " << QString("[%1]").arg(files.join(", ")).toStdString() << endl;
		// tpecalOpt->ifiles.push_back(items->toStdString () );
	}
	if(files.count())
	{
		display_control(1);
		TPECal_calib->open_tpecal_b = TRUE;
	}
}



// Opens ROOT File
void TPEWidget::openROOT()
{
	display_control(0);
	QString WLOG;

	QFileDialog  *TPEroot;
	TPECal_calib->rfile.clear();
	TPECal_calib->rfile = TPEroot->getOpenFileName(this, tr("Open File"), ".", "ROOT  file (.root)").toStdString() ;
	if(TPECal_calib->rfile == "") return;

	int runno = TPECal_calib->open_root();

	if(!TPECal_calib->open_mip_b && !TPECal_calib->open_ped_b)
	{
		WLOG = "";
		WLOG +=  "\nNo valid ROOT file selected.\nFilename must containg \"spe\" or \"ped\".";
		wlog->setText(WLOG);
		return;
	}
	else
	{
		WLOG = "";
		WLOG +=  "\nOpening ";
		if(TPECal_calib->open_mip_b) WLOG += "SPE file:\n";
		if(TPECal_calib->open_ped_b) WLOG += "pedestal file:\n";
		WLOG += TPECal_calib->rfile.c_str();
		WLOG +=  ".\n";
		WLOG +=  "Initializing histos and fit variables...";
		wlog->setText(WLOG);
		if(!runno)
		{
			WLOG += "\nSomething is wrong with the file...\n";
			wlog->setText(WLOG);
		}
		else
		{
			WLOG += "Done.\n";
			WLOG += "Setting database run number to ";
			WLOG += stringify(runno).c_str();
			RN->display(runno);

			if(TPECal_calib->processed_mip_b || TPECal_calib->processed_ped_b)
				WLOG += "\nFile contains Previous Fit Parameters.\n";

			wlog->setText(WLOG);
		}

		if(TPECal_calib->open_mip_b) { display_control(2); SPE_Display->update_table_display(); }
		if(TPECal_calib->open_ped_b) { display_control(5); PED_Display->update_table_display(); }
	}
}

// Display / Hide control widgets
void TPEWidget::display_control(int opt)
{
	static int InitButton;
	if(InitButton != -99)
	{
		// Assigning int to buttons allows to switch on buttons only when needed
		MainButtons_C[0] = 2;  // "Display MIP"
		MainButtons_C[1] = 0;  // "Fit MIP Channels"
		MainButtons_C[2] = 3;  // "MIP Controls"
		MainButtons_C[3] = 0;  // "Display Peds"
		MainButtons_C[4] = 0;  // "Find Pedestals"
		MainButtons_C[5] = 0;  // "Ped Controls"
		MainButtons_C[6] = 0;  // "HV Control"
		MainButtons_C[7] = 0;  // "Exit"
		InitButton = -99;
	}

	
	GetADC->hide();
	SPE_Display->hide();
	SPE_Control->hide();
	PED_Display->hide();
	PED_Control->hide();
	HV_Control->hide();

	// BOS file Dialog
	if(opt == 1)
		GetADC->show();

	// SPE Display / Fit
	if(opt == 2)
		SPE_Display->show();

	// SPE control
	if(opt == 3)
		SPE_Control->show();

	// PED control
	if(opt == 4)
		PED_Control->show();

	// PED Display
	if(opt == 5)
		PED_Display->show();

	// HV Control
	if(opt == 6)
		HV_Control->show();


	SPE_Control->minRun->display(TPECal_calib->min_runno);
	SPE_Control->maxRun->display(TPECal_calib->max_runno);
	PED_Control->minRun->display(TPECal_calib->min_runno);
	PED_Control->maxRun->display(TPECal_calib->max_runno);

}



// ROOT Style
void TPEWidget::Set_Style()
{
	splash->showMessage(" Initializing ROOT Style...\n"); 
	tpecal_gui->processEvents();

//	gStyle->SetOptTitle(kWhite);
//	gStyle->SetOptStat(kWhite);
//	gStyle->SetFrameFillColor(kWhite);
//	gStyle->SetCanvasColor(kWhite);
//	gStyle->SetPadColor(kWhite);
//
//	gStyle->SetPadRightMargin(0.12);
//	gStyle->SetPadLeftMargin(0.20);
//	gStyle->SetPadBottomMargin(0.16);
//	gStyle->SetPadTopMargin(0.08);
//
//	gStyle->SetCanvasBorderMode(0);
//	gStyle->SetFrameBorderMode(0);
//	gStyle->SetPadBorderMode(0);
}


// ROOT Application and Canvas
void TPEWidget::Init_Root_System()
{
	splash->showMessage(" Initializing ROOT GUI...\n");
	tpecal_gui->processEvents();

	TPEROOTWidget = new TQtWidget(0, "ROOT");  // 0 means new window, otherwise accepts QWidget pointer
	TPEROOTWidget->setGeometry(40, 40, 700, 480);
	TPEROOTWidget->setMaximumSize(1000, 720);
	TPEROOTWidget->setMinimumSize(1000, 720);
	TPEROOTWidget->show();
	Spe = TPEROOTWidget->GetCanvas();
	Spe->Update();
}


void TPEWidget::change_min_run_number()
{
	int RUNNO = QInputDialog::getInteger(this, tr( "Change Min Run Number" ), tr( "Please enter desired run number" ),
													 TPECal_calib->min_runno, 1, 100000);

	TPECal_calib->change_run_number(0, RUNNO);
	QString WLOG =  "ATTENTION!: Database Min Run Number changed to ";
	WLOG += stringify(RUNNO).c_str();
	WLOG += "\n";
	wlog->setText(WLOG);
	SPE_Control->minRun->display(TPECal_calib->min_runno);
}

void TPEWidget::change_max_run_number()
{
	int RUNNO = QInputDialog::getInteger(this, tr( "Change Max Run Number" ), tr( "Please enter desired run number" ),
													 TPECal_calib->max_runno, 1, 100000);

	TPECal_calib->change_run_number(1, RUNNO);
	QString WLOG =  "ATTENTION!: Database Max Run Number changed to ";
	WLOG += stringify(RUNNO).c_str();
	WLOG += "\n";
	wlog->setText(WLOG);
	SPE_Control->maxRun->display(TPECal_calib->max_runno);

}

void TPEWidget::read_hv_db()
{
	TPECal_calib->read_hv_db();
	QString WLOG =  "\nReading HV for run number: ";
	WLOG += stringify(TPECal_calib->runno).c_str();
	WLOG += "\n";
	wlog->setText(WLOG);
	HV_Control->update_table(0);
}

void TPEWidget::write_hv_db()
{

	QString str1 = "This will write the current HV Values in the database.\n\n";
	str1 += "         Run Index: ";
	str1 +=  tpecalOpt->args["RUN_INDEX"].args.c_str();
	str1 += "\n         Min Run Number: ";
	str1 +=  stringify(TPECal_calib->min_runno).c_str();
	str1 += "\n         Max Run Number: ";
	str1 +=  stringify(TPECal_calib->max_runno).c_str();
	str1 +=  "\n\nTo change Min, Max run numbers use \"Database\" menu.\n";
	str1 +=  "\nProceed?\n";
	QMessageBox msgBox;
	msgBox.setText("Calibration Database");
	msgBox.setInformativeText(str1);
	msgBox.setStandardButtons(QMessageBox::Apply | QMessageBox::Cancel);
	msgBox.setDefaultButton(QMessageBox::Cancel);
	msgBox.setIcon(QMessageBox::Question);
	int ret = msgBox.exec();

	if(ret == QMessageBox::Apply)
		TPECal_calib->write_hv_db();
}


void TPEWidget::read_hv_snap()
{
	QString snapfile = QFileDialog::getOpenFileName(this, tr("Open File"), ".", "Snap file (*.snap)");
	TPECal_calib->read_hv_snap(snapfile.toStdString());

	HV_Control->update_table(0);
}

void TPEWidget::write_hv_snap()
{
	TPECal_calib->write_hv_snap();
}


void TPEWidget::write_spe_db()
{

	QString str1 = "This will write the current SPE constants in the database.\n\n";
	str1 += "         Run Index: ";
	str1 +=  tpecalOpt->args["RUN_INDEX"].args.c_str();
	str1 += "\n         Min Run Number: ";
	str1 +=  stringify(TPECal_calib->min_runno).c_str();
	str1 += "\n         Max Run Number: ";
	str1 +=  stringify(TPECal_calib->max_runno).c_str();
	str1 +=  "\n\nTo change Min, Max run numbers use \"Database\" menu.\n";
	str1 +=  "\nProceed?\n";
	QMessageBox msgBox;
	msgBox.setText("Calibration Database");
	msgBox.setInformativeText(str1);
	msgBox.setStandardButtons(QMessageBox::Apply | QMessageBox::Cancel);
	msgBox.setDefaultButton(QMessageBox::Cancel);
	msgBox.setIcon(QMessageBox::Question);
	int ret = msgBox.exec();


	if(ret == QMessageBox::Apply)
		TPECal_calib->write_mip_db();
	
	QString WLOG =  "\n Values written in database: ";
	WLOG += tpecalOpt->args["RUN_INDEX"].args.c_str();
	WLOG += "\n rmin: ";
	WLOG +=  stringify(TPECal_calib->min_runno).c_str();
	WLOG += "\n rmax: ";
	WLOG +=  stringify(TPECal_calib->max_runno).c_str();
	wlog->setText(WLOG);

}

void TPEWidget::write_ped_db()
{

	QString str1 = "This will write the current Pedestals constants in the database.\n\n";
	str1 += "         Run Index: ";
	str1 +=  tpecalOpt->args["RUN_INDEX"].args.c_str();
	str1 += "\n         Min Run Number: ";
	str1 +=  stringify(TPECal_calib->min_runno).c_str();
	str1 += "\n         Max Run Number: ";
	str1 +=  stringify(TPECal_calib->max_runno).c_str();
	str1 +=  "\n\nTo change Min, Max run numbers use \"Database\" menu.\n";
	str1 +=  "\nProceed?\n";
	QMessageBox msgBox;
	msgBox.setText("Calibration Database");
	msgBox.setInformativeText(str1);
	msgBox.setStandardButtons(QMessageBox::Apply | QMessageBox::Cancel);
	msgBox.setDefaultButton(QMessageBox::Cancel);
	msgBox.setIcon(QMessageBox::Question);
	int ret = msgBox.exec();


	if(ret == QMessageBox::Apply)
		TPECal_calib->write_ped_db();

}


void TPEWidget::getADCs()
{
	if(GetADC->pedy->isChecked())
	{
		TPECal_calib->getADCs(1);
		TPECal_calib->open_root();
		display_control(2);
	}
	if(GetADC->pedn->isChecked())
	{
		TPECal_calib->getADCs(0);
		TPECal_calib->open_root();
		display_control(5);
	}
}

void TPEWidget::print_current()
{
	QString snapfile = QInputDialog::getText (0, tr("Snapshot"), tr("Snapshot to GIF File"), QLineEdit::Normal, "snapshot.gif");
	Spe->Print(snapfile.toStdString().c_str());
}

TPEWidget::~TPEWidget()
{
	string hd_msg = tpecalOpt->args["LOG_MSG"].args;
	cout << endl << hd_msg << " Exiting. Closing Cerenkov." << endl;
	cout << endl;
}













