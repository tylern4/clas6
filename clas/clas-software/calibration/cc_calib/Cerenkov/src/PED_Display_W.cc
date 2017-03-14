// %%%%%%%%%%
// Qt headers
// %%%%%%%%%%
#include <QVBoxLayout>
#include <QRadioButton>
#include <QPushButton>
#include <QLabel>

// %%%%%%%%%%%%%%%%
// Cerenkov headers
// %%%%%%%%%%%%%%%%
#include "PED_Display_W.h"
#include "icon_explana2.h"

// %%%%%%%%%%%%%
// ROOT headers
// %%%%%%%%%%%%%
#include "TLine.h"
#include "TPostScript.h"

PED_Display_W :: PED_Display_W(QWidget *mom, QSplashScreen* spl, QApplication *ccg, cc_opts *op, cc_calib *ccc, TCanvas *Sp, QLabel* La) : QWidget(mom)
{
	splash   = spl;
	cc_gui   = ccg;
	ccOpt    = op;
	CC_calib = ccc;
	Spe      = Sp;
	wlog     = La;
	
	current_channel = 1;
	sector          = 1;
	PRINT           = 0;
	
	splash->showMessage(" Initializing PED Display GUI...\n");
	cc_gui->processEvents();
	
	// Setting Geometry
	QPalette palette ;
	palette.setBrush(backgroundRole(), QBrush(QPixmap(explana2)));
	setGeometry(160, 134, 520, 340);
	setPalette(palette);
	setAutoFillBackground(true);
	
	QPalette bpalette ;
	bpalette.setColor(QPalette::Normal, QPalette::WindowText, Qt::black);
	bpalette.setColor(QPalette::Normal, QPalette::Window,     QColor(211, 246, 255));
	
	table = new QTableWidget(36, 6, this);
	table->setGeometry(5, 5, 284, 286);
	QStringList sl;
	sl << "S 1" << "S 2" << "S 3" << "S 4" << "S 5" << "S 6";
	table->setHorizontalHeaderLabels(sl);
	table->setPalette(bpalette);
	for(int s=0; s<6; s++)
		for(int ss=0; ss<36; ss++)
			table->setItem(ss,s,new QTableWidgetItem());
		table->setFont( QFont( "lucida", 8) );
	
	for(int r=0;r<36; r++) table->setRowHeight(r, 18);
	for(int r=0;r<6; r++)  table->setColumnWidth(r, 40);
	
	QPushButton *display9_back = new QPushButton( "<< 9", this);
	display9_back->setGeometry(300, 30, 80 , 36);
	
	QPushButton *display9_forw = new QPushButton( "9 >>", this);
	display9_forw->setGeometry(400, 30, 80 , 36);
	
	connect( display9_forw, SIGNAL( clicked() ),              this, SLOT( add9()                   ) );
	connect( display9_back, SIGNAL( clicked() ),              this, SLOT( sub9()                   ) );
	connect( table        , SIGNAL( cellClicked(int, int)  ), this, SLOT( table_clicked(int, int)  ) );
	
	if(CC_calib->open_ped_b) { update_table_display(); }
}


void PED_Display_W::find_pedestals()
{
	int runno = CC_calib->runno;
	string filename = Form("ped_fits_%d.ps", runno);
	TPostScript *ps = NULL;
	if(PRINT)
	{
		ps = new TPostScript(filename.c_str(), 111);
		ps->NewPage();
	}
	
	// progress dialog (if fitting all channels)
	int i    = 1;
	int prog = 0;
	QProgressDialog *pedprogress;
	
	WLOG = "";
	if(!CC_calib->open_ped_b)
	{
		WLOG += "\nNo ROOT PED file opened. ";
		wlog->setText(WLOG);
	}
	else
	{
		WLOG += "\nFitting Pedestals...\n";
		pedprogress = new QProgressDialog("Fitting pedestals ...", "Stop fitting", 0, 216, 0);
		pedprogress->setWindowModality(Qt::WindowModal);
		pedprogress->setValue(prog);
		pedprogress->setMinimumWidth(320);
		wlog->setText(WLOG);
		
		Spe->Clear();
		Spe->Divide(3, 3);
		CC_calib->Set_ParLimits();
		
		for(int s=0; s<6; s++)
			for(int ss=0; ss<36; ss++)
			{
				if(pedprogress->wasCanceled()) break;
				pedprogress->setValue(prog++);
				Spe->cd(i);
				if(!PRINT) CC_calib->fit_ped(s, ss);
				display_channel(s, ss);
				Spe->Update();
				if(++i == 10)
				{
					i = 1;
					if(PRINT) ps->NewPage();
				}
			}
			
			if(PRINT)
			{
				ps->Close();
				delete ps;
			}
			
			pedprogress->setValue(216);
			CC_calib->processed_ped_b = TRUE;
	}
	
	if(PRINT)
	{
		WLOG += "Printing PED fits in file ";
		WLOG += filename.c_str();
		WLOG += "\n";
		wlog->setText(WLOG);
	}
}

void PED_Display_W::add9()
{
	if(current_channel < 28 ) current_channel += 9;
	else
	{
		sector++;
		current_channel = 1;
		if(sector == 7) sector = 1;
	}
	update_table_display();
}


void PED_Display_W::sub9()
{
	if(current_channel > 9 ) current_channel -= 9;
	else
	{
		sector--;
		current_channel = 28;
		if(sector == 0) sector = 6;
	}
	update_table_display();
}



void PED_Display_W::table_clicked(int row, int col)
{
	sector          = col + 1;
	current_channel = row + 1;
	if(current_channel>28) current_channel = 28;
	
	update_table_display();
}

void PED_Display_W::display_channels()
{
	Spe->Clear();
	Spe->Divide(3, 3);
	CC_calib->Set_ParLimits();
	
	static int init;
	if(init != -99)  init = -99;
	else
	{
		CC_calib->Set_ParLimits();
	}
	
	int ci = 1;
	for(int t=current_channel-1; t<current_channel+8; t++)
	{
		Spe->cd(ci++);
		display_channel(sector-1, t);
		Spe->Update();
	}
}



void PED_Display_W::display_channel(int sector, int ch)
{
	
	double par[3];
	if(CC_calib->ped_parameters[sector][ch][0] != 0) for(int p=0;p<3;p++) par[p] = CC_calib->ped_parameters[sector][ch][p] ; // restoring values
	CC_calib->CC_ADC[sector][ch]->Draw("hist");
	if(par[0] != 0)
	{
		CC_calib->PedSignal->SetParameters(&par[0]);
		CC_calib->PedSignal->Draw("same");
	}
}


void PED_Display_W::update_table_display()
{
	update_table();
	display_channels();
}


void PED_Display_W::update_table()
{
	WLOG = wlog->text();
	static int init;
	if(init != -99)
	{
		WLOG +=  "\nDisplaying:\n";
		WLOG += QString("Sector %1    Channel %2-%3").arg(sector).arg(current_channel).arg(current_channel + 9);
		init = -99;
	}
	else
	{
		WLOG  =  "Displaying:\n";
		WLOG += QString("Sector %1    Channel %2-%3").arg(sector).arg(current_channel).arg(current_channel + 9);
	}
	
	wlog->setText(WLOG);
	
	for(int s=0; s<6; s++)
		for(int ss=0; ss<36; ss++)
		{
			if(CC_calib->ped_value[s][ss] != 0) table->item(ss, s)->setText(QString("%1").arg(CC_calib->ped_value[s][ss], 0, 'f', 1));
			if(CC_calib->ped_value[s][ss] == 0) table->item(ss, s)->setText("");
		}
}








