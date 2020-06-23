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

PED_Display_W :: PED_Display_W(QWidget *mom, QSplashScreen* spl, QApplication *ecg, 
                               tpecal_opts *op, tpecal_calib *tec, TCanvas *Sp, QLabel* La) : QWidget(mom)
{
	splash       = spl;
	tpecal_gui   = ecg;
	tpecalOpt    = op;
	TPECal_calib = tec;
	Spe          = Sp;
	wlog         = La;
	
	current_channel = 1;
	sector          = 1;
	PRINT           = 0;
	
	splash->showMessage(" Initializing PED Display GUI...\n");
	tpecal_gui->processEvents();
	
	// Setting Geometry
	QPalette palette ;
	palette.setBrush(backgroundRole(), QBrush(QPixmap(explana2)));
	setGeometry(160, 134, 520, 340);
	setPalette(palette);
	setAutoFillBackground(true);
	
	QPalette bpalette ;
	bpalette.setColor(QPalette::Normal, QPalette::WindowText, Qt::black);
	bpalette.setColor(QPalette::Normal, QPalette::Window,     QColor(211, 246, 255));
	
	table = new QTableWidget(5, 6, this);
	table->setGeometry(5, 62, 282, 168);
	QStringList layers;
	layers << "L 1" << "L 2" << "L 3" << "L 4" << "L 5";
	table->setVerticalHeaderLabels(layers);
	
	table->setPalette(bpalette);
	int i, j;
	for(int ss=0; ss<30; ss++)
	{		
		i = TPECal_calib->jrow(ss);
		j = TPECal_calib->jcol(ss);
		
		table->setItem(i, j, new QTableWidgetItem());
	}
	table->setFont( QFont( "lucida", 8) );
	
	
	for(int r=0;r<30; r++) table->setRowHeight(r, 27);
	for(int r=0;r<6; r++)  table->setColumnWidth(r, 43);
	
			
	if(TPECal_calib->open_ped_b) { update_table_display(); }
}


void PED_Display_W::find_pedestals()
{
 

	int runno = TPECal_calib->runno;
	string filename = Form("ped_fits_%d.gif", runno);
	// not working for mac
//	if(PRINT)
//	{
//		ps = new TPostScript(filename.c_str(), 111);
//		ps->NewPage();
//	}
	
	// progress dialog (if fitting all channels)
	// int prog = 0;
	
	WLOG = "";
	if(!TPECal_calib->open_ped_b)
	{
		WLOG += "\nNo ROOT PED file opened. ";
		wlog->setText(WLOG);
	}
	else
	{
		WLOG += "\nFitting Pedestals...\n";
		wlog->setText(WLOG);
		
		Spe->Clear();
		Spe->Divide(6, 5);
		TPECal_calib->Set_ParLimits();
		
		for(int ss=0; ss<30; ss++)
		{
			Spe->cd(ss+1);
			if(!PRINT) TPECal_calib->fit_ped(ss);
			display_channel(ss);
			Spe->Update();
		}
		
		if(PRINT)
		{
//			ps->Close();
//			delete ps;
			Spe->Print(filename.c_str());

		}
		
		TPECal_calib->processed_ped_b = TRUE;
	}
	
	if(PRINT)
	{
		WLOG += "Printing PED fits in file ";
		WLOG += filename.c_str();
		WLOG += "\n";
		wlog->setText(WLOG);
	}
}



void PED_Display_W::display_channels()
{
	Spe->Clear();
	Spe->Divide(6, 5);
	TPECal_calib->Set_ParLimits();
	
	static int init;
	if(init != -99)  init = -99;
	else
	{
		TPECal_calib->Set_ParLimits();
	}
	
	for(int t=0; t<30; t++)
	{
		Spe->cd(t+1);
		display_channel(t);
		Spe->Update();
	}
}



void PED_Display_W::display_channel(int ch)
{
	
	double par[3];
	if(TPECal_calib->ped_parameters[ch][0] != 0) 
		for(int p=0;p<3;p++) par[p] = TPECal_calib->ped_parameters[ch][p] ; // restoring values
	TPECal_calib->TPECal_ADC[ch]->Draw("hist");
	if(par[0] != 0)
	{
		TPECal_calib->PedSignal->SetParameters(&par[0]);
		TPECal_calib->PedSignal->Draw("same");
	}
}


void PED_Display_W::update_table_display()
{
	update_table();
	display_channels();
}


void PED_Display_W::update_table()
{
  int i, j;
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
	
	for(int ss=0; ss<30; ss++)
	{
		i = TPECal_calib->jrow(ss);
		j = TPECal_calib->jcol(ss);
		if(TPECal_calib->ped_value[ss] != 0) table->item(i, j)->setText(QString("%1").arg(TPECal_calib->ped_value[ss], 0, 'f', 1));
		if(TPECal_calib->ped_value[ss] == 0) table->item(i, j)->setText("aa");
	}
}








