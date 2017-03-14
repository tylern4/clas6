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
#include "SPE_Display_W.h"
#include "icon_explana2.h"

// %%%%%%%%%%%%%
// ROOT headers
// %%%%%%%%%%%%%
//#include "TPostScript.h"
#include "TPDF.h"

// %%%%%%%%%%%
// C++ headers
// %%%%%%%%%%%
#include <cmath>

SPE_Display_W :: SPE_Display_W(QWidget *mom, QSplashScreen* spl, QApplication *ccg, cc_opts *op, cc_calib *ccc, TCanvas *Sp, QLabel* La) : QWidget(mom)
{
	splash   = spl;
	cc_gui   = ccg;
	ccOpt    = op;
	CC_calib = ccc;
	Spe      = Sp;
	wlog     = La;
	
	lat.SetTextAlign(0);
	lat.SetNDC();
	
	current_channel = 1;
	sector          = 1;
	PRINT           = 0;
	show_formula    = 1;
	
	splash->showMessage(" Initializing SPE Display GUI...\n");
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
	
	display4 = new QPushButton("Display\nS1  1-4", this);
	display4->setGeometry(108, 297, 80, 36);
	
	QPushButton *display4_back = new QPushButton( "<< 4", this);
	display4_back->setGeometry(28, 297, 80 , 36);
	
	QPushButton *display4_forw = new QPushButton( "4 >>", this);
	display4_forw->setGeometry(188, 297, 80 , 36);
	
	fit_again = new QPushButton( "Fit again", this);
	fit_again->setGeometry(300, 10, 100 , 40);
	
	QPushButton *reset_par = new QPushButton("Reset pars", this);
	reset_par->setGeometry(410, 10, 100 , 40);
	
	connect( display4_forw, SIGNAL( clicked()              ), this, SLOT( add4()                   ) );
	connect( display4_back, SIGNAL( clicked()              ), this, SLOT( sub4()                   ) );
	connect( display4     , SIGNAL( clicked()              ), this, SLOT( update_table_display()   ) );
	connect( table        , SIGNAL( cellClicked(int, int)  ), this, SLOT( table_clicked(int, int)  ) );
	connect( table        , SIGNAL( cellChanged(int, int)  ), this, SLOT( table_changed(int, int)  ) );
	connect( fit_again    , SIGNAL( clicked()              ), this, SLOT( fit_single_spe()         ) );
	connect( reset_par    , SIGNAL( clicked()              ), this, SLOT( Reset_SPE_sliders()      ) );
	
	QPalette spalette ;
	spalette.setColor(QPalette::Normal, QPalette::WindowText, QColor(0,  0, 0));
	spalette.setColor(QPalette::Normal, QPalette::Window,     QColor(0, 255, 0, 20));
	
	QLabel *to_slider = new QLabel(this);
	to_slider->setGeometry(310, 70, 190, 18);
	to_slider->setAlignment(Qt::AlignHCenter);
	to_slider->setText("POISSON + BACKGROUND");
	to_slider->setPalette(spalette);
	to_slider->setAutoFillBackground(true);
	
	Min_m = new QSlider(Qt::Horizontal, this);
	Min_m->setTickPosition(QSlider::TicksBelow);
	Min_m->setGeometry(305, 120, 90, 20);
	Min_m->setPalette(spalette);
	Min_m->setRange(50, 250);
	Min_m->setSliderPosition(ccOpt->mean_min);
	
	Max_m = new QSlider(Qt::Horizontal, this);
	Max_m->setTickPosition(QSlider::TicksBelow);
	Max_m->setGeometry(420, 120, 90, 20);
	Max_m->setPalette(spalette);
	Max_m->setRange(100, 700);
	Max_m->setSliderPosition(ccOpt->mean_max);
	
	Min_b = new QSlider(Qt::Horizontal, this);
	Min_b->setTickPosition(QSlider::TicksBelow);
	Min_b->setGeometry(305, 184, 90, 20);
	Min_b->setPalette(spalette);
	Min_b->setRange(50, 200);
	Min_b->setSliderPosition(ccOpt->back_min);
	
	Max_b = new QSlider(Qt::Horizontal, this);
	Max_b->setTickPosition(QSlider::TicksBelow);
	Max_b->setGeometry(420, 184, 90, 20);
	Max_b->setPalette(spalette);
	Max_b->setRange(100, 700);
	Max_b->setSliderPosition(ccOpt->back_max);
	
	
	QLabel *Lin_m = new QLabel(this);
	Lin_m->setGeometry(293, 100, 108, 18);
	Lin_m->setText("Min peak value");
	Lin_m->setPalette(spalette);
	Lin_m->setAutoFillBackground(true);
	Lin_m->setAlignment(Qt::AlignHCenter);
	
	QLabel *Lax_m = new QLabel(this);
	Lax_m->setGeometry(410, 100, 108, 18);
	Lax_m->setText("Max peak value");
	Lax_m->setPalette(spalette);
	Lax_m->setAutoFillBackground(true);
	Lax_m->setAlignment(Qt::AlignHCenter);
	
	QLabel *Lin_b = new QLabel(this);
	Lin_b->setGeometry(300, 162, 96, 18);
	Lin_b->setText("Min Fit range");
	Lin_b->setPalette(spalette);
	Lin_b->setAutoFillBackground(true);
	Lin_b->setAlignment(Qt::AlignHCenter);
	
	QLabel *Lax_b = new QLabel(this);
	Lax_b->setGeometry(415, 162, 96, 18);
	Lax_b->setText("Max Fit Range");
	Lax_b->setPalette(spalette);
	Lax_b->setAutoFillBackground(true);
	Lax_b->setAlignment(Qt::AlignHCenter);
	
	connect(Min_m, SIGNAL( valueChanged(int) ), this, SLOT( load_gui_fit_parameters() ) );
	connect(Max_m, SIGNAL( valueChanged(int) ), this, SLOT( load_gui_fit_parameters() ) );
	connect(Min_b, SIGNAL( valueChanged(int) ), this, SLOT( load_gui_fit_parameters() ) );
	connect(Max_b, SIGNAL( valueChanged(int) ), this, SLOT( load_gui_fit_parameters() ) );
	
	if(CC_calib->open_spe_b) { update_table_display(); }
	
}


void SPE_Display_W::fit_spe(int what=0)
{
	int runno = CC_calib->runno;
	string filename = Form("spe_fits_%d.pdf", runno);
//	TPostScript  *ps = NULL;
//	
//	if(PRINT)
//	{
//		ps = new TPostScript(filename.c_str(), 111);
//		ps->NewPage();
//	}
	
	// progress dialog (if fitting all channels)
	int i    = 1;
	int prog = 0;
	QProgressDialog *speprogress = NULL;
	
	if(what == 0) Reset_SPE_sliders();
	WLOG = "";
	if(!CC_calib->open_spe_b)
	{
		WLOG += "\nNo ROOT SPE file opened. ";
		wlog->setText(WLOG);
	}
	else
	{
		// setting choice of one or all channels
		int min_ch, max_ch;
		int min_s,  max_s;
		min_ch = 1;
		max_ch = 36;
		min_s  = 1;
		max_s  = 6;
		
		if(what == 1)
		{
			min_ch = max_ch = table->currentRow()    + 1;
			min_s  = max_s  = table->currentColumn() + 1;
			WLOG += QString("\nFitting Channel %2 - Sector %1...\n").arg(min_s).arg(min_ch);
		}
		else if(what == 0)
		{
			CC_calib->fit_all = 1;
			min_ch = 1;
			max_ch = 36;
			min_s  = 1;
			max_s  = 6;
			speprogress = new QProgressDialog("Fitting/Printing SPE positions ...",  "Stop", 0, 216, 0);
			speprogress->setWindowModality(Qt::WindowModal);
			speprogress->setValue(prog);
			speprogress->setMinimumWidth(320);
			if(!PRINT) WLOG += "\nFitting All Channels...\n";
			if(PRINT)  WLOG += "\nPrinting All Channels...\n";
		}
		wlog->setText(WLOG);
		
		Spe->Clear();
		if(what == 1) Spe->Divide(1, 1);
		else          Spe->Divide(2, 2);
		
    if(PRINT) 
    {
      Spe->cd(1);
      Spe->Print("a.pdf(");
      Spe->Update();
    }
		for(int s=min_s-1; s<max_s; s++)
			for(int ss=min_ch-1; ss<max_ch; ss++)
			{
        
				if(what == 0) if(speprogress->wasCanceled()) break;
				if(what == 0) speprogress->setValue(prog++);
				Spe->cd(i);
				if(!PRINT) CC_calib->fit_spe(s,ss);
				if((CC_calib->chi2df[s][ss] > 5 || CC_calib->mean[s][ss] > 250) && CC_calib->fit_all && !PRINT)
				{
					cout << " Chi2 is too big - refining fit... " << endl;
					CC_calib->fit_all = 0;
					CC_calib->fit_spe(s, ss);
					CC_calib->fit_all = 1;
				}
				display_channel(s, ss);
				if(i==1) lat.DrawLatex(.34, .94, Form("Run Number: %d", CC_calib->runno));
				Spe->Update();
				if(++i == 5)
				{
					i = 1;
					//if(PRINT) ps->NewPage();
          if(PRINT) Spe->Print("a.pdf");

				}
			}
			
			CC_calib->fit_all = 0;
			if(PRINT)
			{
		//		ps->Close();
		//		delete ps;
        if(PRINT) Spe->Print("a.pdf)");
			}
			
			if(what == 0)
			{
				speprogress->setValue(216);
		//		delete speprogress;
			}
			if(!PRINT) CC_calib->processed_spe_b = TRUE;
	}
	update_table();
	if(PRINT)
	{
		WLOG += "\nSPE fits printed in file ";
		WLOG += filename.c_str();
		WLOG += "\n";
		wlog->setText(WLOG);
	}
}

void SPE_Display_W::add4()
{
	if(current_channel < 33 )
	{
		current_channel += 4;
		if(current_channel > 33) current_channel = 33;
	}
	else
	{
		sector++;
		current_channel = 1;
		if(sector == 7) sector = 1;
	}
	update_table_display();
}

void SPE_Display_W::sub4()
{
	if(current_channel > 4 ) current_channel -= 4;
	else
	{
		sector--;
		current_channel = 33;
		if(sector == 0) sector = 6;
	}
	update_table_display();
}


void SPE_Display_W::table_clicked(int row, int col)
{
	sector          = col + 1;
	current_channel = row + 1;
	if(current_channel > 33) current_channel = 33;
	
	update_table_display();
}


void SPE_Display_W::display_channels()
{
	Spe->Clear();
	Spe->Divide(2, 2);
	
	static int init;
	if(init != -99)  init = -99;
	else
	{
		load_gui_fit_parameters();
		CC_calib->Set_ParLimits();
	}
	
	int ci = 1;
	for(int t=current_channel-1; t<current_channel+3; t++)
	{
		Spe->cd(ci++);
		display_channel(sector-1, t);
		if(ci==2) lat.DrawLatex(.34, .94, Form("Run Number: %d", CC_calib->runno));
		Spe->Update();
	}
}


void SPE_Display_W::display_channel(int sector, int ch)
{
	double par[5];
	if(CC_calib->parameters[sector][ch][0] != 0) for(int p=0; p<5; p++) par[p] = CC_calib->parameters[sector][ch][p] ; // restoring values
	CC_calib->CC_ADC[sector][ch]->Draw("hist");
	if(par[0] != 0 && !CC_calib->DEAD[sector][ch])
	{
		CC_calib->BG->SetParameters(&par[0]);
		CC_calib->PS->SetParameters(&par[2]);
		
		CC_calib->Fitf->SetParameters(&par[0]);
		CC_calib->Fitf->Draw("same");
		CC_calib->BG->Draw("same");
		CC_calib->PS->Draw("same");
		
		lat.SetTextColor(1);
		lat.DrawLatex(.54, .84, Form("<ADC>: %4.1f",    CC_calib->MEAN[sector][ch]));
		lat.SetTextColor(2);
		lat.DrawLatex(.48, .78, Form("Fit: %4.1f      #pm %3.1f", CC_calib->mean[sector][ch], CC_calib->emean[sector][ch]));
		lat.SetTextColor(1);
	}
	
	if(CC_calib->DEAD[sector][ch])
	{
		lat.DrawLatex(.48, .80, "WARNING:");
		lat.DrawLatex(.48, .72, "DEAD CHANNEL");
	}
	if(fabs(CC_calib->mean[sector][ch] - CC_calib->spe_value[sector][ch]) > 0.6)
	{
		lat.SetTextColor(2);
		lat.DrawLatex(.48, .64, Form("SPE Value: %4.1f",  CC_calib->spe_value[sector][ch]));
		lat.SetTextColor(1);
	}
	if(par[0] != 0 &&  !CC_calib->DEAD[sector][ch] && fabs(CC_calib->mean[sector][ch] - CC_calib->spe_value[sector][ch]) < 0.6)
	{
		lat.DrawLatex(.56, .70, Form("#chi^{2}/ n.d.f.: %4.1f",  CC_calib->chi2df[sector][ch]));
		if(show_formula)
		{
			lat.SetTextColor(9);
			lat.DrawLatex(.63, .53, "A#frac{#mu^{#frac{x}{P}}}{#Gamma(#frac{x}{P}+1)} e^{- #mu}");
			lat.DrawLatex(.67, .48, Form("#mu = %4.1f", CC_calib->parameters[sector][ch][3]));
			lat.DrawLatex(.67, .43, Form("P = %4.1f", CC_calib->parameters[sector][ch][4]));
			lat.SetTextColor(1);
		}
	}
	
}

void SPE_Display_W::update_table_display()
{
	update_table();
	display_channels();
}


void SPE_Display_W::update_table()
{
	QString CH =  "Display\n";
	CH += QString("S%1  %2-%3").arg(sector).arg(current_channel).arg(current_channel+3);
	display4->setText(CH);
	CH =  "Fit Again\n";
	CH += QString("Ch %2 - Sect %1").arg(sector).arg(table->currentRow()+1);
	fit_again->setText(CH);
	
	WLOG = wlog->text();
	static int init;
	if(init != -99)
	{
		WLOG +=  "\nDisplaying:\n";
		WLOG += QString("Sector %1    Channel %2-%3").arg(sector).arg(current_channel).arg(current_channel + 3);
		init = -99;
	}
	else
	{
		WLOG  =  "Displaying:\n";
		WLOG += QString("Sector %1    Channel %2-%3").arg(sector).arg(current_channel).arg(current_channel + 3);
	}
	
	wlog->setText(WLOG);
	
	for(int s=0; s<6; s++)
		for(int ss=0; ss<36; ss++)
		{
			if(CC_calib->spe_value[s][ss] != 0) table->item(ss, s)->setText(QString("%1").arg(CC_calib->spe_value[s][ss], 0, 'f', 1));
			if(CC_calib->spe_value[s][ss] == 0) table->item(ss, s)->setText("");
			if(CC_calib->DEAD[s][ss]      != 0) table->item(ss, s)->setText("DEAD");
		}
}



void SPE_Display_W::table_changed(int row, int col)
{
	QString str = table->item(row, col)->text();
	WLOG = "";
	WLOG += QString("\nChanging value of Channel %2 - Sector %1  to ").arg(col+1).arg(row+1);
	WLOG += str;
	wlog->setText(WLOG);
	CC_calib->spe_value[col][row] = atof(str.toStdString().c_str());
	if(str == "DEAD") CC_calib->spe_value[col][row] = 200;
}

void SPE_Display_W::Reset_SPE_sliders()
{
	CC_calib->reset_pars();
	
	Min_m->setSliderPosition(ccOpt->mean_min);
	Max_m->setSliderPosition(ccOpt->mean_max);
	Min_b->setSliderPosition(ccOpt->back_min);
	Max_b->setSliderPosition(ccOpt->back_max);
	
	WLOG = "Resetting Parameters Limits. ";
	WLOG += "\nGaussian + Exponential Fit:\n";
	WLOG += QString("Min peak value:  %1       Max peak value:  %2\n").arg(  ccOpt->mean_min, 3, 10).arg( ccOpt->mean_max, 0, 10);
	WLOG += QString("Min fit range:  %1            Max fit range:  %2").arg( ccOpt->back_min, 3, 10).arg( ccOpt->back_max, 0, 10);
	wlog->setText(WLOG);
}

void SPE_Display_W::load_gui_fit_parameters()
{
	ccOpt->mean_min = Min_m->value();
	ccOpt->mean_max = Max_m->value();
	ccOpt->back_min = Min_b->value();
	ccOpt->back_max = Max_b->value();
	
	WLOG = "Channel ";
	WLOG += stringify(current_channel).c_str();
	WLOG += " - Sector ";
	WLOG += stringify(sector).c_str();
	WLOG += "\nGaussian + Exponential Fit:\n";
	WLOG += QString("Min peak value:  %1       Max peak value:  %2\n").arg(  ccOpt->mean_min, 3, 10).arg( ccOpt->mean_max,0, 10);
	WLOG += QString("Min fit range:  %1            Max fit range:  %2").arg( ccOpt->back_min, 3, 10).arg( ccOpt->back_max,0, 10);
	wlog->setText(WLOG);
}


void SPE_Display_W::print_all_channels()
{
	int runno = CC_calib->runno;
	string filen_top = Form("spe_fit_%d", runno);
	
	// progress dialog (if fitting all channels)
	int prog = 0;
	QProgressDialog *speprogress = NULL;
	
	WLOG = "";
	if(!CC_calib->open_spe_b)
	{
		WLOG += "\nNo ROOT SPE file opened. ";
		wlog->setText(WLOG);
	}
	else
	{
		WLOG += "\nPrinting All Fits in gif Files...\n";
		wlog->setText(WLOG);
		speprogress = new QProgressDialog("Priting SPE Fits in Gif Files...", "Stop Printing", 0, 216, 0);
		speprogress->setWindowModality(Qt::WindowModal);
		speprogress->setValue(prog);
		speprogress->setMinimumWidth(320);
		
		Spe->Clear();
		Spe->Divide(1, 1);
		for(int s=0; s<6; s++)
			for(int ss=0; ss<36; ss++)
			{
				if(speprogress->wasCanceled()) break;
				speprogress->setValue(prog++);
				Spe->cd();
				display_channel(s, ss);
				lat.DrawLatex(.3, .92, Form("Run Number: %d", CC_calib->runno));
				lat.SetTextColor(1);
				Spe->Update();
				string gifname = filen_top + "_s" + stringify(s+1) + "_ch" + stringify(ss+1) + ".gif";
				Spe->Print(gifname.c_str());
			}
			speprogress->setValue(216);
			delete speprogress;
	}
	
	update_table();
	WLOG += "\n\nSPE fits Printed in gif files.\n";
	wlog->setText(WLOG);
}







