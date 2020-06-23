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
#include "TPDF.h"

// %%%%%%%%%%%
// C++ headers
// %%%%%%%%%%%
#include <cmath>

SPE_Display_W :: SPE_Display_W(QWidget *mom, QSplashScreen* spl, QApplication *ecg, 
                               tpecal_opts *op, tpecal_calib *tec, TCanvas *Sp, QLabel* La) : QWidget(mom)
{
	splash       = spl;
	tpecal_gui   = ecg;
	tpecalOpt    = op;
	TPECal_calib = tec;
	Spe          = Sp;
	wlog         = La;
	
	lat.SetTextAlign(0);
	lat.SetNDC();
	
	current_channel = 1;
	PRINT           = 0;
	
	splash->showMessage(" Initializing SPE Display GUI...\n");
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
	
	display6 = new QPushButton("Display\nS1  1-6", this);
	display6->setGeometry(108, 297, 80, 36);
	
	QPushButton *display6_back = new QPushButton( "<< 6", this);
	display6_back->setGeometry(28, 297, 80 , 36);
	
	QPushButton *display6_forw = new QPushButton( "6 >>", this);
	display6_forw->setGeometry(188, 297, 80 , 36);
	
	fit_again = new QPushButton( "Fit again", this);
	fit_again->setGeometry(300, 10, 100 , 40);
	
	QPushButton *reset_par = new QPushButton("Reset pars", this);
	reset_par->setGeometry(410, 10, 100 , 40);
	
	connect( display6_forw, SIGNAL( clicked()              ), this, SLOT( add6()                   ) );
	connect( display6_back, SIGNAL( clicked()              ), this, SLOT( sub6()                   ) );
	connect( display6     , SIGNAL( clicked()              ), this, SLOT( update_table_display()   ) );
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
	Min_m->setSliderPosition(tpecalOpt->mean_min);
	
	Max_m = new QSlider(Qt::Horizontal, this);
	Max_m->setTickPosition(QSlider::TicksBelow);
	Max_m->setGeometry(420, 120, 90, 20);
	Max_m->setPalette(spalette);
	Max_m->setRange(100, 700);
	Max_m->setSliderPosition(tpecalOpt->mean_max);
	
	Min_b = new QSlider(Qt::Horizontal, this);
	Min_b->setTickPosition(QSlider::TicksBelow);
	Min_b->setGeometry(305, 184, 90, 20);
	Min_b->setPalette(spalette);
	Min_b->setRange(50, 200);
	Min_b->setSliderPosition(tpecalOpt->back_min);
	
	Max_b = new QSlider(Qt::Horizontal, this);
	Max_b->setTickPosition(QSlider::TicksBelow);
	Max_b->setGeometry(420, 184, 90, 20);
	Max_b->setPalette(spalette);
	Max_b->setRange(100, 700);
	Max_b->setSliderPosition(tpecalOpt->back_max);
	
	
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
	
	if(TPECal_calib->open_mip_b) { update_table_display(); }
	
	// Pedestal Widget
	QWidget *which_hist = new QWidget(this);
	which_hist->setGeometry(320, 200, 120, 120);
	
	QVBoxLayout *which = new QVBoxLayout;
	all_adc = new QRadioButton("All ADCs");
	all_adc->setChecked( TRUE );
	all_adc->setPalette(bpalette);
	
	single_adc = new QRadioButton("No Neighbors ADCs");
	single_adc->setPalette(bpalette);
		
	all_tdc = new QRadioButton("All TDCs");
	all_tdc->setPalette(bpalette);
	
	
	which->addWidget(all_adc);
	which->addWidget(single_adc);
	which->addWidget(all_tdc);
	
	which_hist->setLayout(which);

	connect ( all_adc,      SIGNAL( pressed() ), this, SLOT( hist_choice() ) );
	connect ( single_adc,   SIGNAL( pressed() ), this, SLOT( hist_choice() ) );
	connect ( all_tdc,      SIGNAL( pressed() ), this, SLOT( hist_choice() ) );

	
}

void SPE_Display_W::hist_choice()
{
	if(all_adc->isDown())     which_histos = 0;
	if(single_adc->isDown())  which_histos = 1;
	if(all_tdc->isDown())     which_histos = 2;
}



void SPE_Display_W::fit_spe(int what=0)
{
	int runno = TPECal_calib->runno;
	string filename = Form("mip_fits_%d.pdf", runno);

	
	if(what == 0) Reset_SPE_sliders();
	WLOG = "";
	if(!TPECal_calib->open_mip_b)
	{
		WLOG += "\nNo ROOT SPE file opened. ";
		wlog->setText(WLOG);
	}
	else
	{
		// setting choice of one or all channels
		int min_ch, max_ch;
		int min_la, max_la;
		min_ch = 1;
		max_ch = 30;
		
		if(what == 1)
		{
			min_ch = max_ch = table->currentRow()    + 1;
			min_la = max_la = table->currentColumn() + 1;
			WLOG += QString("\nFitting Channel %2 - Layer %1...\n").arg(min_la).arg(min_ch);
		}
		else if(what == 0)
		{
			TPECal_calib->fit_all = 1;
			if(!PRINT) WLOG += "\nFitting All Channels...\n";
			if(PRINT)  WLOG += "\nPrinting All Channels...\n";
		}
		wlog->setText(WLOG);
		
		Spe->Clear();
		if(what == 1) Spe->Divide(1, 1);
		else          Spe->Divide(2, 3);
		
		int i    = 1;
		for(int ss=min_ch-1; ss<max_ch; ss++)
		{
			Spe->cd(i);
			//if(!PRINT) TPECal_calib->fit_spe(ss);
			
			display_channel(ss);
			if(i==1) lat.DrawLatex(.34, .94, Form("Run Number: %d", TPECal_calib->runno));
			Spe->Update();
			if(++i == 5)
			{
				i = 1;
				string pdf;
				if(ss==5)  pdf = filename + "(";
				if(ss==30) pdf = filename + ")";
				//if(PRINT) Spe->Print(pdf.c_str(), "Title");

			}
		}
		
		TPECal_calib->fit_all = 0;
		
		if(!PRINT) TPECal_calib->processed_mip_b = TRUE;
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

void SPE_Display_W::add6()
{
	if(current_channel < 24 )
	{
		current_channel += 6;
		if(current_channel > 30) current_channel = 25;
	}
	else
	{
		current_channel = 1;
	}
	update_table_display();
}

void SPE_Display_W::sub6()
{
	if(current_channel > 6 ) 
	{
		current_channel -= 6;
		if(current_channel < 1) current_channel = 1;
	}
	else
	{
		current_channel = 25;
	}
	update_table_display();
}


void SPE_Display_W::table_clicked(int row, int col)
{
	
	current_channel = row*6 + col + 1;
	if(current_channel > 30) current_channel = 30;
	
	update_table_display();
}


void SPE_Display_W::display_channels()
{
	int MIPS = (int) tpecalOpt->args["PROCESS_MIP"].arg;
	int BEAM = (int) tpecalOpt->args["PROCESS_BEAM"].arg;
	int logy = (int) tpecalOpt->args["LOGY"].arg;

	Spe->Clear();
	Spe->ForceUpdate();

	
	if(MIPS)
		Spe->Divide(2, 3);
	if(BEAM)
		Spe->Divide(6, 5);
	
//	static int init;
//	if(init != -99)  init = -99;
//	else
//	{
//		load_gui_fit_parameters();
//		TPECal_calib->Set_ParLimits();
//	}
	
	int ci = 1;
	int chan=current_channel;
	if(chan > 25) chan = 25;
	
	int min = 0;
	int max = 30;
	if(MIPS){ min = chan-1 ; max = chan+5;}
	for(int t=min; t<max; t++)
	{
		Spe->cd(ci);
	//	gPad->SetLogy(logy);
		
		display_channel(t);
//		if(ci==2) lat.DrawLatex(.34, .94, Form("Run Number: %d", TPECal_calib->runno));
		Spe->Update();
		ci++;
	}
}


void SPE_Display_W::display_channel(int ch)
{
	int logy = (int) tpecalOpt->args["LOGY"].arg;
	double par[5];
	gPad->Clear();
	
	if(which_histos == 0)
	{
		TPECal_calib->TPECal_ADC[ch]->Draw("hist");
	}
	if(which_histos == 1)
	{
		TPECal_calib->TPECal_ADC_Single[ch]->Draw();
	}

//	TH1F *hist = 0;
	
//	if(TPECal_calib->parameters[ch][0] != 0) 
//		for(int p=0; p<5; p++) par[p] = TPECal_calib->parameters[ch][p] ; // restoring values
	
//	if(which_histos == 0)  hist = TPECal_calib->TPECal_ADC[ch];
//	if(which_histos == 1)  hist = TPECal_calib->TPECal_ADC_Single[ch];
//	if(which_histos == 2)  hist = TPECal_calib->TPECal_TDC[ch];
	
//	if(logy)
//	{
//			hist->SetMinimum(0.001);
//			TPECal_calib->TPECal_ADC_Single[ch]->SetMinimum(0.001);
//	}
//		
//	hist->Draw("hist");
	

//	if(which_histos == 0) 
		//TPECal_calib->TPECal_ADC_Single[ch]->Draw("samehist");

	
//	if(par[0] != 0 && !TPECal_calib->DEAD[ch])
	{
	//	TPECal_calib->BG->SetParameters(&par[0]);
	//	TPECal_calib->PS->SetParameters(&par[2]);
		
	//	TPECal_calib->Fitf->SetParameters(&par[0]);
		TPECal_calib->Fitf->Draw("same");
	//	TPECal_calib->BG->Draw("same");
//		TPECal_calib->PS->Draw("same");
		
//		lat.SetTextColor(1);
//		lat.DrawLatex(.53, .84, Form("<ADC>: %4.1f",    TPECal_calib->MEAN[ch]));
		//lat.DrawLatex(.55, .82, Form("<TDC>: %4.1f",    TPECal_calib->TPECal_TDC[ch]->GetRMS()));
//		lat.DrawLatex(.535, .76, Form("N Entries %4.0f", hist->GetEntries()));
		// lat.SetTextColor(2);
//		lat.DrawLatex(.48, .78, Form("Fit: %4.1f      #pm %3.1f", TPECal_calib->mean[ch], TPECal_calib->emean[ch]));
//		lat.SetTextColor(1);
	}
	
//	if(TPECal_calib->DEAD[ch])
	{
//		lat.SetTextColor(2);
//		lat.DrawLatex(.48, .84, "WARNING:");
//		lat.DrawLatex(.48, .76, "DEAD CHANNEL");
//		lat.SetTextColor(1);
	}
//	if(fabs(TPECal_calib->mean[ch] - TPECal_calib->mip_value[ch]) > 0.6)
	{
		//lat.SetTextColor(2);
		//lat.DrawLatex(.48, .64, Form("MIP Value: %4.1f",  TPECal_calib->mip_old[ch]));
		//lat.SetTextColor(1);
	}
}

void SPE_Display_W::update_table_display()
{
	//update_table();
	display_channels();
}


void SPE_Display_W::update_table()
{
  int i, j;
	QString CH =  "Display\n";
	CH += QString("Ch %2-%3").arg(current_channel).arg(current_channel+5);
	display6->setText(CH);
	CH =  "Fit Again\n";
	CH += QString("Ch %2").arg(current_channel);
	fit_again->setText(CH);
	
	WLOG = wlog->text();
	static int init;
	if(init != -99)
	{
		WLOG +=  "\nDisplaying:\n";
		WLOG += QString("Channel %2-%3").arg(current_channel).arg(current_channel + 5);
		init = -99;
	}
	else
	{
		WLOG  =  "Displaying:\n";
		WLOG += QString("Channel %2-%3").arg(current_channel).arg(current_channel + 5);
	}
	
	wlog->setText(WLOG);
	
	for(int ss=0; ss<30; ss++)
	{
		i = TPECal_calib->jrow(ss);
		j = TPECal_calib->jcol(ss);
		if(TPECal_calib->mip_value[ss] != 0) table->item(i, j)->setText(QString("%1").arg(TPECal_calib->mip_value[ss], 0, 'f', 1));
		if(TPECal_calib->mip_value[ss] == 0) table->item(i, j)->setText("");
		if(TPECal_calib->DEAD[ss]      != 0) table->item(i, j)->setText("DEAD");
	}
}



void SPE_Display_W::table_changed(int row, int col)
{
	int j = row*5+col;
	QString str = table->item(row, col)->text();
	WLOG = "";
	WLOG += QString("\nChanging value of Channel %2 - to ").arg(j);
	WLOG += str;
	wlog->setText(WLOG);
	TPECal_calib->mip_value[j] = atof(str.toStdString().c_str());
	if(str == "DEAD") TPECal_calib->mip_value[j] = 200;
}

void SPE_Display_W::Reset_SPE_sliders()
{
	TPECal_calib->reset_pars();
	
	Min_m->setSliderPosition(tpecalOpt->mean_min);
	Max_m->setSliderPosition(tpecalOpt->mean_max);
	Min_b->setSliderPosition(tpecalOpt->back_min);
	Max_b->setSliderPosition(tpecalOpt->back_max);
	
	WLOG = "Resetting Parameters Limits. ";
	WLOG += "\nGaussian + Exponential Fit:\n";
	WLOG += QString("Min peak value:  %1    Max peak value:  %2\n").arg(tpecalOpt->mean_min, 3, 10).arg( tpecalOpt->mean_max, 0, 10);
	WLOG += QString("Min fit range:  %1       Max fit range:  %2").arg( tpecalOpt->back_min, 3, 10).arg( tpecalOpt->back_max, 0, 10);
	wlog->setText(WLOG);
}

void SPE_Display_W::load_gui_fit_parameters()
{
	tpecalOpt->mean_min = Min_m->value();
	tpecalOpt->mean_max = Max_m->value();
	tpecalOpt->back_min = Min_b->value();
	tpecalOpt->back_max = Max_b->value();
	
	WLOG = "Channel ";
	WLOG += stringify(current_channel).c_str();
	WLOG += " (Layer) ";
	WLOG += stringify(current_channel/6+1).c_str();
	wlog->setText(WLOG);
}


void SPE_Display_W::print_all_channels()
{
	int logy = (int) tpecalOpt->args["LOGY"].arg;
	int runno = TPECal_calib->runno;
	string whist[3] = {"adcs", "", "tdcs"};
				
	string filen_top = Form("mip_%d", runno);
	
	WLOG = "";
	{
		WLOG += "\nPrinting All Fits in gif Files...\n";
		wlog->setText(WLOG);
		
		Spe->Clear();
		Spe->Divide(2, 3);
		int layer = 1;
		for(int ss=0; ss<30; ss++)
		{
			Spe->cd(ss%6+1);
			gPad->SetLogy(logy);

			display_channel(ss);
			if(ss%6 == 0) 
			{
				lat.SetTextColor(kRed+2);
				lat.DrawLatex(.3, .94, Form("Run Number: %d", TPECal_calib->runno));
				lat.SetTextColor(kBlack);
			}
			Spe->Update();
			if(ss%6 == 5)
			{
				string gifname = filen_top  + "_layer" + stringify(layer) + "_" + whist[which_histos] + ".gif";
				layer++;
				Spe->Print(gifname.c_str());
			}
		}
	}
	
	update_table();
	WLOG += "\n\nMIP fits Printed in gif files.\n";
	wlog->setText(WLOG);
}







