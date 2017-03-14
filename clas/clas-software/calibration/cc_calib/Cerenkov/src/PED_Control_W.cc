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
#include "PED_Control_W.h"
#include "icon_explana2.h"

// %%%%%%%%%%%%%
// ROOT headers
// %%%%%%%%%%%%%
#include "TPostScript.h"

// %%%%%%%%%%%
// C++ headers
// %%%%%%%%%%%
#include <cmath>

PED_Control_W :: PED_Control_W(QWidget *mom, QSplashScreen* spl, QApplication *ccg, cc_opts *op, cc_calib *ccc, TCanvas *Sp, QLabel* La) : QWidget(mom)
{
 splash   = spl;
 cc_gui   = ccg;
 ccOpt    = op;
 CC_calib = ccc;
 Spe      = Sp;
 wlog     = La;

 PRINT = 0;

 me.SetTextAlign(0);
 me.SetNDC();

 splash->showMessage(" Initializing PED Display GUI...\n"); 
 cc_gui->processEvents();

 // Setting Geometry
 QPalette palette ;
 palette.setBrush(backgroundRole(), QBrush(QPixmap(explana2)));
 setGeometry(160, 134, 520, 340);
 setPalette(palette);
 setAutoFillBackground(true);

 QPushButton *MainButtons[5];
 string buttons[5] = {
                      "Show peds position",
                      "Print peds position",
                      "Print peds fits",
                      "Write pars to file",
                      "Put ped values in db"
                     };

 QWidget *MButtonW = new QWidget(this);
 MButtonW->setGeometry(2, 2, 160, 350);
 QVBoxLayout *VMainButtons = new QVBoxLayout();
 for(int i=0; i<5; i++)
 {
    MainButtons[i] = new QPushButton(buttons[i].c_str());
    VMainButtons->addWidget(MainButtons[i]);
 }
 MButtonW->setLayout(VMainButtons);

 connect ( MainButtons[0],  SIGNAL( clicked() ), this, SLOT( show_peds()      ) );
 connect ( MainButtons[1],  SIGNAL( clicked() ), this, SLOT( print_peds()     ) );
 connect ( MainButtons[2],  SIGNAL( clicked() ), mom,  SLOT( print_all_peds() ) );
 connect ( MainButtons[3],  SIGNAL( clicked() ), this, SLOT( write_pars()     ) );
 connect ( MainButtons[4],  SIGNAL( clicked() ), mom,  SLOT( write_ped_db()   ) );

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
 minRun->display(CC_calib->min_runno);

 maxRun = new QLCDNumber(RunInfo);
 maxRun->setGeometry(170, 60, 90, 30);
 maxRun->setFrameStyle(0);
 maxRun->setPalette(Palette);
 maxRun->setAutoFillBackground(true);
 maxRun->setSegmentStyle( QLCDNumber::Filled );
 maxRun->display(CC_calib->max_runno);


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
 runIndex->setText(ccOpt->args["RUN_INDEX"].args.c_str());

}

void PED_Control_W::show_peds()
{
 int runno       = CC_calib->runno;

 TH1F *hr ;
 Spe->Clear();
 TPad *spad = new TPad("spad", "spad", 0.02, 0.02, 0.98, 0.9);
 spad->Divide(2, 3);
 spad->Draw();

 CC_calib->ped_graphs(1);  ///< builds  TGraphErrors

 // determining max of graphs
 float max = 0;
 for(int s=0; s<6; s++)
   for(int ss=0; ss<36; ss++)
      if(fabs(CC_calib->ped_value[s][ss]) > max) max = fabs(CC_calib->ped_value[s][ss]);
 max = max*1.2;

 float min = 100;
 for(int s=0; s<6; s++)
   for(int ss=0; ss<36; ss++)
      if(fabs(CC_calib->ped_value[s][ss]) < min) min = fabs(CC_calib->ped_value[s][ss]);
 min = min*0.8;

 for(int s=0; s<6; s++)
 {
    spad->cd(s+1);
    hr = gPad->DrawFrame(0, min, 36, max);
    hr->SetXTitle(Form("Sector %d                   ", s+1));
    hr->SetTitleOffset(1.0);
    hr->SetTitleSize(.09);
    hr->SetLabelSize(.065, "XY");
    CC_calib->PMeanO[s]->Draw("LP");
    CC_calib->PMean[s]->Draw("LP");
 }
 Spe->cd();

 me.SetTextColor(1);
 me.SetTextSize(0.040);
 me.DrawLatex(.10, .94, "Pedestals ");
 me.DrawLatex(.15, .90, Form("Run %d", CC_calib->runno));
 me.SetTextSize(0.031);
 me.SetTextColor(2);
 me.DrawLatex(.75, .95, "#diamond DB Values ");
 me.SetTextColor(4);
 me.DrawLatex(.75, .91, "#diamond Fit Values ");

 Spe->Update();


 if(PRINT)
    Spe->Print(Form("ped_positions_%d.gif", runno));

 CC_calib->ped_graphs(0); ///< deletes  TGraphErrors
 delete hr;
 delete spad;

 QString WLOG = "";
 if(!PRINT) WLOG += "Showing Pedestals...\n";
 if(PRINT)
 {
	 WLOG += "Printing Pedestals in file ped_positions.gif\n";
   WLOG += "\n";
 }

 wlog->setText(WLOG);
}

void PED_Control_W::write_pars()
{
 QString WLOG = "";
 WLOG += "\n Writing parameters to ROOT file ";
 WLOG +=  CC_calib->rfile.c_str();
 WLOG += "\n Writing pedestals to dat file ";
 WLOG +=  CC_calib->datfile.c_str();
 wlog->setText(WLOG);

 CC_calib->write_ped_pars();

}

