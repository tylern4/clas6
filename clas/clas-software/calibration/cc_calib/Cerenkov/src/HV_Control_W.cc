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
#include "HV_Control_W.h"
#include "icon_explana2.h"

// %%%%%%%%%%%%%
// ROOT headers
// %%%%%%%%%%%%%
#include "TLine.h"
#include "TPostScript.h"

// %%%%%%%%%%%
// C++ headers
// %%%%%%%%%%%
#include <cmath>

HV_Control_W :: HV_Control_W(QWidget *mom, QSplashScreen* spl, QApplication *ccg, cc_opts *op, cc_calib *ccc, TCanvas *Sp, QLabel* La) : QWidget(mom)
{
 splash   = spl;
 cc_gui   = ccg;
 ccOpt    = op;
 CC_calib = ccc;
 Spe      = Sp;
 wlog     = La;

 splash->showMessage(" Initializing HV Control GUI...\n");
 cc_gui->processEvents();

 current_channel = 1;
 sector          = 1;
 PRINT           = 0;

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
 table->setGeometry(5, 5, 320, 300);
 QStringList sl;
 sl << "S 1" << "S 2" << "S 3" << "S 4" << "S 5" << "S 6";
 table->setHorizontalHeaderLabels(sl);
 table->setPalette(bpalette);
 for(int s=0; s<6; s++)
    for(int ss=0; ss<36; ss++)
       table->setItem(ss,s,new QTableWidgetItem());
 table->setFont( QFont( "lucida", 8) );

 for(int r=0;r<36; r++) table->setRowHeight(r, 18);
 for(int r=0;r<6; r++)  table->setColumnWidth(r, 46);


 QPushButton *MainButtons[6];
 string buttons[6] = {
                      "Show Current HV",
                      "Show New HV",
                      "Show HV Correction",
                      "Print HV Graphs",
                      "Put HV values in db",
                      "Write Snap File"
                     };

 QWidget *MButtonW = new QWidget(this);
 MButtonW->setGeometry(360, 10, 150, 310);
 QVBoxLayout *VMainButtons = new QVBoxLayout();
 for(int i=0; i<6; i++)
 {
    MainButtons[i] = new QPushButton(buttons[i].c_str());
    VMainButtons->addWidget(MainButtons[i]);
 }
 MButtonW->setLayout(VMainButtons);

 connect ( MainButtons[0],  SIGNAL( clicked() ), this, SLOT( show_old_hv()   ) );
 connect ( MainButtons[1],  SIGNAL( clicked() ), this, SLOT( show_new_hv()   ) );
 connect ( MainButtons[2],  SIGNAL( clicked() ), this, SLOT( show_diff_hv()  ) );
 connect ( MainButtons[3],  SIGNAL( clicked() ), this, SLOT( print_hv()      ) );
 connect ( MainButtons[4],  SIGNAL( clicked() ), mom, SLOT( write_hv_db() ) );
 connect ( MainButtons[5],  SIGNAL( clicked() ), this, SLOT( write_hv_snap() ) );

}

void HV_Control_W::show_hv(int status)
{
 int runno       = CC_calib->runno;
 string filename = Form("high_voltages_%d.ps", runno);
 TPostScript *ps = NULL;

 if(PRINT)
 {
    ps = new TPostScript(filename.c_str(), 111);
    ps->NewPage();
 }

 TH1F *hr ;
 Spe->Clear();
 Spe->Divide(2, 3);

 TLine *Line = new TLine(-1, 0, 37, 0);
 Line->SetLineColor(2);

 // determining max of graphs
 float max = 0;
 for(int s=0; s<6; s++)
   for(int ss=0; ss<36; ss++)
      if(fabs(CC_calib->CC_HV_CORR[s][ss]) > max) max = fabs(CC_calib->CC_HV_CORR[s][ss]);
 max = max*1.2;

 CC_calib->hv_graphs(1);  ///< builds  TGraphErrors

 for(int s=0; s<6; s++)
 {
    Spe->cd(s+1);
    if(status != 2) hr = gPad->DrawFrame(0, 1000, 36, 2600);
    else            hr = gPad->DrawFrame(0, -max, 36, max);
    hr->SetXTitle(Form(" High Voltages Sector %d              ", s+1));
    hr->SetTitleOffset(1.58);
    hr->SetTitleSize(.06);
    if(status != 2)
    {
       CC_calib->HVMeanO[s]->Draw("LP");
       CC_calib->HVMeanN[s]->Draw("LP");
    }
    else
    {
       CC_calib->HVMeanD[s]->Draw("LP");
       Line->Draw("same");
    }
 }

 Spe->Update();

 if(PRINT)
 {
    ps->Close();
    delete ps;
 }

 CC_calib->hv_graphs(0); ///< deletes  TGraphErrors
 delete hr;

 QString WLOG = "";
 if(!PRINT && status != 2) WLOG += "Showing High Voltages...\nBlue squares: New HV.\nRed squares: original HV.\n";
 if(!PRINT && status == 2) WLOG += "Showing High Voltages Change: NEW - OLD\n";
 if(PRINT)
 {
    WLOG += "Printing High Voltages in file ";
    WLOG += filename.c_str();
    WLOG += "\n";
 }

 wlog->setText(WLOG);
 update_table(status);
}





// void HV_Control_W::write_pars()
// {
//  QString WLOG = "";
//  WLOG += "\n Writing parameters to ROOT file ";
//  WLOG +=  CC_calib->rfile.c_str();
//  WLOG += "\n Writing pedestals to dat file ";
//  WLOG +=  CC_calib->datfile.c_str();
//  wlog->setText(WLOG);
// 
//  CC_calib->write_ped_pars();
// 
// }
// 
void HV_Control_W::update_table(int status)
{
 if(status==0)
 {
    for(int s=0; s<6; s++)
      for(int ss=0; ss<36; ss++)
      {
         if(CC_calib->CC_HV_OLD[s][ss] != 0) table->item(ss, s)->setText(QString("%1").arg(CC_calib->CC_HV_OLD[s][ss], 0, 'f', 1));
         if(CC_calib->CC_HV_OLD[s][ss] == 0) table->item(ss, s)->setText("n/a");
      }
 }
 if(status==1)
 {
    for(int s=0; s<6; s++)
      for(int ss=0; ss<36; ss++)
      {
         if(CC_calib->CC_HV_NEW[s][ss] != 0) table->item(ss, s)->setText(QString("%1").arg(CC_calib->CC_HV_NEW[s][ss], 0, 'f', 1));
         if(CC_calib->CC_HV_NEW[s][ss] == 0) table->item(ss, s)->setText("n/a");
      }
 }
 if(status==2)
 {
    for(int s=0; s<6; s++)
      for(int ss=0; ss<36; ss++)
      {
         if(CC_calib->CC_HV_CORR[s][ss] != 0) table->item(ss, s)->setText(QString("%1").arg(CC_calib->CC_HV_CORR[s][ss], 0, 'f', 1));
         if(CC_calib->CC_HV_CORR[s][ss] == 0) table->item(ss, s)->setText("n/a");
      }
 }
}








