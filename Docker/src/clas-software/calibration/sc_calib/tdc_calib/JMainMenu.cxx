#include "JMainMenu.h"

using namespace std;

extern int SC_Version_Flag;

JMainMenu::ShowChannel::ShowChannel(TGraphErrors* g1_, TH1F* hist_, double* parVal_, double* parErr_):
  g1(g1_), g2(NULL), p2(NULL), hist(hist_) {
  int n;
  double *x, *y, *ex, *ey;
  char cfun [20], name[80];
  TF1* fitf = NULL;
  if (!g1) {  //  no entries for this channel
    memset (parVal, 0, sizeof(parVal));
    memset (parErr, 0, sizeof(parErr));
    parVal[1] = 0.05;
    return;
  }

  for (int i=0; i < 4; i++) 
    parVal[i] = parVal_[i];

  for (int i=0; i < 3; i++) 
    parErr[i] = parErr_[i];

  // red line for fit function
  fitf = g1-> GetFunction("pol2");
  if (fitf) fitf-> SetLineColor(2);

  TGraphErrors* gdum = (TGraphErrors*) g1-> Clone("dummy");

  n  = gdum->GetN();
  x  = gdum->GetX();            ex = gdum->GetEX();
  y  = gdum->GetY();            ey = gdum->GetEY();

  for (int i=0; i<n; i++) {
    y[i] -= (parVal[0] + x[i] * parVal[1] + parVal[3]);
  }

  strcpy (name, g1-> GetName());
  strcat (name, ".T2");

  g2 = new TGraphErrors (n, x, y, ex, ey);
  g2-> SetMarkerStyle(1);
  g2-> SetMarkerColor(4);
  g2-> SetNameTitle(name,name);

  sprintf (cfun, "%e*x*x", parVal[2]);
  p2 = new TF1 ("p2",cfun, x[0], x[n-1]);
  p2-> SetLineColor(2);

  delete gdum;
}

void JMainMenu::ShowChannel::Write(write_t what) {
  switch (what) {
  case w_hist:
    hist->Write(); break;
  case w_fitl:
    if (g1) g1->Write(); break;
  case w_poly:
    if (g2) g2->Write(); break;
  default:
    throw "JMainMenu::ShowChannel::Write, program error";
    break;
  }
}

void JMainMenu::ShowChannel::ModifyTitleSwapped(int newIndex) {
  char title [80], appendix [80];
  if(SC_Version_Flag == 2)
    sprintf (appendix, " (swap) -> %02d", newIndex%57+1);
  else{sprintf (appendix, " (swap) -> %02d", newIndex%48+1);}
  strcpy (title, hist->GetTitle());
  strcat (title, appendix);
  hist->SetTitle(title);
  strcpy (title, g1->GetTitle());
  strcat (title, appendix);
  g1->SetTitle(title);
  strcpy (title, g2->GetTitle());
  strcat (title, appendix);
  g2->SetTitle(title);
}

void JMainMenu::ShowChannel::DrawFit(int soWhat) {
  // show only effect of T2 paramter
  if (!g1) return;
  switch (soWhat) {
  case 0:
    hist-> Draw(); break;
  case 1:
    g1->Draw("AP"); break;
  case 2:
    g2->Draw("AP");
    p2->Draw("same");
    break;
  default:
    throw "JMainMenu::ShowChannel::DrawFit(int soWhat) ... programmers stupidity???\n";
  }
}

JMainMenu::JMainMenu():
  C(NULL), runNumber(0), showWhat(0), showRight(false),
  showSector(0), showParam(-1) {}

JMainMenu::~JMainMenu() {}

JMainMenu::JMainMenu(int runNumber_, char* constOutDir_):
  C(NULL), runNumber(runNumber_), 
  showWhat(1), showRight(false), showSector(0), showParam(-1) {
  if (constOutDir_) { 
    strcpy (constOutDir, constOutDir_);
    sprintf (constOutMask, "%s/T%%d%%s%%s.dat", constOutDir_);
  }
  else { 
    strcpy (constOutMask, "T%d%s%s.dat");
    memset (constOutDir, 0, sizeof(constOutDir));
  }
  strcpy (commentWhat[0], "Show raw TDC entries for each selected channel");
  strcpy (commentWhat[1], "Show fit result: t[ns] vs. TDC peak positions");
  strcpy (commentWhat[2], "Show fit result: 2nd degree polynomial parameter only");
}

void JMainMenu::SetChannel(int ichan, int noswap, int lr, TGraphErrors* g1_, TH1F* hist_,
		     double* parVal_, double* parErr_) {
  sc[ichan][lr] = new ShowChannel(g1_, hist_, parVal_, parErr_);
  if (ichan != noswap) sc[ichan][lr]->ModifyTitleSwapped (ichan);
}

void JMainMenu::SetOverview() {
  char name [80], title [80];
  for (int ipar=0; ipar<3; ipar++){
    for (int lr=0; lr<2; lr++) {
      if(SC_Version_Flag == 2){
        double x[342], y[342], ex[342], ey[342];
        for (int i=0; i<342; i++) {
          x[i]  = i;
          ex[i] = 0.;
          y[i]  = sc[i][lr]->parVal [ipar];
          ey[i] = sc[i][lr]->parErr [ipar];
        }
        sp[ipar][lr] = new TGraphErrors(342,x,y,ex,ey);
      }else{
        double x[288], y[288], ex[288], ey[288];
        for (int i=0; i<288; i++) {
          x[i]  = i;
          ex[i] = 0.;
          y[i]  = sc[i][lr]->parVal [ipar];
          ey[i] = sc[i][lr]->parErr [ipar];
        }
        sp[ipar][lr] = new TGraphErrors(288,x,y,ex,ey);
      }
      sprintf (name, "T%d%s", ipar, (lr? "r" : "l"));
      sprintf (title, "T%d %s", ipar, (lr? "right" : "left"));
      sp[ipar][lr]-> SetNameTitle(name, title);
    }
  }
}

void JMainMenu::WriteHisto(char* rootFileName) {
  TFile rf (rootFileName, "recreate");
  TDirectory* hist = new TDirectory("hist", "hist");
  TDirectory* fitl = new TDirectory("fitl", "fitl");
  TDirectory* poly = new TDirectory("poly", "poly");

  for (int i=0; i<6; i++)
    gs[i]-> Write();

  hist->cd();
  int locNumChannels = 288;
  if(SC_Version_Flag == 2)
    locNumChannels = 342;
  for (int i = 0; i < locNumChannels; i++) 
    for (int j = 0; j < 2; j++) 
      sc[i][j] -> Write(w_hist);

  fitl->cd();
  for (int i = 0; i < locNumChannels; i++) 
    for (int j = 0; j < 2; j++) 
      sc[i][j] -> Write(w_fitl);

  poly->cd();
  for (int i = 0; i < locNumChannels; i++) 
    for (int j = 0; j < 2; j++) 
      sc[i][j] -> Write(w_poly);

  //leaving procedure closes file
}

void JMainMenu::ToggleLeftRight () {
  showRight = ! showRight; 
  if (showSector) DrawChannel(showSector, showStripe);
  if (showParam >= 0) DrawOverview(showParam);
}

void JMainMenu::TogglePresentation (int newValue) {
  showWhat = newValue; 
  if (showSector) DrawChannel(showSector, showStripe);
}

void JMainMenu::InitCanvas(const char* title) {
  if (!C) {  
    C = new TCanvas ("C", title, 1200, 800);
  }
  else {
    C->Clear();
    C->SetTitle(title);
  }
}

void JMainMenu::DrawChannel(int sector, int stripe) {
  char ctitle [80];
  int s0 = 1, s1 = 48;
  if(SC_Version_Flag == 2)
    s1 = 57;
  if (stripe) 
    sprintf (ctitle, "Sector %d Stripe %d: %s - %s TDC", sector, stripe, 
	     commentWhat[showWhat], (showRight? "right" : "left"));
  else
    sprintf (ctitle, "Sector %d: %s - %s TDC", sector, 
	     commentWhat[showWhat], (showRight? "right" : "left"));
  InitCanvas(ctitle);
  showSector = sector;
  showStripe = stripe;
  showParam  = -1;
  if (stripe)    s0 = s1 = stripe;
  else           C->Divide(8,6,0.001,0.001);
 
  for (int j=s0; j <= s1; j++) {
    int index;
    if(SC_Version_Flag == 2)
      index = (sector - 1)*57 + j;
    else{index = (sector - 1)*48 + j;}
    if (!stripe) C->cd(j);
    sc [index] [showRight?1:0] -> DrawFit(showWhat);
  }
  C->Update();
}

void JMainMenu::DrawOverview(int deg) {
  char ctitle [80];
  sprintf (ctitle, "Overwiew: Parameter T%d - %s TDC", deg, (showRight? "right" : "left"));
  InitCanvas(ctitle);
  showSector = 0;
  showParam  = deg;
  sp [deg] [showRight]->Draw("AP");
  C->Update ();
}

void JMainMenu::CloseChannelMenu() {
  td->Clear();
  td->Close();
  td = NULL;
}

void JMainMenu::ChannelMenu() {
  char name[80], command [80];
  if (td) return;
  td = new TDialogCanvas ("td", "select channel", 300, 900);
  int locChannelMax = 48;
  if(SC_Version_Flag == 2)
    locChannelMax = 57;
  double locDoubleChanelMax = double(locChannelMax);

  for (int i = 0; i < locChannelMax; i++)
    for (int j = 0; j < 6; j++) {
      //      int k = j*48 + i;
      float x0 = (j+0.5)/7.;
      float x1 = (j+1.5)/7.;
      float y0 = (locDoubleChanelMax + 1.5-i)/(locDoubleChanelMax + 3.0);
      float y1 = (locDoubleChanelMax + 2.5-i)/(locDoubleChanelMax + 3.0);
      sprintf (name, "%d.%02d", j+1, i+1);
      sprintf(command, "%s0x%x%s%d%s%d%s", "((JMainMenu*)(", 
	      (unsigned long)(this), "))->DrawChannel(", j+1, ",", i+1, ")");  
      TButton* b= new TButton (name, command, x0, y0, x1, y1);
      b->Draw();
    }
  sprintf (command, "%s0x%x%s", "((JMainMenu*)(", (unsigned long) this, "))->CloseChannelMenu()");
  TButton* b= new TButton ("cancel", command, 0.3, 0.2/(locDoubleChanelMax + 3.0), 0.7, 1.3/(locDoubleChanelMax + 3.0));
  b->Draw();
}

void JMainMenu::DrawGaussHisto(int i) {
  InitCanvas(gs[i]->GetTitle());
  showSector = 0;
  showParam  = -1;
  gs[i]-> Draw("colz");

  TPaveLabel* tl = new TPaveLabel(10,340,80,370,"left");
  tl->SetBorderSize(1);
  tl->Draw();

  TPaveLabel* tr = new TPaveLabel(10,-340,80,-370,"right");
  tr->SetBorderSize(1);
  tr->Draw();
  int locChannelMax = 48;
  if(SC_Version_Flag == 2)
    locChannelMax = 57;

  for (int j=1; j< 6; j++) {
    TLine* l=new TLine( j*locChannelMax-0.5, gs[i]->GetYaxis()->GetXmin(), 
			j*locChannelMax-0.5, gs[i]->GetYaxis()->GetXmax() );
    l->SetLineStyle(3);
    l->Draw();
  }

  C->Update();
}

int JMainMenu::WriteResult() {
  char filename[80];
  int locChannelMax = 48;
  if(SC_Version_Flag == 2)
    locChannelMax = 57;

  //  double* ptr;
  for (int ipar=0; ipar<3; ipar++)  // T0 .. T2
    for (int lr=0; lr<2; lr++)      // 0 = left     1 = right 
      for (int ve=0; ve<2; ve++) {  // 0 = values   1 = errors
	sprintf (filename, constOutMask, ipar, (lr? "r" : "l"), (ve? "err":""));
	//  cout << "WriteResult <" << filename << ">\t Mask<" << constOutMask << ">" << endl;
	ofstream of(filename, ios::out);
	if (!of.good()) {
	  cerr << "Error opening "<< filename << endl;
	  return -1;
	}

	for (int i=0; i< locChannelMax; i++) {
	  if (ve) of << sc[i][lr]-> parErr[ipar] << endl;
	  else    of << sc[i][lr]-> parVal[ipar] << endl;
	}
      }
  return 0;
}

void JMainMenu::WriteDatabase() {
  char command [80];
  sprintf (command, "TDC_put.tcl %d %s &", runNumber, constOutDir);
  if (WriteResult () < 0) return;;
  gSystem->Exec(command);
}

void JMainMenu::StartMenu() {
  char command [80], text[80], comment[255];
  C->SetTitle("Processing done: Select survey histogram from menu");
  TControlBar* menu = new TControlBar("vertical","TDC-Calibration", 10, 480 );

  for (int i=0; i < 6; i++) {
    sprintf(command, "%s0x%x%s%d%s", "((JMainMenu*)(", (unsigned long) this, "))->DrawGaussHisto(", i,")" );
    menu-> AddButton (gs[i]->GetName(), command, gs[i]->GetTitle());
  }

  menu-> AddSeparator();
  for (int i=0; i < 3; i++) {
    sprintf(command, "%s0x%x%s%d%s", "((JMainMenu*)(", (unsigned long) this, "))->DrawOverview(",i,")" );
    sprintf(text, "T%d", i);
    sprintf(comment, "Show paramter T%d including error bars", i);
    menu-> AddButton (text, command, comment);
  }

  menu-> AddSeparator();
  sprintf(command, "%s0x%x%s", "((JMainMenu*)(", (unsigned long) this, "))->ToggleLeftRight()" );
  menu-> AddButton ("Option: toggle L/R", command, "Toggle fit view between left and right channel");
  
  sprintf(command, "%s0x%x%s", "((JMainMenu*)(", (unsigned long) this, "))->TogglePresentation(0)" );  
  menu-> AddButton ("Option: raw histo", command, commentWhat[0]);

  sprintf(command, "%s0x%x%s", "((JMainMenu*)(", (unsigned long) this, "))->TogglePresentation(1)" );  
  menu-> AddButton ("Option: show fit", command, commentWhat[1]);
		     
  sprintf(command, "%s0x%x%s", "((JMainMenu*)(", (unsigned long) this, "))->TogglePresentation(2)" );  
  menu-> AddButton ("Option: 2deg par", command, commentWhat[2]);

  menu-> AddSeparator();

  for (int i=1; i <= 6; i++) {
    sprintf(command, "%s0x%x%s%d%s", "((JMainMenu*)(", (unsigned long) this, "))->DrawChannel(", i,",0)" );
    sprintf(text,    "Result sector %d", i);
    sprintf(comment, "Show plots according to option settings for sector %d", i);
    menu-> AddButton (text, command, comment);
  }

  sprintf(command, "%s0x%x%s", "((JMainMenu*)(", (unsigned long) this, "))->ChannelMenu()" );
  menu-> AddButton ("Single channel", command, "Enter submenu to display individual channels");

  sprintf(command, "%s0x%x%s", "((JMainMenu*)(", (unsigned long) this, "))->WriteResult()" );
  menu-> AddButton ("Write file", command, "Write results (T0..T2, left/right, values/errors) to files");

  sprintf(command, "%s0x%x%s", "((JMainMenu*)(", (unsigned long) this, "))->WriteDatabase()" );
  menu-> AddButton ("Write caldb", command, "As above, do aditionally caldb check-in using external tcl and perl script");

  menu-> AddButton ("exit Root", ".q", "exit Root");
  menu-> Show();
}
