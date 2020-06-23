//$Id: ProjectionFit.C,v 1.2 2008/03/27 14:22:50 pasyuk Exp $

////////////////////////// ProjectionFit.C ////////////////////////////////////////////////
//                                                                                       //
//      Part of the Start Counter calibration process. This macro calculates the         //
//   timing offset corrections between the 24 paddles. It is designed to be executed     //
//   on the root histogram file created by stn_calib (or the amalgamated file            //
//   created by concatenating several root files). It projects the pion sttag            //
//   histogram (start counter timing - tagger timing) along each paddle, thus creating   //
//   24 projections. It fits them with a Gaussian function, plots them on 6 canvases,    //
//   each containing the fits for one sector, and obtains fit parameters.                //
//   It then extracts the "old" pd2pd values from the STN_CALIB_delta_T_pd2pd_old.dat    //
//   file created by new_veff_fit.C, corrects those constants by the offset and          //
//   writes the "new" values into the file STN_CALIB_delta_T_pd2pd.dat. Run by typing    //
//   (in Root, after opening desired file):                                              //
//                                           [root] .x ProjectionFit.C                   //
//                                                                                       //
//   Then examine the fits -- if you're happy with all, press the pink "finish" button   //
//   underneath the last histogram, which will create the file with new constants.       //
//   If you're not happy with the fits anywhere, use the helpfully coloured buttons      //
//   to move the fitting range. You want to fit to the peak position but the data        //
//   isn't quite Gaussian, so it's fine if you're not fitting across the whole range.    //
//                                                                                       //
//   If the histogram is off-scale, adjust plotting limits in the code.                  //
//                                                                                       //
//   Enjoy!                                                                              //
//                                                                                       //
//     Daria Sokhan, University of Edinburgh, May 2007, February 2008.                   //
//                                                                                       //
///////////////////////////////////////////////////////////////////////////////////////////



#include "Riostream.h"

TH1D *sttag_pion_[24];

TF1 *fit[24];

ifstream old_file;
ofstream new_file;

double p[24];
double e[24];
double chi[24];

double old_pd2pd[24];
double new_pd2pd[24];

TCanvas *c[6];
TPad *pad[24];

int canvas = 99;
int plot = 99;
int current = 99;

int lclick[24] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
int rclick[24] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

char* action[] = {"min <-", "min ->", "max <-", "max ->", "finish", NULL };
enum action_t {min_down, min_up, max_down, max_up, finish};

TButton* gBut[5];


// A fix for old versions of root, which don't have the colour wheel:

TColor *yellow = new TColor(1050,1.,1.,.8,"yellow"); // kYellow-10, 390
TColor *green = new TColor(1051,.0,.4,.0,"green"); // kGreen+3, 419
TColor *pale_blue = new TColor(1052,.8,1.,1.,"pale_blue"); // kCyan-10, 422
TColor *magenta = new TColor(1053,.4,.0,.4,"magenta"); // kMagenta+3, 619
TColor *pink = new TColor(1054,1.,.4,.8,"pink"); // kPink+6, 906




/****************** Functions **********************/


// Fitting each projection with a Gaussian: 

void Fit(int paddle = 99){     
                       
  sttag_pion_[paddle]->Fit("gaus","Q","",-0.5.+(lclick[paddle]*0.05),0.5.+(rclick[paddle]*0.05));
  fit[paddle] = sttag_pion_[paddle]->GetFunction("gaus");       
  fit[paddle]->SetLineColor(2);
}


// Extracting the peak position fit parameter (p), its error (e) and the chi squared of fit for pions:

void Fit_Parameter(int paddle = 99){

  p[paddle] = fit[paddle]->GetParameter(1);
  e[paddle] = fit[paddle]->GetParError(1);
  chi[paddle] = fit[paddle]->GetChisquare();

}


// Handling the button-clicks:

void act(int choice = 9){
  switch (choice) {
    
  case min_down:
    lclick[current]=lclick[current]-1;
    Refit(current);
    break;
    
  case min_up:
    lclick[current]=lclick[current]+1;
    Refit(current);
    break;
    
  case max_down:
    rclick[current]=rclick[current]-1;
    Refit(current);
    break;
    
  case max_up:
    rclick[current]=rclick[current]+1;
    Refit(current);
    break;
    
  case finish:
    Finish();
    cout << "good bye!" << endl;
    exit(0);
  }
}



// For the re-fitting option:

void Refit(int histogram = 99){

  pad[histogram]->cd();
  Fit(histogram);
  Fit_Parameter(histogram);
  pad[histogram]->Update();

}



// When all fits have been approved:

void Finish(){
  
  Calculate_Constants();
  
  Write_New_Constants();

  for (i = 0; i < 6; i++){
    char name1[30];
    sprintf(name1,"gif/sector%d.gif",i+1);
    c[i]->cd();
    c[i]->Print(name1);
    char name2[30];
    sprintf(name2,"ps/sector%d.ps",i+1);
    c[i]->cd();
    c[i]->Print(name2);

  }
  
}


// Extract old constants, file needs to be in same directory:

void Get_Old_Constants(){
  
  old_file.open("STN_CALIB_delta_T_pd2pd_old.dat"); 
  
  int nlines = 0;
  
  if (old_file.is_open()){
    cout << "\n file open!" << endl;
    while ((!old_file.eof()) && (nlines < 24)){
      old_file >> old_pd2pd[nlines];
      cout << "line " << nlines << ",  old pd2pd is " << old_pd2pd[nlines] <<endl;
      if (!old_file.good()) break;
      nlines++;
    }
    old_file.close(); 
  }
  
  else {
    cout << "file cannot be opened, sorry for the headache :(" << endl;
  }

}



// Do the subtraction to get new constant:

void Calculate_Constants(){
  
  for (int i=0; i<24; i++) cout << "offset value: " << p[i] << endl;
  
  for (int i=0; i<24; i++){     
    new_pd2pd[i] = old_pd2pd[i] - p[i];
    cout << "new " << new_pd2pd[i] << endl;
  }
}


// Write the new constants to file:

void Write_New_Constants(){
  
  new_file.open("STN_CALIB_delta_T_pd2pd.dat");
  
  for (int i=0; i<24; i++){
    new_file << new_pd2pd[i] << endl;
  }
  
  new_file.close();
  
}



/************************************  The main function: **************************************/


void ProjectionFit(){
 
  gStyle->SetOptFit(111);
  gStyle->SetOptStat(11);
  gStyle->SetStatW(0.14);
  gStyle->SetStatH(0.14);
  gStyle->SetStatColor(kWhite);
  gStyle->SetOptTitle(0);
  gROOT->UseCurrentStyle();

  
// Projecting pion histograms along Y, per paddle: 
  
  for (int i=1; i<25; i++){
    char hstname[80];
    sprintf (hstname, "sttag_pion_%d", i);
    sttag_pion_[i-1] = sttag_pion->ProjectionY(hstname,i,i);
  }
  
  
// Fit with a Gaussian and extract fit parameters:
  
  for (int i=0; i<24; i++){
    Fit(i); 
    Fit_Parameter(i);   
  }


// Set up 6 canvases, draw buttons, create 24 histogram pads and draw the histograms for inspection:

  int n = 0;                  // histogram number
  int m = 9;                  // for button positioning on the last pad   

  for (int i=0; i<6; i++){
    
    char ctitle[80];
    char cname[80];
    sprintf (ctitle, "Projection Fits - Sector %d", i+1);
    sprintf (cname, "c%d", i+1);
    
    c[i] = new TCanvas(cname,ctitle,1350,950); // Fits snugly on my laptop screen :-)
    c[i]->SetFillColor(1051);                   // (kGreen+3)
    c[i]->GetFrame()->SetFillColor(1053);       // (kMagenta+3)
    c[i]->GetFrame()->SetBorderSize(6);
    c[i]->GetFrame()->SetBorderMode(-1);
    c[i]->Divide(2,2);
    
    for (int j=0; j<4; j++){
      
      c[i]->cd(j+1);
      
      n=(4*i)+j;
           
// Draw buttons:
      
      for (int k=0; action[k] != NULL; k++) {          
	
	char request[80];
	sprintf (request, "current = %d, act(%d)", n, k);
	
	if ((i == 5) && (j == 3)){           // The very last pad has the "finish" button
	  
	  if (k < 2) m = k;  
	  else if ((k == 2) || (k == 3)) m = k+1;
	  else m = 2;
	  
	  gBut[k] = new TButton(action[k], request, (0.2*m)+0.025, 0.01, (0.2*m)+0.15, 0.07);
	  if (k < 2) gBut[k]->SetFillColor(1050);                                               // (kYellow-10)
	  if ((k == 2) || (k == 3)) gBut[k]->SetFillColor(1052);                                // (kCyan-10)
	  if (k == 4) gBut[k]->SetFillColor(1054);                                              // (kPink+6)
	  gBut[k]->Draw();
	}
	
	else{
	  
	  if (k < 2){
	    gBut[k] = new TButton(action[k], request, (0.2*k)+0.075, 0.01, (0.2*k)+0.225, 0.07);
	    gBut[k]->SetFillColor(1050);                                                        // (kYellow-10)
	    gBut[k]->Draw();
	  }
	  
	  else if (k == 4) continue;
	  else{
	    gBut[k] = new TButton(action[k], request, (0.2*k)+0.175, 0.01, (0.2*k)+0.325, 0.07);
	    gBut[k]->SetFillColor(1052);                                                        // (kCyan-10)
	    gBut[k]->Draw();
	  }
	}
      }
      

// Draw all histograms for inspection:

      char ptitle[80];
      char pname[80];
      sprintf (ptitle, "Paddle %d", n+1);
      sprintf (pname, "paddle%d", n+1);
      
      pad[n] = new TPad(pname, ptitle, 0., 0.08, 1., 1.);
      pad[n]->SetFillColor(0);  // (kWhite)
      pad[n]->Draw();  
      
      pad[n]->cd();
      if (n == 8) sttag_pion_[n]->SetAxisRange(-5.,5.,"X");
      else sttag_pion_[n]->SetAxisRange(-1.5,1.5,"X");
      sttag_pion_[n]->Draw(); 

      c[i]->Modified();
      c[i]->Update();   
    } 
  }

  
// Open old constants file and extract constants:
  
  Get_Old_Constants();
  
}
