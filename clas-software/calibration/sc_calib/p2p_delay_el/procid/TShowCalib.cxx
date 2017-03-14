#include <iostream>
#include "TShowCalib.h"

using namespace std;

/*
  TShowCalib::TShowCalib( const char* RootFileName )
  {
  gStyle->SetPalette( 1 ) ;
  gStyle->SetOptStat ( 0 );
  gStyle->SetOptFile ( 1 ) ;
  
  strcpy( fRootFileName, RootFileName ) ;
  
  cout << "Opening File " << fRootFileName << " now" << endl ;
  fRootFile = (TFile*)gROOT->GetFile( fRootFileName ) ;
  if ( fRootFile == 0 )
  fRootFile = new TFile( fRootFileName ) ;
  if ( fRootFile == 0 ) 
    {
       printf("Couldnt open the File %s\n", fRootFileName );
       exit(2);  
     }
  else
     {
       printf("File %s opened \n", fRootFileName );
     }
   TShowCalib( (TFile*) fRootFile );
   }
*/

TShowCalib::TShowCalib( const char* RootFileName )
{
  gStyle->SetPalette( 1 ) ;
  gStyle->SetOptStat ( 0 );
  gStyle->SetOptFile ( 1 ) ;

  strcpy( fRootFileName, RootFileName ) ;
  
  cout << "Opening File <" << fRootFileName << "> now" << endl ;
  fRootFile = (TFile*)gROOT->GetFile( fRootFileName ) ;
  if ( fRootFile == 0 )
    fRootFile = new TFile( fRootFileName ) ;
  if ( fRootFile == 0 ) 
    {
      printf("Couldnt open the File %s\n", fRootFileName );
      exit(2); 
    }
  else
    {
      printf("File %s opened \n", fRootFileName );
    }

  for ( int iC = 0; iC<=1 ; iC++ ) 
    {
      fCanvM[iC] = 0  ;
    }
  fMainCanvas = 0;

  fTOF_Bar = new TControlBar("vertical","TOF", 10, 480 );
  
  //  void (TShowCalib::*SMA)( int ) =    (TShowCalib::ShowMassAlignment) ;
 
  //  printf("%x\n", SMA);
  
  // char AddressString[20] ;
  //sprintf( AddressString, "0x%x", this );
  //printf("%s\n", AddressString );

  char Command[50] ;

  sprintf( Command, "%s0x%x%s", "((TShowCalib*)(", this, 
	   "))->ShowMassAlignment(0)" );

  //  printf("%s\n", Command);
  fTOF_Bar->AddButton("Alignment, before", (Text_t*)Command,
		      "Show non-calibrated mass allignment");

  sprintf( Command, "%s0x%x%s", "((TShowCalib*)(", this, 
	   "))->ShowMassAlignment(1)" );
  fTOF_Bar->AddButton("Alignment, after", (Text_t*)Command,
		      "Show calibrated mass allignment");



  sprintf( Command, "%s0x%x%s", "((TShowCalib*)(", this, 
	   "))->ShowMassSpec()" );
  fTOF_Bar->AddButton("M Specs", (Text_t*)Command,
		      "Show Mass Spectra");

  sprintf( Command, "%s0x%x%s", "((TShowCalib*)(", this, 
	   "))->ShowBvsP()" );
  fTOF_Bar->AddButton("B vs P, All", (Text_t*)Command,
		      "Show Beta versus P histograms, All");



  sprintf( Command, "%s0x%x%s", "((TShowCalib*)(", this, 
	   "))->ShowBvsP( 1 )" ); 
  fTOF_Bar->AddButton("B vs P, 1", (Text_t*)Command,
		      "Show Beta versus P, Sect 1");

  sprintf( Command, "%s0x%x%s", "((TShowCalib*)(", this, 
	   "))->ShowBvsP( 2 )" ); 
  fTOF_Bar->AddButton("B vs P, 2", (Text_t*)Command,
		      "Show Beta versus P, Sect 2");

  sprintf( Command, "%s0x%x%s", "((TShowCalib*)(", this, 
	   "))->ShowBvsP( 3 )" ); 
  fTOF_Bar->AddButton("B vs P, 3", (Text_t*)Command,
		      "Show Beta versus P, Sect 3");

  sprintf( Command, "%s0x%x%s", "((TShowCalib*)(", this, 
	   "))->ShowBvsP( 4 )" ); 
  fTOF_Bar->AddButton("B vs P, 4", (Text_t*)Command,
		      "Show Beta versus P, Sect 4");

  sprintf( Command, "%s0x%x%s", "((TShowCalib*)(", this, 
	   "))->ShowBvsP( 5 )" ); 
  fTOF_Bar->AddButton("B vs P, 5", (Text_t*)Command,
		      "Show Beta versus P, Sect 5");

  sprintf( Command, "%s0x%x%s", "((TShowCalib*)(", this, 
	   "))->ShowBvsP( 6 )" ); 
  fTOF_Bar->AddButton("B vs P, 6", (Text_t*)Command,
		      "Show Beta versus P, Sect 6");



  sprintf( Command, "%s0x%x%s", "((TShowCalib*)(", this, 
	   "))->ShowGoodRF()" );
  fTOF_Bar->AddButton("RF signal", (Text_t*)Command,
		      "Show RF histigrams before calibration");


  sprintf( Command, "%s0x%x%s", "((TShowCalib*)(", this, 
	   "))->ShowFine(1)" );
  fTOF_Bar->AddButton("Fine 1", (Text_t*)Command,
		      "Show Fine Tune, Sect 1");

  sprintf( Command, "%s0x%x%s", "((TShowCalib*)(", this, 
	   "))->ShowFine(2)" );
  fTOF_Bar->AddButton("Fine 2", (Text_t*)Command,
		      "Show Fine Tune, Sect 2");

  sprintf( Command, "%s0x%x%s", "((TShowCalib*)(", this, 
	   "))->ShowFine(3)" );
  fTOF_Bar->AddButton("Fine 3", (Text_t*)Command,
		      "Show Fine Tune, Sect 3");

  sprintf( Command, "%s0x%x%s", "((TShowCalib*)(", this, 
	   "))->ShowFine(4)" );
  fTOF_Bar->AddButton("Fine 4", (Text_t*)Command,
		      "Show Fine Tune, Sect 4");

  sprintf( Command, "%s0x%x%s", "((TShowCalib*)(", this, 
	   "))->ShowFine(5)" );
  fTOF_Bar->AddButton("Fine 5", (Text_t*)Command,
		      "Show Fine Tune, Sect 5");

  sprintf( Command, "%s0x%x%s", "((TShowCalib*)(", this, 
	   "))->ShowFine(6)" );
  fTOF_Bar->AddButton("Fine 6", (Text_t*)Command,
		      "Show Fine Tune, Sect 6");

  sprintf( Command, "%s0x%x%s", "((TShowCalib*)(", this, 
	   "))->ShowConstants()" );
  fTOF_Bar->AddButton("Constants", (Text_t*)Command,
		      "Show the Relative Delay Constants");

  sprintf( Command, "%s0x%x%s", "((TShowCalib*)(", this, 
	   "))->ShowResolutions()" );
  fTOF_Bar->AddButton("Resolutions", (Text_t*)Command,
		      "Show the Resolutions");

  //  sprintf( Command, "%s0x%x%s", "delete ((TShowCalib*)(", this,"))" );
  //  fTOF_Bar->AddButton("Close", (Text_t*)Command,
  //		      "End viewing calibrations");


  fTOF_Bar->AddButton("Exit ROOT", ".q",
		      "Exit the ROOT Session");
    
  fTOF_Bar->Show();  
  
  ShowMassSpec( ) ;
}

TShowCalib::~TShowCalib()
{
  //  if ( fTOF_Bar != 0 )  delete fTOF_Bar;
  if ( fMainCanvas!= 0 )  
    fMainCanvas->Close();
  if ( fMainCanvas!= 0 )  
    fMainCanvas->Close();
  for ( int iC = 0; iC<=1 ; iC++ ) 
    {
      if ( fCanvM[iC] != 0 )  
	fCanvM[iC]->Close() ;
    }
  //    if ( fCanvM[iC] != 0 )  delete fCanvM[iC] ;
}


void TShowCalib::ShowMassAlignment( int level )
{

  char CanName[20] ;
  sprintf( CanName, "Calibration_pass_%d", level );
  
  TCanvas* TempCanv =  new TCanvas( (const Text_t*)CanName,(const Text_t*)CanName  ,
			       50+level*700 , 10, 400, 500 );
  fCanvM[level] = TempCanv ;

  gStyle->SetOptStat(0);
  gStyle->SetPalette(1);
  fCanvM[level]->SetHighLightColor(2);
  fCanvM[level]->Range(0,0,1,1);
  fCanvM[level]->SetFillColor(10);
  fCanvM[level]->SetBorderSize(2);  
  
//  fCanvM[level]->Divide( 1,1 );
//  fCanvM[level]->cd(1);
  fCanvM[level]->cd();
  
  TPad* MainPad = (TPad*)gPad ;
  MainPad->Draw();
  MainPad->cd();
  MainPad->Range(0,0,1,1);
  MainPad->SetFillColor(10);
  MainPad->SetBorderSize(2);
  
  MainPad->Divide(2, 3);



  TDirectory* TestDir = (TDirectory*)fRootFile->Get("CalibTest") ;
  if ( TestDir == 0 ) 
    {
      printf("Couldnt open the root directory %s\n", "CalibTest" );
      return; 
    }
  TestDir->cd() ;

  char HisName[20] ;
  for ( int iSect = 1; iSect <= 6; iSect++ )
    {
      MainPad->cd( iSect ) ;
      TPad* CurrentPad = (TPad*)gPad ;
      CurrentPad->cd();
      CurrentPad->SetFillColor(29);
      CurrentPad->SetLogz();
  
      sprintf( HisName, "MvsS_%1d_%1d", level, iSect ) ;
      TH2F* His2Draw = (TH2F*)TestDir->Get( HisName ) ;
      His2Draw->DrawCopy( "colz" ) ;
      CurrentPad->GetFrame()->SetFillColor(10);
      CurrentPad->Modified() ;
    }
  
  MainPad->Modified() ;
  MainPad->cd() ;
  return;
  
}



void TShowCalib::ShowMassSpec()
{

  char CanvName[50];
  sprintf( CanvName, "Mass Spectra, Comparison"  ) ;
  fMainCanvas = new TCanvas("MainCanvas", (Text_t*)CanvName , 200, 350, 500, 600 ) ;


  fMainCanvas->cd();
//  fMainCanvas->Divide( 1, 1 ) ;
//  fMainCanvas->cd(1) ;

  TPad* MainPad = (TPad*) gPad ;
  
  MainPad->Draw();
  MainPad->cd();
  MainPad->Range( 0, 0, 1, 1 );
  MainPad->SetFillColor( 10 );
  MainPad->SetBorderSize( 2 );
   
  MainPad->Divide( 2, 2 );
  
  TDirectory* TestDir = (TDirectory*)fRootFile->Get("CalibTest") ;
  if ( TestDir == 0 ) 
    {
      printf("Couldnt open the root directory %s\n", "CalibTest" );
      return; 
    }
  TestDir->cd() ;
  char HisName[20] ;
  
  for ( int iX = 1; iX <= 2 ; iX++ )
    {
      for ( int iY = 1; iY <= 2 ; iY++ )
	{
	  int iPad = ( iY - 1 ) * 2  + iX ;         // Go Clockwise 
	  MainPad->cd( iPad ) ;
	  TPad* CurrentPad = (TPad*) gPad ;
	  CurrentPad->SetFillColor( 29 );
	  CurrentPad->SetLogy( iX - 1 );       // Set Log Scale for Y coordinate for the
	                                       // second column  
	  sprintf( HisName, "MSpec_%1d", iY-1 ) ;  // Lines are different levels of calibrations
	  TH1F* His2Draw = (TH1F*)TestDir->Get( HisName ) ;
	  His2Draw->SetLineWidth(4);
	  His2Draw->DrawCopy( ) ;
	  CurrentPad->GetFrame()->SetFillColor(10);
	  CurrentPad->Modified() ;
	  
	}
    }
  MainPad->Modified();
  MainPad->cd() ;
  MainPad->cd() ;
  
  return ;
}



void TShowCalib::ShowBvsP()
{
  char CanvName[50];
  sprintf( CanvName, "Beta vs P" ) ;
  fMainCanvas = new TCanvas("MainCanvas", (Text_t*)CanvName , 200, 350, 500, 600 ) ;

  fMainCanvas->cd();
  fMainCanvas->Divide( 1, 1 ) ;
  fMainCanvas->cd(1) ;

  TPad* MainPad = (TPad*) gPad ;
  
  MainPad->Draw();
  MainPad->cd();
  MainPad->Range( 0, 0, 1, 1 );
  MainPad->SetFillColor( 10 );
  MainPad->SetBorderSize( 2 );
   
  MainPad->Divide( 2, 2 );
  
  TDirectory* TestDir = (TDirectory*)fRootFile->Get("CalibTest") ;
  if ( TestDir == 0 ) 
    {
      printf("Couldnt open the root directory %s\n", "CalibTest" );
      return; 
    }
  TestDir->cd() ;
  char HisName[20] ;
  
  for ( int iX = 1; iX <= 2 ; iX++ )
    {
      for ( int iY = 1; iY <= 2 ; iY++ )
	{
	  int iPad = ( iY - 1 ) * 2  + iX ;         // Go Clockwise 
	  MainPad->cd( iPad ) ;
	  TPad* CurrentPad = (TPad*) gPad ;
	  CurrentPad->SetFillColor( 29 );
	  CurrentPad->SetLogz( iX - 1 );       // Set Log Scale for Y coordinate for the
	                                       // second column  
	  sprintf( HisName, "BvsP_%1d", iY-1 ) ;  // Lines are different levels of calibrations
	  TH2F* His2Draw = (TH2F*)TestDir->Get( HisName ) ;
	  His2Draw->DrawCopy( "colz" ) ;
	  CurrentPad->GetFrame()->SetFillColor(10);
	  CurrentPad->Modified() ;
	  
	}
    }
  MainPad->Modified();
  MainPad->cd() ;
  MainPad->cd() ; 
  
  return ;
}


void TShowCalib::ShowBvsP( int iSect )
{
  gStyle->SetPalette(1);
  gStyle->SetOptStat(0);
  gStyle->SetTitleH( 0.1 ) ;      
  gStyle->SetTitleW( 0.7 ) ;
   
  char CanvName[50];
  sprintf( CanvName, "Beta vs P, %d", iSect ) ;
  fMainCanvas = new TCanvas("MainCanvas", (Text_t*)CanvName , 200, 350, 500, 600 ) ;

  TF1* pi_plus_line = new TF1( "PiLine", "1./sqrt( 1. + pow(0.13957/x, 2) )", 0., 1.6 );
  pi_plus_line->SetLineWidth( 2 );
  pi_plus_line->SetLineColor( 2 ) ;

  TF1* positron_line = new TF1( "PosLine", "1.", 0., 1.6 );
  positron_line->SetLineWidth( 2 );
  positron_line->SetLineColor( 1 ) ; 
 
  TF1* proton_line = new TF1( "ProtLine", "1./sqrt( 1. + pow(0.938/x, 2) )", 0., 1.6 );
  proton_line->SetLineWidth( 2 );
  proton_line->SetLineColor( 50 ) ;

  TF1* proton_kaon_line = new TF1( "ProtKaonLine", 
				   "0.5/sqrt( 1. + pow(0.938/x, 2) ) + 0.5/sqrt( 1. + pow(0.494/x, 2) ) ",
				   0., 1.6 );
  proton_kaon_line->SetLineWidth( 2 );
  proton_kaon_line->SetLineColor( 6 ) ;
  proton_kaon_line->SetLineStyle( 4 ) ;



  fMainCanvas->cd(); 
  fMainCanvas->Divide( 1, 1 ) ;
  fMainCanvas->cd(1) ;
  fMainCanvas->SetFillColor( 10 ) ;

  TPad* MainPad = (TPad*) gPad ;
  
  MainPad->Draw();
  MainPad->cd();
  MainPad->Range( 0, 0, 1, 1 );
  MainPad->SetFillColor( 19 );
  MainPad->SetBorderSize( 2 );
   
  MainPad->Divide( 6, 10, 0.002, 0.002 );
  MainPad->Modified() ;
  MainPad->Update();
  
  TDirectory* TestDir = (TDirectory*)fRootFile->Get("CalibTest") ;
  if ( TestDir == 0 ) 
    {
      printf("Couldnt open the root directory %s\n", "CalibTest" );
      return; 
    }
  TestDir->cd() ;
  char HisName[30] ;
  for ( int iStrip = 1; iStrip <= NumStrips::Instance().Get_NumStrips(); iStrip++ )
    {
      MainPad->cd( iStrip );
      gPad->SetFillColor( 29 );
      gPad->SetLogz( 1 ) ;
      gPad->GetFrame()->SetFillColor(9);
      gPad->Update() ;     
      //      gPad->Modified() ;
      sprintf( HisName, "_%d_%d", iSect, iStrip );
      TH2F* temp = (TH2F*) TestDir->Get( HisName ) ;
      temp->DrawCopy( "colz" ) ;
      pi_plus_line->DrawCopy("same") ;
      proton_line->DrawCopy("same") ;
      proton_kaon_line->DrawCopy("same") ;
      positron_line->DrawCopy("same") ;
      gPad->Update();
      gPad->GetFrame()->SetFillColor(9);
      //     gPad->Modified() ;   
      gPad->Update() ;
      delete temp ;
      gPad->Update();
    }
  MainPad->Modified();
  MainPad->Update() ;
  MainPad->cd() ;
  MainPad->cd() ;
  delete pi_plus_line;
}



void TShowCalib::ShowFine ( int iSect )
{
  char CanvName[50];
  sprintf( CanvName, "Fine Tune, Sec %1d", iSect ) ;
  fMainCanvas = new TCanvas("MainCanvas", (Text_t*)CanvName , 200, 200, 700, 800 ) ;

  fMainCanvas->cd();
  fMainCanvas->Divide( 1, 1 ) ;
  fMainCanvas->cd(1) ;

  TPad* MainPad = (TPad*) gPad ;
  
  MainPad->Draw();
  MainPad->cd();
  MainPad->Range( 0, 0, 1, 1 );
  MainPad->SetFillColor( 10 );
  MainPad->SetBorderSize( 2 );
   
  MainPad->Divide( 6, 10 );
  
  TDirectory* TestDir = (TDirectory*)fRootFile->Get("FineTune") ;
  if ( TestDir == 0 ) 
    {
      printf("Couldnt open the root directory %s\n", "FineTune" );
      return; 
    }
  TestDir->cd() ;
  char HisName[20] ;
  
  for ( int iPad = 1; iPad <= NumStrips::Instance().Get_NumStrips() ; iPad++ )
    {
      MainPad->cd( iPad ) ;
      TPad* CurrentPad = (TPad*) gPad ;
      CurrentPad->SetFillColor( 29 );
      sprintf( HisName, "%1d_%d_fine", iSect, iPad ) ;  
      TH1F* His2Draw = (TH1F*)TestDir->Get( HisName ) ;
      His2Draw->SetLineWidth(2);
      His2Draw->DrawCopy( ) ;
      CurrentPad->GetFrame()->SetFillColor(10);
      CurrentPad->Modified() ;
    }
  MainPad->Modified();
  MainPad->cd() ;
  MainPad->cd() ;
  
  return ;
}


void TShowCalib::ShowGoodRF()
{

  char CanvName[50];
  sprintf( CanvName, "RF Offset"  ) ;

  gStyle->SetOptFit(1011) ;
  fMainCanvas = new TCanvas("MainCanvas", (Text_t*)CanvName , 200, 350, 500, 600 ) ;


  fMainCanvas->cd();
  fMainCanvas->Divide( 1, 1 ) ;
  fMainCanvas->cd(1) ;

  TPad* MainPad = (TPad*) gPad ;
  
  MainPad->Draw();
  MainPad->cd();
  MainPad->Range( 0, 0, 1, 1 );
  MainPad->SetFillColor( 10 );
  MainPad->SetBorderSize( 2 );
   
  MainPad->Divide( 1, 2 );
  
  TDirectory* TestDir = (TDirectory*)fRootFile->Get("CalibTest") ;
  if ( TestDir == 0 ) 
    {
      printf("Couldnt open the root directory %s\n", "CalibTest" );
      return; 
    }
  TestDir->cd() ;
  char HisName[20] ;
  sprintf( HisName, "GoodRF"  ) ; 
  TH2F* His2Draw = (TH2F*)TestDir->Get( HisName ) ;
  
  MainPad->cd(1);
  TPad* CurrentPad = (TPad*) gPad ;
  CurrentPad->SetFillColor( 29 );
  His2Draw->DrawCopy( "colz" ) ;
  CurrentPad->GetFrame()->SetFillColor(10);
  CurrentPad->Modified() ;

  
  MainPad->cd(2);
  CurrentPad = (TPad*) gPad ;
  CurrentPad->SetFillColor( 29 );
  TH1F* H1D = (TH1F*) His2Draw->ProjectionY("RF Projection", 0, 9999, "E" ) ;
 
  gStyle->SetOptFit(1011) ;
  H1D->Fit("gaus", "Q", "", -0.5, +0.5 );
  H1D->SetLineWidth(4);
  H1D->SetXTitle("RF offset, ns" );
  H1D->DrawCopy() ;
  ((TF1*) gROOT->FindObject("gaus"))->SetLineWidth(5);
  gROOT->FindObject("gaus")->Draw("same") ;
  CurrentPad->GetFrame()->SetFillColor(10);
  CurrentPad->Modified() ;
  delete  H1D ;
  //  gStyle->SetOptFit(0) ;
  
  MainPad->Modified();
  MainPad->cd() ;
  
  return ;
}



void TShowCalib::ShowConstants()
{
  char CanvName[20] ;
  sprintf( CanvName, "Fit Results" );
  
  TCanvas* fMainCanvas =  new TCanvas( "MainCanvas",
    (const Text_t*) CanvName , 200 , 350, 500, 600 );
  gStyle->SetOptStat(0);
  fMainCanvas->cd();
  fMainCanvas->Divide( 1, 1 ) ;
  fMainCanvas->cd(1) ;

  TPad* MainPad = (TPad*) gPad ;
  
  MainPad->Range( 0, 0, 1, 1 );
  MainPad->SetFillColor( 10 );
  MainPad->SetBorderSize( 2 );
  MainPad->Draw();
  MainPad->cd();
  MainPad->SetFillColor(10);
  MainPad->SetBorderSize(2);
  MainPad->Divide(2, 3);
  MainPad->Draw();

  fRootFile->cd();

  TDirectory* TestDir = (TDirectory*) gFile ;
  if ( TestDir == 0 ) 
    {
      printf("Couldn't open the ROOT top directory \n" );
      return; 
    }
  TestDir->cd() ;

  char HisName[20] ;
  for ( int iSect = 1; iSect <= 6; iSect++ )
    {
      MainPad->cd( iSect ) ;
      TPad* CurrentPad = (TPad*)gPad ;
      CurrentPad->cd();
      CurrentPad->SetFillColor(29);
  
      sprintf( HisName, "DLY_S%1d", iSect ) ;
      TH1F* His2Draw = (TH1F*)TestDir->Get( HisName ) ;
      His2Draw->DrawCopy( "e1" ) ;
      CurrentPad->Update();
      CurrentPad->GetFrame()->SetFillColor(10);
      CurrentPad->Modified() ;
      CurrentPad->Update();
    }  
  MainPad->Modified() ;
  MainPad->Update();
  MainPad->cd() ;
  return;
}


void TShowCalib::ShowResolutions()
{
  char CanvName[20] ;
  sprintf( CanvName, "Resolutions" );
  
  TCanvas* fMainCanvas =  new TCanvas( "MainCanvas",
    (const Text_t*) CanvName , 200 , 350, 500, 600 );
  gStyle->SetOptStat(0);
  fMainCanvas->cd();
  fMainCanvas->Divide( 1, 1 ) ;
  fMainCanvas->cd(1) ;

  TPad* MainPad = (TPad*) gPad ;
  
  MainPad->Range( 0, 0, 1, 1 );
  MainPad->SetFillColor( 10 );
  MainPad->SetBorderSize( 2 );
  MainPad->Draw();
  MainPad->cd();
  MainPad->SetFillColor(10);
  MainPad->SetBorderSize(2);
  MainPad->Divide(2, 3);
  MainPad->Draw();

  fRootFile->cd();

  TDirectory* TestDir = (TDirectory*) gFile ;
  if ( TestDir == 0 ) 
    {
      printf("Couldn't open the ROOT top directory \n" );
      return; 
    }
  TestDir->cd() ;

  char HisName[20] ; 
  for ( int iSect = 1; iSect <= 6; iSect++ )
    {
      MainPad->cd( iSect ) ;
      TPad* CurrentPad = (TPad*)gPad ;
      CurrentPad->cd();
      CurrentPad->SetFillColor(29);
  
      sprintf( HisName, "SGM_S%1d", iSect ) ;
      TH1F* His2Draw = (TH1F*)TestDir->Get( HisName ) ;
      His2Draw->DrawCopy( "e1" ) ;
      CurrentPad->Update();
      CurrentPad->GetFrame()->SetFillColor(10);
      CurrentPad->Modified() ;
      CurrentPad->Update();
    }  
  MainPad->Modified() ;
  MainPad->Update();
  MainPad->cd() ;
  return;
}
