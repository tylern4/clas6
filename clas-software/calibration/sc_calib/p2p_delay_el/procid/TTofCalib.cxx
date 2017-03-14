#include "TTofCalib.h"

using namespace std;

float n_strips_for_el  ;          // Number of strips starting from #1 to be
float BeamEnergy       ;

float Z_0 = 0.0 ;                 // Target position will be read from the Map later

extern RunControl* PhyAna ;

inline double fmod_d(double a, double b) { return fmod(a,b); }

TH1F* makeHistBankAvail(const char* process) {
  char hstname[255];
  sprintf (hstname, "bankAvalable_%s", process);
  TH1F* h = new TH1F(hstname, hstname, 10, 0.5, 10.5);
  h->GetXaxis()->SetBinLabel(1, "every");
  h->GetXaxis()->SetBinLabel(2, "Evnt");
  h->GetXaxis()->SetBinLabel(3, "Hevt");
  h->GetXaxis()->SetBinLabel(4, "SCPB");
  h->GetXaxis()->SetBinLabel(5, "CCPB");
  h->GetXaxis()->SetBinLabel(6, "ECPB");
  h->GetXaxis()->SetBinLabel(7, "DCPB");
  h->GetXaxis()->SetBinLabel(8, "CL01");
  h->SetFillColor(5);
  return h;
}

TTofCalib::TTofCalib()
{
  // Main constructor 
  
  RootFile = PhyAna->GetRootFile();
  RootFile->cd();
  
  FineDir = RootFile->mkdir("FineTune","Fine Tuning");
  RootFile->cd("FineTune");
  FineTune = new TFineTune; 
  
  RootFile->cd();
  CrudeDir = RootFile->mkdir("CrudeTune","Crude Tuning");
  RootFile->cd("CrudeTune");
  CrudeTune = new TCrudeTune;
  
  
  RootFile->cd();
  TestDir = RootFile->mkdir("CalibTest","Calibration Test");
  RootFile->cd("CalibTest");
  CalibTest = new TCalibTest;
  
  RootFile->cd();

  // Initilize the Histograms for the final results

  char HisName[20], HisTitle[60];
  for ( int iSect = 1; iSect <= N_SECTS; iSect++ )
    {
      sprintf( HisName, "DLY_S%1d", iSect );
      sprintf( HisTitle, "Delay, Sec %1d", iSect );      
      ConstHisto[iSect-1] = new TH1F( HisName, HisTitle, NumStrips::Instance().Get_NumStrips(), 0.5, NumStrips::Instance().Get_NumStrips()+0.5 ); 

      sprintf( HisName, "SGM_S%1d", iSect );
      sprintf( HisTitle, "Sigma, Sec %1d", iSect );      
      SigmaHisto[iSect-1] = new TH1F( HisName, HisTitle, NumStrips::Instance().Get_NumStrips(), 0.5, NumStrips::Instance().Get_NumStrips()+0.5 ); 
    }
  RootFile->cd();
}



TTofCalib::~TTofCalib()
  // Main Destructor 
  // This is where constants start being calculated
{
  float TuneConst[N_SECTS][N_MAX_STRIPS] ;
  int Status[N_SECTS][N_MAX_STRIPS];

  RootFile->cd(); 

  RootFile->cd("FineTune"); 
  FineTune->Do();	// Do Fine Tuning
  FineDir->Write();		
   

  // Get Fine Tune constants into an array and pass it to Crude Tune

  for ( int iSect = 1; iSect <= N_SECTS; iSect++ )
    for ( int iStrip = 1; iStrip <= NumStrips::Instance().Get_NumStrips(); iStrip++ )
      {
	TuneConst[iSect-1][iStrip-1] = FineTune->GetT0( iSect, iStrip );
	// printf("%f\n",TuneConst[iSect-1][iStrip-1]);
	Status[iSect-1][iStrip-1] = FineTune->GetStatus( iSect, iStrip );
      }

  RootFile->cd("CrudeTune");
  CrudeTune->Do( TuneConst );		// Do crude tune using Fine Tune constants
  CrudeDir->Write();

  RootFile->cd("CalibTest");
  CalibTest->Do( TuneConst, Status );	// Test Calibrations ( make histos for test )
  TestDir->Write();
 
  // Save the constants into a ASCII file
  SaveConstants( TuneConst, "constants.dat" );
 
  // Call the destructors
  delete FineTune;
  delete CrudeTune;
  delete CalibTest; 
 
  // Write  out the histograms with the final results

  RootFile->cd();
  for ( int iSect = 1; iSect <= N_SECTS; iSect++ )
    {
      ConstHisto[iSect-1]->Write();
      SigmaHisto[iSect-1]->Write();	
    }
  RootFile->cd();
}


void TTofCalib::SaveConstants( float Const[][N_MAX_STRIPS], const char* filename )
{

  // Function to save the final constants into a ASCII file

  std::string ModuleName ("TTofCalib::SaveConstants");
  std::ostringstream Message;
  
  char   *ParmsDir ;
  char MapName[128];
  
  int iFirst;
  float MapConst[N_MAX_TOTAL_STRIPS];

  
  //    Open the output file to write 

  FILE* OutFile = fopen( filename , "w" );
  string locString(filename);
  locString += ".trunc";
  FILE* TruncOutFile = fopen( locString.c_str() , "w" );
  if (( OutFile == NULL ) || (TruncOutFile == NULL))
    {
      printf( "Couldn't Open File for the TOF Constants \a \n" );
      return;
    } 
    
  //   Retrive the existing constants from the Map
  //    You have to be sure these are the constants used 
  //    in the cooking process

  ParmsDir = getenv( "CLAS_PARMS" );
  if(NumStrips::Instance().Get_NumStrips() == 48)
    sprintf( MapName, "%s/Maps/SC_CALIBRATIONS.map", ParmsDir );
  else{sprintf( MapName, "%s/Maps/SC_CALIBRATIONS_V2.map", ParmsDir );}
  
  Message << "Reading SC constatnts from " << MapName;
  PhyAna->PrintMessage (ModuleName, Message.str() );  

  int RunNumb = PhyAna->GetRunNumber();
  map_get_float( MapName, "delta_T","paddle2paddle", NumStrips::Instance().Get_NumTotalStrips(), (float*)MapConst, RunNumb, &iFirst);

  // Saving the final constants into histograms

  for ( int jSect = 1 ; jSect <= N_SECTS; jSect++ )   
    for ( int jStrip = 1 ; jStrip <= NumStrips::Instance().Get_NumStrips(); jStrip++ )   
      {
	ConstHisto[jSect-1]->SetBinContent( (Int_t) jStrip, 
					  (Stat_t) Const[jSect-1][jStrip-1] );
	ConstHisto[jSect-1]->SetBinError( (Int_t) jStrip, 
					  (Stat_t) FineTune->GetErrT0( jSect, jStrip ) );
	SigmaHisto[jSect-1]->SetBinContent( (Int_t) jStrip, 
					  (Stat_t) FineTune->GetSigma( jSect, jStrip ) );
	SigmaHisto[jSect-1]->SetBinError( (Int_t) jStrip, 
					  (Stat_t) FineTune->GetErrSigma( jSect, jStrip ) );
      }
  printf("Saving Constants in %s\n", filename );

  //  Write the constants
  
  fprintf( OutFile, "Sector  Strip   Status     Delay      Sigma      Delay4Map\n");
  fprintf( OutFile, "-----------------------------------------------------------------------\n");
 
  for ( int iSect = 1; iSect <= N_SECTS; iSect++ )
    for ( int iStrip = 1; iStrip <= NumStrips::Instance().Get_NumStrips(); iStrip++ ) 
      {
	int Status = FineTune->GetStatus( iSect, iStrip );
	float Delay = Const[iSect-1][iStrip-1];
	float Sigma = FineTune->GetSigma( iSect, iStrip );

     float Result = MapConst[(iSect-1)*(NumStrips::Instance().Get_NumStrips()) + iStrip-1] - Delay;
     fprintf(TruncOutFile, "%8g\n", Result);
	fprintf( OutFile, " %2d      %2d       %1d     %8g    %8g    %8g\n", iSect, iStrip, Status, 
		 Delay, Sigma, Result );
      }  
  
  fclose( OutFile );
  fclose( TruncOutFile );
}


 
void TTofCalib::Process( physEVNT_c* EVENT )
{
  static int iFirst = 1;

  if ( iFirst == 1 ) 
    {

      // Define the maximum strip number for fine
      // tuning with electrons 
      
      float BeamEnergy = PhyAna->GetBeamEnergy();
      if ( BeamEnergy < 3. ) 
	n_strips_for_el = 20 ;
      else
	n_strips_for_el = 15 ;
      //      printf("E_b = %f, n strips el = %f\n", BeamEnergy, n_strips_for_el );
      iFirst = 0;

      // Define the Target Z-position from the Map 

      std::string ModuleName ("TTofCalib::Process");
      std::ostringstream Message;
      
      char   *ParmsDir ;
      char MapName[128];
      
      int iFirst;
      float Target[3] ;
      
      ParmsDir = getenv( "CLAS_PARMS" ); 
      sprintf( MapName, "%s/Maps/GEOMETRY.map", ParmsDir );
      
      int RunNumb = PhyAna->GetRunNumber();
      Message << "Reading Target Position from " << MapName 
	      << " for run #" <<  RunNumb;
      PhyAna->PrintMessage (ModuleName, Message.str() );
      
      map_get_float( MapName, "target","position", 3, (float*)Target, RunNumb, &iFirst);
      
      Z_0 = Target[2];
      Message << ", Target Position is " << Z_0;
      PhyAna->PrintMessage (ModuleName, Message.str() );      
    }

  //  Call Process functions in sequence 

  RootFile->cd("FineTune");
  FineTune->Process();		// Fill Fine Tune Histos
 
  RootFile->cd("CrudeTune");
  CrudeTune->Process();		// Fill Crude Tune NTuple

  RootFile->cd("CalibTest");	// Fill Test Ntuple
  CalibTest->Process();
 
  RootFile->cd();
}




TTofCalib::TFineTune::TFineTune()
{
 
  // Initialize Strips
 
  int iSect, iStrip;
  for ( iSect = 1; iSect <= N_SECTS ; iSect++ )
    for ( iStrip = 1; iStrip <= NumStrips::Instance().Get_NumStrips(); iStrip++ )
      Strip[iSect-1][iStrip-1] = new Strip_t( iSect, iStrip );
  DoneFlag = 0;  
    
  histBankAvail = makeHistBankAvail("fine");

  //  printf("Fine Tune is Initialized\n");
 
}


TTofCalib::TFineTune::~TFineTune()
{

  // Destroy Strips

  for( int iSect = 1; iSect <= N_SECTS; iSect++ )
    for( int iStrip = 1; iStrip <= NumStrips::Instance().Get_NumStrips(); iStrip++ )
      delete Strip[iSect-1][iStrip-1] ;
}



void TTofCalib::TFineTune::Process()
{

  // Define BOS banks 
  
  clasEVNT_t* Evnt = NULL;
  Evnt = (clasEVNT_t *) getBank(&bcs_,"EVNT"); 
  clasHEVT_t* Hevt = NULL ;
  Hevt = (clasHEVT_t *) getBank(&bcs_,"HEVT");
  clasSCPB_t* SCPB = NULL;
  SCPB = (clasSCPB_t *) getBank(&bcs_,"SCPB");
  clasCCPB_t* CCPB = NULL;
  CCPB = (clasCCPB_t *) getBank(&bcs_,"CCPB");
  clasECPB_t* ECPB = NULL;
  ECPB = (clasECPB_t *) getBank(&bcs_,"ECPB");
  clasDCPB_t* DCPB = NULL;
  DCPB = (clasDCPB_t *) getBank(&bcs_,"DCPB");
  clasCL01_t* CL01 = NULL; 
  CL01 = (clasCL01_t *) getBank(&bcs_,"CL01");
 
  if (Evnt) histBankAvail->Fill(2.);
  if (Hevt) histBankAvail->Fill(3.);
  if (SCPB) histBankAvail->Fill(4.);
  if (CCPB) histBankAvail->Fill(5.);
  if (ECPB) histBankAvail->Fill(6.);
  if (DCPB) histBankAvail->Fill(7.);
  if (CL01) histBankAvail->Fill(8.);
 
  if ( Hevt == 0 || Evnt == 0 || SCPB == 0 || DCPB == 0 || 
       CCPB == 0 || ECPB == 0 || CL01 == 0 ) return;

  histBankAvail->Fill(1.);

  if ( ( Hevt->hevt[0].npgp % 100 ) > 0 )		// There are tracks
    {
      if (
      	   Evnt->evnt[0].ecstat > 0 &&			// first particle hit EC
      	   Evnt->evnt[0].dcstat > 0 && 			// first particle hit DC
	   Evnt->evnt[0].scstat > 0 &&			// first particle hit SC
	   Evnt->evnt[0].ccstat > 0 &&			// first particle hit CC
	   CCPB->ccpb[0].nphe   > 35 &&                 // More than 1 p-El in CC
	   DCPB->dcpb[Evnt->evnt[0].dcstat-1].status > 0 &&	// TBT check
	   Evnt->evnt[0].charge < 0 && 			// Negative
	   (
	    ( ECPB->ecpb[Evnt->evnt[0].ecstat-1].etot > 
	      ( -0.06 + 0.27 * Evnt->evnt[0].pmom ) ) &&
	    (  ECPB->ecpb[Evnt->evnt[0].ecstat-1].etot <        // Extra EC cuts to 
	      ( +0.08 + 0.36 * Evnt->evnt[0].pmom ) ) &&        // reduce the background
	    (  ECPB->ecpb[Evnt->evnt[0].ecstat-1].etot > 0.15 )
	    ) &&
	   Evnt->evnt[0].id == 11				// It s an electron
	   )	
	{

	  float rf_time1 = CL01->cl01[0].rf ;
      	  //float rf_time1 = Hevt->hevt[0].rf1 ;
	  
	  //	Electron Part of fine Tuning	//  

	  int scpb_code_el = SCPB->scpb[Evnt->evnt[0].scstat-1].scpdht ;
	  int Sect_e  = ( int )  scpb_code_el / 10000 ;
	  int Strip_e = ( int ) ( ( scpb_code_el % 10000 ) / 100 );  
	  float TDC_e = SCPB->scpb[Evnt->evnt[0].scstat-1].time * CalConst_TOF ;
	  float Path_e = SCPB->scpb[Evnt->evnt[0].scstat-1].path ; 
	  float Z_el = Evnt->evnt[0].vert.z ;
	  float TOF_e = Path_e / c_light ;
	  float T_vtx_el = ( Z_el - Z_0 ) / c_light ;
	  float D_T_e = ( TDC_e - TOF_e - T_vtx_el ) - rf_time1*CalConst_RF + 1000.*RF_Period ;
	  float CT_e = fmod_d( D_T_e , RF_Period )  - RF_Period/2 ; 
//cout << "dte, cte = " << D_T_e << ", " << CT_e << endl;
//cout << "tdc_e, tofe, tvtxe, rftime, rfperiod = " << TDC_e << ", " << TOF_e << ", " << T_vtx_el << ", " << rf_time1 << ", " << RF_Period << endl;

	  if ( Sect_e > N_SECTS || Strip_e > NumStrips::Instance().Get_NumStrips() ) 
	    {
	      printf("Strange SCPB code for electron %d\n",scpb_code_el);
	      return;
	    } 
 	  if ( Strip_e <= n_strips_for_el )    	  	  
	    Strip[Sect_e-1][Strip_e-1]->hist->Fill( CT_e );
   
	  //	Looping for Pions
   
	  for ( int ipart = 1; ipart < ( Hevt->hevt[0].npgp % 100 ); ipart++ )
	    if ( Evnt->evnt[ipart].dcstat > 0 && 			// particle hit DC
		 Evnt->evnt[ipart].scstat > 0 &&			// particle hit SC
		 DCPB->dcpb[Evnt->evnt[ipart].dcstat-1].status > 0 &&	// TBT check
        	 PionDeDxCut( (float)Evnt->evnt[ipart].pmom,
	      	  (float)SCPB->scpb[Evnt->evnt[ipart].scstat-1].edep )	// It s an pion 
//		 ( abs( Evnt->evnt[ipart].id ) == 211 )                 //Cheating for now
		 )	 						
	      {
		
		int scpb_code_pi = SCPB->scpb[Evnt->evnt[ipart].scstat-1].scpdht ;
		int Sect_pi  = ( int )  scpb_code_pi / 10000 ;
		int Strip_pi = ( int ) ( ( scpb_code_pi % 10000 ) / 100 );  
		float TDC_pi = SCPB->scpb[Evnt->evnt[ipart].scstat-1].time * CalConst_TOF;
		float Path_pi = SCPB->scpb[Evnt->evnt[ipart].scstat-1].path ; 
		float p_pi = Evnt->evnt[ipart].pmom ;
		float Z_pi = Evnt->evnt[ipart].vert.z ;
		float Beta_pi = 1. / sqrt( 1. + pow( ( PI_CH_MASS / p_pi ), 2) );
		float TOF_pi = ( Path_pi / Beta_pi ) / c_light ;
		float T_vtx_pi = ( Z_pi - Z_0 ) / c_light ;
		float D_T_pi = ( TDC_pi - TOF_pi - T_vtx_pi ) - rf_time1*CalConst_RF + 1000.*RF_Period ;
		float CT_pi = fmod_d( D_T_pi , RF_Period ) - RF_Period/2 ; 	
//cout << "dtpi, ctpi = " << D_T_pi << ", " << CT_pi << endl;
//cout << "tdc_pi, tofpi, tvtxpi, rftime, rfperiod = " << TDC_pi << ", " << TOF_pi << ", " << T_vtx_pi << ", " << rf_time1 << ", " << RF_Period << endl;

		if ( Sect_pi > N_SECTS || Strip_pi > NumStrips::Instance().Get_NumStrips() ) 
		  {
		    printf("Strange SCPB code for pion %d\n",scpb_code_pi );
		    return;
		  } 
		if ( Strip_pi > n_strips_for_el )  
		  Strip[Sect_pi-1][Strip_pi-1]->hist->Fill( CT_pi );
	      }
	}  
    }
}



void TTofCalib::TFineTune::Do()
{
  std::string ModuleName ("TTofCalib::TFineTune::Do");
  std::ostringstream Message;

  TF1* Gauss = new TF1("Gauss","[0]*exp( -1.*pow( x-[1], 2 ) / ( 2*[2]*[2] ) )", -2, +2 );
  float Center, Sigma, xMin, xMax, Max ;

  Message << "Doing Fine Tune Analysis";
  PhyAna->PrintMessage (ModuleName, Message.str());  
 
  for( int iSect = 1; iSect <= N_SECTS; iSect++ )
    for( int iStrip = 1; iStrip <= NumStrips::Instance().Get_NumStrips(); iStrip++ )
      {   
	if( iStrip <= n_strips_for_el )		// Fine Tune with Electrons
	  Sigma = Sigma_el;
	else 					// Fine Tune with pions 
	  Sigma = Sigma_pi;  

	Int_t CenterBin = Strip[iSect-1][iStrip-1]->Rearrange();	// Finding the Center of Histogram^
	
	Center = Strip[iSect-1][iStrip-1]->hist->GetBinCenter( CenterBin );
	
	xMin = Center - 2.*Sigma ;
	xMax = Center + 2.*Sigma ;
	Max = Strip[iSect-1][iStrip-1]->hist->GetBinContent( CenterBin );
   
	Gauss->SetParameters( Max, Center, Sigma );
	
	Strip[iSect-1][iStrip-1]->hist->Fit( "Gauss", "0Q", "", xMin, xMax );
	Strip[iSect-1][iStrip-1]->hist->Fit( "Gauss", "0Q", "", xMin, xMax );
	
	float ampl_fit = Gauss->GetParameter( 0 );
	float t0_fit = Gauss->GetParameter( 1 );
	float err_t0_fit = Gauss->GetParError ( 1 );
	float sigma_fit = fabs( Gauss->GetParameter( 2 ) );
	float err_sigma_fit = fabs( Gauss->GetParError( 2 ) );
   
	if ( 0. > ampl_fit || ampl_fit > 1.e+06 || 
	     1.5 < fabs( t0_fit ) ||
	     ( Sigma/3. ) > sigma_fit || sigma_fit > ( 2.5*Sigma ) 
	     )
	  {   
	    Strip[iSect-1][iStrip-1]->SetBad();
	    Strip[iSect-1][iStrip-1]->SetT0 ( 0. );
	    Strip[iSect-1][iStrip-1]->SetErrT0 ( 2. );	    
	    Strip[iSect-1][iStrip-1]->SetSigma( 2. ); 
	    Strip[iSect-1][iStrip-1]->SetErrSigma( 2. ); 
	  }     
	else    
	  {
	    Strip[iSect-1][iStrip-1]->SetT0 ( t0_fit );
	    Strip[iSect-1][iStrip-1]->SetErrT0 ( err_t0_fit );
	    Strip[iSect-1][iStrip-1]->SetSigma( sigma_fit ); 
	    Strip[iSect-1][iStrip-1]->SetErrSigma( err_sigma_fit ); 
	    Strip[iSect-1][iStrip-1]->SetGood();
	  }
	//	PhyAna->World.SendHisto1F( Strip[iSect-1][iStrip-1]->hist );
      }
  Message << "Fine Tuning Done";
  PhyAna->PrintMessage (ModuleName, Message.str() );  
  DoneFlag = 1;  
}


int TTofCalib::TFineTune::Done()
{
  return DoneFlag;
}


float TTofCalib::TFineTune::GetT0( int iSect, int iStrip )
{
  return Strip[iSect-1][iStrip-1]->GetT0();
}

float TTofCalib::TFineTune::GetErrT0( int iSect, int iStrip )
{
  return Strip[iSect-1][iStrip-1]->GetErrT0();
}

float TTofCalib::TFineTune::GetSigma( int iSect, int iStrip )
{
  return Strip[iSect-1][iStrip-1]->GetSigma();
}

float TTofCalib::TFineTune::GetErrSigma( int iSect, int iStrip )
{
  return Strip[iSect-1][iStrip-1]->GetErrSigma();
}

int TTofCalib::TFineTune::GetStatus( int iSect, int iStrip )
{
  return Strip[iSect-1][iStrip-1]->IsOK();
}


void TTofCalib::TFineTune::Write()
{
  
  // Write Histogtams for all Strips into root file

  for( int iSect = 1; iSect <= N_SECTS; iSect++ )
    for( int iStrip = 1; iStrip <= NumStrips::Instance().Get_NumStrips(); iStrip++ )
      Strip[iSect-1][iStrip-1]->hist->Write();
}




TTofCalib::TFineTune::Strip_t::Strip_t( int Sect, int Strip )
{
  char Name1[20];
  char Title1[50];
  
  iSect = Sect;
  iStrip = Strip;
  OKstatus = 0;
  t0 = 0.;
  err_t0 = 2. ;
  sigma = 0.;
  err_sigma = 2.0 ;
  
  sprintf( Name1, "%1d_%d_fine", iSect, iStrip);
  sprintf( Title1, "%2d  %2d ", iSect, iStrip);
  
  hist = new TH1F( Name1, Title1, n_bins, -RF_Period, +RF_Period );
}


TTofCalib::TFineTune::Strip_t::~Strip_t()
{
  delete hist;
}


int TTofCalib::TFineTune::Strip_t::Rearrange( )
{

  // Rearrange histograms to make it connected 
  // ( remove discontinuity due to fmod )

  TH1F* Histo = hist;
  
  TH1F HistoCopy = (*Histo);
  
  float MaxVal = Histo->GetBinContent( (Int_t)1 );
  int   MaxBin = 1; 
 
  for( int iBin = 1; iBin <= n_bins ; iBin++ )
    if ( Histo->GetBinContent( (Int_t)iBin ) > MaxVal )
      {
	MaxVal = Histo->GetBinContent( (Int_t)iBin );
	MaxBin = iBin ;
      }

  float Center = (float) Histo->GetBinCenter( (Int_t)MaxBin );
  if      ( fabs( Center - 1. ) < 0.35 ) //if 0.65 < center < 1
    {
      for( int iBin = 1; iBin <= n_bins ; iBin++ )
	{
	  float BinPos;
	  if ( ( BinPos = HistoCopy.GetBinCenter( (Int_t) iBin ) ) < 0. )
	    {
	      Stat_t NewCont = HistoCopy.GetBinContent( (Int_t) iBin );
	      Histo->Fill( ( RF_Period + BinPos ) , NewCont );
	      Histo->AddBinContent( (Int_t)  iBin , (Stat_t) -1.*NewCont );
	    } 
	}
    }
  else if ( fabs( Center + 1. ) < 0.35 ) //if -1 < center < -0.65
    {
      for( int iBin = 1; iBin <= n_bins ; iBin++ )
	{
	  float BinPos;
	  if ( ( BinPos = HistoCopy.GetBinCenter( (Int_t) iBin ) ) > 0. )
	    {
	      Stat_t NewCont = HistoCopy.GetBinContent( (Int_t) iBin );
	      Histo->Fill( ( -RF_Period + BinPos ) , NewCont );
	      Histo->AddBinContent( (Int_t)  iBin , (Stat_t) -1.*NewCont );
	    } 
	}
    } 
 
  return MaxBin;
}


void TTofCalib::TFineTune::Strip_t::SetT0( float t )
{
  t0 = t;
}

void TTofCalib::TFineTune::Strip_t::SetErrT0( float err )
{
  err_t0 = err;
}

void TTofCalib::TFineTune::Strip_t::SetSigma( float sig )
{
  sigma = sig;
}

void TTofCalib::TFineTune::Strip_t::SetErrSigma( float err )
{
  err_sigma = err;
}

float TTofCalib::TFineTune::Strip_t::GetT0(  )
{
  return t0;
}

float TTofCalib::TFineTune::Strip_t::GetErrT0(  )
{
  return err_t0;
}

float TTofCalib::TFineTune::Strip_t::GetSigma( )
{
  return sigma;
}

float TTofCalib::TFineTune::Strip_t::GetErrSigma( )
{
  return err_sigma;
}

void TTofCalib::TFineTune::Strip_t::SetGood()
{
  OKstatus = 1;
}


void TTofCalib::TFineTune::Strip_t::SetBad()
{
  OKstatus = 0;
}


int TTofCalib::TFineTune::Strip_t::IsOK()
{
  return OKstatus;
} 

TTofCalib::TCrudeTune::TCrudeTune()
{

  // Initialize Strips and Sectors

  //  printf("Initializing Crude Tune\n");

  int iSect, iStrip;
  for ( iSect = 1; iSect <= N_SECTS ; iSect++ )
    {
      for ( iStrip = 1; iStrip <= NumStrips::Instance().Get_NumStrips(); iStrip++ )
	Strip[iSect-1][iStrip-1] = new Strip_t( iSect, iStrip );
      Sector[iSect-1] = new Sector_t( iSect );  
    } 
  DoneFlag = 0;  
 
  NTuple = new TNtuple("Crude","Crude Tune",
		       "Sect_e:Strip_e:Sect_pi:Strip_pi:T0_e:T0_pi:rf_time");	
  
  histBankAvail = makeHistBankAvail("crude");
}


TTofCalib::TCrudeTune::~TCrudeTune()
{
  
  // Destroy Strips and Sectors

  for( int iSect = 1; iSect <= N_SECTS; iSect++ )
    {
      for( int iStrip = 1; iStrip <= NumStrips::Instance().Get_NumStrips(); iStrip++ )
	delete Strip[iSect-1][iStrip-1] ;
      delete Sector[iSect-1];
    }
  
  delete NTuple;
}


void TTofCalib::TCrudeTune::Process()
{

  clasEVNT_t* Evnt = NULL;
  Evnt = (clasEVNT_t *) getBank(&bcs_,"EVNT"); 
  clasHEVT_t* Hevt = NULL ;
  Hevt = (clasHEVT_t *) getBank(&bcs_,"HEVT");
  clasSCPB_t* SCPB = NULL;
  SCPB = (clasSCPB_t *) getBank(&bcs_,"SCPB");
  clasCCPB_t* CCPB = NULL;
  CCPB = (clasCCPB_t *) getBank(&bcs_,"CCPB");
  clasECPB_t* ECPB = NULL;
  ECPB = (clasECPB_t *) getBank(&bcs_,"ECPB");
  clasDCPB_t* DCPB = NULL;
  DCPB = (clasDCPB_t *) getBank(&bcs_,"DCPB");
  clasCL01_t* CL01 = NULL; 
  CL01 = (clasCL01_t *) getBank(&bcs_,"CL01");

  if (Evnt) histBankAvail->Fill(2.);
  if (Hevt) histBankAvail->Fill(3.);
  if (SCPB) histBankAvail->Fill(4.);
  if (CCPB) histBankAvail->Fill(5.);
  if (ECPB) histBankAvail->Fill(6.);
  if (DCPB) histBankAvail->Fill(7.);
  if (CL01) histBankAvail->Fill(8.);
  

  if ( Hevt == 0 || Evnt == 0 || SCPB == 0 || DCPB == 0 || 
       CCPB == 0 || ECPB == 0 || CL01 == 0 ) return;
 
  histBankAvail->Fill(1.);
  
  if ( ( Hevt->hevt[0].npgp % 100 ) > 1 )		// There are 2 tracks
    {
      if ( 
      	   Evnt->evnt[0].ecstat > 0 &&
      	   Evnt->evnt[0].dcstat > 0 && 			// first particle hit DC
	   Evnt->evnt[0].scstat > 0 &&			// first particle hit SC
	   Evnt->evnt[0].ccstat > 0 &&			// first particle hit CC
	   CCPB->ccpb[0].nphe   > 25 &&                 // More than 1 p-El in CC
	   DCPB->dcpb[Evnt->evnt[0].dcstat-1].status > 0 &&	// TBT check
	   Evnt->evnt[0].charge < 0 && 			// Negative
	   Evnt->evnt[0].id == 11				// It s an electron
	   )	
	{
	  float rf_time1 = CL01->cl01[0].rf ;
	  //float rf_time1 = Hevt->hevt[0].rf1 ;
  
	  //	Electron 	//  

	  int scpb_code_el = SCPB->scpb[Evnt->evnt[0].scstat-1].scpdht ;
	  float Sect_e  = ( int )  scpb_code_el / 10000 ;
	  float Strip_e = ( int ) ( ( scpb_code_el % 10000 ) / 100 );  
	  float TDC_e = SCPB->scpb[Evnt->evnt[0].scstat-1].time * CalConst_TOF ;
	  float Path_e = SCPB->scpb[Evnt->evnt[0].scstat-1].path ; 
	  float Z_el = Evnt->evnt[0].vert.z ;
	  float TOF_e = Path_e / c_light ;
	  float T_vtx_el = ( Z_el - Z_0 ) / c_light ;
	  float T0_e = ( TDC_e - TOF_e - T_vtx_el );

	  if ( Sect_e > N_SECTS || Strip_e > NumStrips::Instance().Get_NumStrips() ) 
	    {
	      printf("Strange SCPB code for electron %d\n",scpb_code_el);
	      return;
	    } 
	  
	  //	Looping for Pions
	  
	  for ( int ipart = 1; ipart < ( Hevt->hevt[0].npgp % 100 ); ipart++ )
	    if ( Evnt->evnt[ipart].dcstat > 0 && 			// particle hit DC
		 Evnt->evnt[ipart].scstat > 0 &&			// particle hit SC
		 DCPB->dcpb[Evnt->evnt[ipart].dcstat-1].status > 0 &&	// TBT check
	       	 PionDeDxCut( (float)Evnt->evnt[ipart].pmom,
        	  (float)SCPB->scpb[Evnt->evnt[ipart].scstat-1].edep )	// It s an pion 
//		 ( abs( Evnt->evnt[ipart].id ) == 211 )                 // Cheating for now
		 )	 							
	      {
    
		int scpb_code_pi = SCPB->scpb[Evnt->evnt[ipart].scstat-1].scpdht ;
		float Sect_pi  = ( int )  scpb_code_pi / 10000 ;
		float Strip_pi = ( int ) ( ( scpb_code_pi % 10000 ) / 100 );  
		float TDC_pi = SCPB->scpb[Evnt->evnt[ipart].scstat-1].time * CalConst_TOF;
		float Path_pi = SCPB->scpb[Evnt->evnt[ipart].scstat-1].path ; 
		float p_pi = Evnt->evnt[ipart].pmom ;
		float Z_pi = Evnt->evnt[ipart].vert.z ;
		float Beta_pi = 1. / sqrt( 1. + pow( ( PI_CH_MASS / p_pi ), 2) );
		float TOF_pi = ( Path_pi / Beta_pi ) / c_light ;
		float T_vtx_pi = ( Z_pi - Z_0 ) / c_light ;

		if ( Sect_pi > N_SECTS || Strip_pi > NumStrips::Instance().Get_NumStrips() ) 
		  {
		    printf("Strange SCPB code for pion %d\n",scpb_code_pi );
		    return;
		  } 
		float T0_pi = ( TDC_pi - TOF_pi - T_vtx_pi );      
     
		NTuple->Fill( Sect_e, Strip_e, Sect_pi, Strip_pi, T0_e, T0_pi, rf_time1*CalConst_RF );
		
	      }
	}  
    }
}


void TTofCalib::TCrudeTune::Do( float TuneConst[][N_MAX_STRIPS] )
{
  std::string ModuleName ("TTofCalib::TCrudeTune::Do");
  std::ostringstream Message;

  Message << "Doing Crude Tune Analysis";
  PhyAna->PrintMessage (ModuleName, Message.str());  

  DoFirst10( TuneConst );
  DoSect2Sect( TuneConst );
  DoLast38( TuneConst );
  DoFirst10Again( TuneConst );

  Message << "Crude Tune Analysis Done";
  PhyAna->PrintMessage (ModuleName, Message.str());  

  DoneFlag = 1;
}


void TTofCalib::TCrudeTune::DoFirst10( float TuneConst[][N_MAX_STRIPS] )
{
  Float_t Sect_e,
    Strip_e,
    Sect_pi,
    Strip_pi,
    T0_e,
    T0_pi,
    rf_time;
 		
  NTuple->SetBranchAddress( "Sect_e", &Sect_e ); 		
  NTuple->SetBranchAddress( "Strip_e", &Strip_e ); 		
  NTuple->SetBranchAddress( "Sect_pi", &Sect_pi ); 		
  NTuple->SetBranchAddress( "Strip_pi", &Strip_pi ); 		
  NTuple->SetBranchAddress( "T0_e", &T0_e ); 		
  NTuple->SetBranchAddress( "T0_pi", &T0_pi ); 	
  NTuple->SetBranchAddress( "rf_time", &rf_time ); 	
 
  Int_t n_entries = (Int_t)NTuple->GetEntries();
  Int_t nbytes = 0;
  int n_bunches;
 
  for ( Int_t iEvt = 0; iEvt < n_entries; iEvt++) 
    {                   
      nbytes += NTuple->GetEvent( iEvt );
      int iSect_e   = (int) Sect_e;
      int iStrip_e  = (int) Strip_e;
      int iSect_pi  = (int) Sect_pi;
      int iStrip_pi = (int) Strip_pi; 
        
      if ( iStrip_e <= 10 &&  
	   PiIsOpposite( iSect_e, iStrip_e, iSect_pi, iStrip_pi ) )
	{
	  float delta_T0 = ( T0_e - TuneConst[iSect_e-1][iStrip_e-1]  - 
			     ( fmod_d( T0_e - TuneConst[iSect_e-1][iStrip_e-1] - rf_time
				     + 1000.*RF_Period, RF_Period    ) - RF_Period/2. ) ) -
	    ( T0_pi - TuneConst[iSect_pi-1][iStrip_pi-1]  - 
	      ( fmod_d( T0_pi - TuneConst[iSect_pi-1][iStrip_pi-1] - rf_time 
		      + 1000.*RF_Period, RF_Period ) - RF_Period/2. ) );

//	  float delta_T0 = (T0_e  - FineTune_e  - (fmod_d(T0_e  - FineTune_e  - rf_time + 2004., 2.004) - 1.002)) -
//	                   (T0_pi - FineTune_pi - (fmod_d(T0_pi - FineTune_pi - rf_time + 2004., 2.004) - 1.002));

	  Strip[iSect_e-1][iStrip_e-1]->hist->Fill( delta_T0 ); 
	}  
    }

  for ( int iSect_e = 1; iSect_e <= N_SECTS; iSect_e++ )
    for ( int iStrip_e = 1; iStrip_e <= 10; iStrip_e++ )
      {
	if ( Strip[iSect_e-1][iStrip_e-1]->hist->GetEntries() > 4 )
	  n_bunches = RoundOff( Strip[iSect_e-1][iStrip_e-1]->GetMax() / RF_Period );
	else
	  n_bunches = 0;
//	    printf("%d\n", n_bunches );
	Strip[iSect_e-1][iStrip_e-1]->SetNBunches( n_bunches );
	TuneConst[iSect_e-1][iStrip_e-1] += n_bunches * RF_Period ; 
      }
}



void TTofCalib::TCrudeTune::DoSect2Sect( float TuneConst[][N_MAX_STRIPS] )
{
  Float_t Sect_e,
    Strip_e,
    Sect_pi,
    Strip_pi,
    T0_e,
    T0_pi,
    rf_time;
 		
  NTuple->SetBranchAddress( "Sect_e", &Sect_e ); 		
  NTuple->SetBranchAddress( "Strip_e", &Strip_e ); 		
  NTuple->SetBranchAddress( "Sect_pi", &Sect_pi ); 		
  NTuple->SetBranchAddress( "Strip_pi", &Strip_pi ); 		
  NTuple->SetBranchAddress( "T0_e", &T0_e ); 		
  NTuple->SetBranchAddress( "T0_pi", &T0_pi ); 	
  NTuple->SetBranchAddress( "rf_time", &rf_time ); 	
 
  Int_t n_entries = (Int_t) NTuple->GetEntries();
  Int_t nbytes = 0;
  int n_bunches;
  
  for (Int_t iEvt = 0; iEvt < n_entries; iEvt++) 
    {
      nbytes += NTuple->GetEvent( iEvt );
      int iSect_e   = (int) Sect_e;
      int iStrip_e  = (int) Strip_e;
      int iSect_pi  = (int) Sect_pi;
      int iStrip_pi = (int) Strip_pi;
      
      if ( iStrip_e <= 10 && iStrip_pi <= 10 &&
	   iSect_pi == 1 )
	{
	  float delta_T0 = ( T0_e - TuneConst[iSect_e-1][iStrip_e-1]  - 
			     ( fmod_d( T0_e - TuneConst[iSect_e-1][iStrip_e-1] - rf_time
				     + 1000.*RF_Period, RF_Period    ) - RF_Period/2. ) ) -
	    ( T0_pi - TuneConst[iSect_pi-1][iStrip_pi-1]  - 
	      ( fmod_d( T0_pi - TuneConst[iSect_pi-1][iStrip_pi-1] - rf_time
		      + 1000.*RF_Period, RF_Period ) - RF_Period/2. ) );

	  Sector[iSect_e-1]->hist->Fill( delta_T0 ); 
	}  
    }

  for ( int iSect_e = 1; iSect_e <= N_SECTS; iSect_e++ )
    {
      if ( Sector[iSect_e-1]->hist->GetEntries() > 4 )
	n_bunches = RoundOff( Sector[iSect_e-1]->GetMax() / RF_Period );
      else
	n_bunches = 0; 
      
      Sector[iSect_e-1]->SetNBunches( n_bunches );
   
      for ( int iStrip_e = 1; iStrip_e <= 10; iStrip_e++ )
	TuneConst[iSect_e-1][iStrip_e-1] += n_bunches * RF_Period ; 
    } 
}



void TTofCalib::TCrudeTune::DoLast38( float TuneConst[][N_MAX_STRIPS] )
{
 Float_t 	Sect_e,
 		Strip_e,
 		Sect_pi,
 		Strip_pi,
 		T0_e,
 		T0_pi,
 		rf_time;
 		
 NTuple->SetBranchAddress( "Sect_e", &Sect_e ); 		
 NTuple->SetBranchAddress( "Strip_e", &Strip_e ); 		
 NTuple->SetBranchAddress( "Sect_pi", &Sect_pi ); 		
 NTuple->SetBranchAddress( "Strip_pi", &Strip_pi ); 		
 NTuple->SetBranchAddress( "T0_e", &T0_e ); 		
 NTuple->SetBranchAddress( "T0_pi", &T0_pi ); 	
 NTuple->SetBranchAddress( "rf_time", &rf_time ); 	
 
 Int_t n_entries = (Int_t) NTuple->GetEntries();
 Int_t nbytes = 0;
 int n_bunches;
 
 for (Int_t iEvt = 0; iEvt < n_entries; iEvt++) 
 {
   nbytes += NTuple->GetEvent( iEvt );
   int iSect_e   = (int) Sect_e;
   int iStrip_e  = (int) Strip_e;
   int iSect_pi  = (int) Sect_pi;
   int iStrip_pi = (int) Strip_pi;

   if ( iStrip_e <= 10 && iStrip_pi > 10 )
     {
       float delta_T0 = ( T0_e - TuneConst[iSect_e-1][iStrip_e-1]  - 
			  ( fmod_d( T0_e - TuneConst[iSect_e-1][iStrip_e-1] - rf_time
				  + 1000.*RF_Period, RF_Period    ) - RF_Period/2. ) ) -
	 ( T0_pi - TuneConst[iSect_pi-1][iStrip_pi-1]  - 
	   ( fmod_d( T0_pi - TuneConst[iSect_pi-1][iStrip_pi-1] - rf_time
		   + 1000.*RF_Period, RF_Period ) - RF_Period/2. ) );
       
       Strip[iSect_pi-1][iStrip_pi-1]->hist->Fill( -1.*delta_T0 ); 
     }  
 }

 for ( int iSect_pi = 1; iSect_pi <= N_SECTS; iSect_pi++ )
   for ( int iStrip_pi = 11; iStrip_pi <= NumStrips::Instance().Get_NumStrips(); iStrip_pi++ )
     {
       if ( Strip[iSect_pi-1][iStrip_pi-1]->hist->GetEntries() > 4 )
	 n_bunches = RoundOff( Strip[iSect_pi-1][iStrip_pi-1]->GetMax() / RF_Period );
       else
	 n_bunches = 0; 
       Strip[iSect_pi-1][iStrip_pi-1]->SetNBunches( n_bunches );
       
       TuneConst[iSect_pi-1][iStrip_pi-1] += n_bunches * RF_Period ; 
     }
}




void TTofCalib::TCrudeTune::DoFirst10Again( float TuneConst[][N_MAX_STRIPS] )
{
 Float_t 	Sect_e,
 		Strip_e,
 		Sect_pi,
 		Strip_pi,
 		T0_e,
 		T0_pi,
 		rf_time;
 		
 NTuple->SetBranchAddress( "Sect_e", &Sect_e ); 		
 NTuple->SetBranchAddress( "Strip_e", &Strip_e ); 		
 NTuple->SetBranchAddress( "Sect_pi", &Sect_pi ); 		
 NTuple->SetBranchAddress( "Strip_pi", &Strip_pi ); 		
 NTuple->SetBranchAddress( "T0_e", &T0_e ); 		
 NTuple->SetBranchAddress( "T0_pi", &T0_pi ); 	
 NTuple->SetBranchAddress( "rf_time", &rf_time ); 	
 
 Int_t n_entries = (Int_t) NTuple->GetEntries();
 Int_t nbytes = 0;
 int n_bunches;
 
 for ( int iSect_e = 1; iSect_e <= N_SECTS; iSect_e++ )
   for ( int iStrip_e = 1; iStrip_e <= 10; iStrip_e++ )
     Strip[iSect_e-1][iStrip_e-1]->hist->Reset();
 
 for ( Int_t iEvt = 0; iEvt < n_entries; iEvt++) 
   {
     nbytes += NTuple->GetEvent( iEvt );
     int iSect_e   = (int) Sect_e;
     int iStrip_e  = (int) Strip_e;
     int iSect_pi  = (int) Sect_pi;
     int iStrip_pi = (int) Strip_pi;
     
     if ( iStrip_e <= 10 )
       {
	 float delta_T0 = ( T0_e - TuneConst[iSect_e-1][iStrip_e-1]  - 
			    ( fmod_d( T0_e - TuneConst[iSect_e-1][iStrip_e-1] - rf_time
				    + 1000.*RF_Period, RF_Period    ) - RF_Period/2. ) ) -
	   ( T0_pi - TuneConst[iSect_pi-1][iStrip_pi-1]  - 
	     ( fmod_d( T0_pi - TuneConst[iSect_pi-1][iStrip_pi-1] - rf_time
		     + 1000.*RF_Period, RF_Period ) - RF_Period/2. ) );
	 
	 Strip[iSect_e-1][iStrip_e-1]->hist->Fill( delta_T0 ); 
       }  
   }

 for ( int iSect_e = 1; iSect_e <= N_SECTS; iSect_e++ )
   for ( int iStrip_e = 1; iStrip_e <= 10; iStrip_e++ )
     {
       if ( Strip[iSect_e-1][iStrip_e-1]->hist->GetEntries() > 4 )
	 n_bunches = RoundOff( Strip[iSect_e-1][iStrip_e-1]->GetMax() / RF_Period );
       else 
	 n_bunches = 0; 
       Strip[iSect_e-1][iStrip_e-1]->SetNBunches( n_bunches );
       
       TuneConst[iSect_e-1][iStrip_e-1] += n_bunches * RF_Period ; 
     }
}




int TTofCalib::TCrudeTune::Done()
{
  return DoneFlag;
}

void TTofCalib::TCrudeTune::Write()
{
  
  // Write Histogtams for all Strips into root file
  
  for( int iSect = 1; iSect <= N_SECTS; iSect++ )
    {
      for( int iStrip = 1; iStrip <= NumStrips::Instance().Get_NumStrips(); iStrip++ )
	Strip[iSect-1][iStrip-1]->hist->Write();
      Sector[iSect-1]->hist->Write();
    }
}


int TTofCalib::TCrudeTune::PiIsOpposite( int iSect_e, int iStrip_e, int iSect_pi, int iStrip_pi )
{
  
  int iStrip_pi_Ref = 28;
  int iSect_op;
  
  switch ( iSect_e )
    { 
    case 1: iSect_op = 5; break;
    case 2: iSect_op = 6; break;
    case 3: iSect_op = 1; break;
    case 4: iSect_op = 2; break;
    case 5: iSect_op = 3; break;
    case 6: iSect_op = 4; break; 
    default: break;
    } 
  
  if ( ( iStrip_pi == iStrip_pi_Ref ) && ( iSect_pi == iSect_op ) ) 
    return 1;
  else
    return 0; 
}


int TTofCalib::TCrudeTune::GetNBunches( int iSect, int iStrip )
{
  return (int)( Strip[iSect-1][iStrip-1]->GetNBunches() + Sector[iSect-1]->GetNBunches() );
}



TTofCalib::TCrudeTune::Strip_t::Strip_t( int Sect, int Strip )
{
  char Name1[20];
  char Title1[50];
  
  iSect = Sect;
  iStrip = Strip;
  n_bunch = 0;
  
  sprintf( Name1, "%1d_%d_crude", iSect, iStrip );
  sprintf( Title1, "%2d  %2d ", iSect, iStrip);
  
  hist = new TH1F( Name1, Title1, n_bins, -30., +30. ); 
}


TTofCalib::TCrudeTune::Strip_t::~Strip_t()
{
  delete hist;
}


float TTofCalib::TCrudeTune::Strip_t::GetMax( )
{
  
  int MaxBin = 1;
  float MaxVal = hist->GetBinContent( (Int_t) MaxBin );
  
  for ( int iBin = 1; iBin <= n_bins; iBin++ )
    if ( hist->GetBinContent( (Int_t) iBin ) > MaxVal )
      {
	MaxVal = (float) hist->GetBinContent( (Int_t) iBin );
	MaxBin = iBin;
      }
  return (float) hist->GetBinCenter( (Int_t ) MaxBin );
}


void TTofCalib::TCrudeTune::Strip_t::SetNBunches( int n_bunches )
{
  n_bunch = n_bunches;
}


int TTofCalib::TCrudeTune::Strip_t::GetNBunches( )
{
  return n_bunch;
}



TTofCalib::TCrudeTune::Sector_t::Sector_t( int Sect )
{
  char Name1[20];
  char Title1[50];
  
  iSect = Sect;
  n_bunch = 0;
  
  sprintf( Name1, "%1d_crude", iSect );
  sprintf( Title1, "Sect %2d  ", iSect );
  
  hist = new TH1F( Name1, Title1, n_bins, -20., +20. );
}


TTofCalib::TCrudeTune::Sector_t::~Sector_t()
{
  delete hist;
}


float TTofCalib::TCrudeTune::Sector_t::GetMax( )
{
  
  int MaxBin = 1;
  float MaxVal = hist->GetBinContent( (Int_t) MaxBin );
  
  for ( int iBin = 1; iBin <= n_bins; iBin++ )
    if ( hist->GetBinContent( (Int_t) iBin ) > MaxVal )
      {
	MaxVal = (float) hist->GetBinContent( (Int_t) iBin );
	MaxBin = iBin;
      }
  return (float) hist->GetBinCenter( (Int_t ) MaxBin );
}


void TTofCalib::TCrudeTune::Sector_t::SetNBunches( int n_bunches )
{
  n_bunch = n_bunches;
}


int TTofCalib::TCrudeTune::Sector_t::GetNBunches( )
{
  return n_bunch;
}




TTofCalib::TCalibTest::TCalibTest()
{
  
  // Initialize Histograms and NTuple
  char Name[30];
  char Title[50];
  
  //  printf("Initializing Calibration Test\n");
  
  
  for ( int iVer = 0; iVer <= 1; iVer++ )
    {   
      sprintf( Name, "BvsP_%1d", iVer );
      sprintf( Title, "Beta vs SP , %d", iVer );  
      BetavsP[iVer] = new TH2F( Name, Title, 100, 0., 3.5 , 100, 0., 1.3 );
      
      sprintf( Name, "MSpec_%1d", iVer );
      sprintf( Title, "Mass Spectrum , %d", iVer );  
      M_Spec[iVer] = new TH1F( Name, Title, 120, 0., 1.2  );
      
      for ( int iSect = 1; iSect <= N_SECTS; iSect++ )
	{
	  sprintf( Name, "MvsS_%1d_%1d", iVer, iSect );
	  sprintf( Title, "M2 vs Strip , %d, Sector %1d", iVer, iSect );  
	  M2vsStrip[iVer][iSect-1] = new TH2F( Name, Title, NumStrips::Instance().Get_NumStrips(), 0.5, NumStrips::Instance().Get_NumStrips() + 0.5 , 100, -0.1, 1.3 );
	}
    }  
  fRF_Test = new TH2F( "GoodRF", "Good RF Test", 100, 0., 100., 70, -2., +2. );
  fRF_Test_sngl = new TH2F( "GoodRF_sngl", "Good RF Test, Single", 100, 0., 100., 70, -2., +2. );
  PhotStat = new TH1F( "PhotoStat", "CC photon statistics", 100, 0., 300. );
  NTuple = new TNtuple("Test","TOF Calibration Test",
		       "Sect_e:Strip_e:Sect_h:Strip_h:T_st:T_h:P_h:Path_h:rf_time:n_ph_e");	

  histBankAvail = makeHistBankAvail("calib_test");

}


TTofCalib::TCalibTest::~TCalibTest()
{
  delete NTuple;
}


void TTofCalib::TCalibTest::Process()
{

  clasEVNT_t* Evnt = NULL;
  Evnt = (clasEVNT_t *) getBank(&bcs_,"EVNT"); 
  clasHEVT_t* Hevt = NULL ;
  Hevt = (clasHEVT_t *) getBank(&bcs_,"HEVT");
  clasSCPB_t* SCPB = NULL;
  SCPB = (clasSCPB_t *) getBank(&bcs_,"SCPB");
  clasCCPB_t* CCPB = NULL;
  CCPB = (clasCCPB_t *) getBank(&bcs_,"CCPB");
  clasECPB_t* ECPB = NULL;
  ECPB = (clasECPB_t *) getBank(&bcs_,"ECPB");
  clasDCPB_t* DCPB = NULL;
  DCPB = (clasDCPB_t *) getBank(&bcs_,"DCPB");
  clasCL01_t* CL01 = NULL; 
  CL01 = (clasCL01_t *) getBank(&bcs_,"CL01");

  if (Evnt) histBankAvail->Fill(2.);
  if (Hevt) histBankAvail->Fill(3.);
  if (SCPB) histBankAvail->Fill(4.);
  if (CCPB) histBankAvail->Fill(5.);
  if (ECPB) histBankAvail->Fill(6.);
  if (DCPB) histBankAvail->Fill(7.);
  if (CL01) histBankAvail->Fill(8.);
  
  if ( Hevt == 0 || Evnt == 0 || SCPB == 0 || DCPB == 0 || 
       CCPB == 0 || ECPB == 0 || CL01 == 0 ) return;

  histBankAvail->Fill(1.);
 
  if ( ( Hevt->hevt[0].npgp % 100 ) > 1 )		// There are 2 tracks
    {
      if ( Evnt->evnt[0].dcstat > 0 && 			// first particle hit DC
	   Evnt->evnt[0].scstat > 0 &&			// first particle hit SC
	   Evnt->evnt[0].ccstat > 0 &&                  // first particle hit CC
	   CCPB->ccpb[0].nphe   > 35 &&                 // More than 1 p-El in CC
	   DCPB->dcpb[Evnt->evnt[0].dcstat-1].status > 0 &&	// TBD check
	   Evnt->evnt[0].charge < 0 && 		         	// Negative
	   Evnt->evnt[0].id == 11				// It s an electron
	   )	
	{
	  float rf_time1 = CL01->cl01[0].rf ;
	  //float rf_time1 = Hevt->hevt[0].rf1 ;
	  
	  //	Electron Part of fine Tuning	//  
	  
	  int scpb_code_el = SCPB->scpb[Evnt->evnt[0].scstat-1].scpdht ;
	  float Sect_e  = ( int )  scpb_code_el / 10000 ;
	  float Strip_e = ( int ) ( ( scpb_code_el % 10000 ) / 100 );  
	  float TDC_e = SCPB->scpb[Evnt->evnt[0].scstat-1].time * CalConst_TOF ;
	  float Path_e = SCPB->scpb[Evnt->evnt[0].scstat-1].path ; 
	  float Z_el = Evnt->evnt[0].vert.z ;
	  float TOF_e = Path_e / c_light ;
	  float T_vtx_el = ( Z_el - Z_0 ) / c_light ;
	  
	  Float_t n_ph_e = CCPB->ccpb[0].nphe ;
	  PhotStat->Fill( n_ph_e , 1. );

	  if ( Sect_e > N_SECTS || Strip_e > NumStrips::Instance().Get_NumStrips() ) 
	    {
	      printf("Strange SCPB code for electron %d\n",scpb_code_el);
	      return;
	    }  
	  Float_t T0_e = ( TDC_e - TOF_e - T_vtx_el );


	  // Histogram to test RF calibrations used for TOF calibrations

	  float CT =  ( fmod_d(( T0_e - rf_time1 + 1000.*RF_Period ), RF_Period ) - RF_Period/2. );
	  fRF_Test->Fill( rf_time1, CT, 1. );
	  if ( ( Sect_e == 1 ) && ( Strip_e == 4 ) ) 
	    {
	      fRF_Test_sngl->Fill( T0_e - rf_time1, CT, 1 );
	    }
	  
	  //	Looping for Pions ( actually any hadron !! neglect _pi suffix )
	  
	  for ( int ipart = 1; ipart < ( Hevt->hevt[0].npgp % 100 ); ipart++ )
	    if ( Evnt->evnt[ipart].dcstat > 0 && 			// particle hit DC
		 Evnt->evnt[ipart].scstat > 0 &&			// particle hit SC
		 DCPB->dcpb[Evnt->evnt[ipart].dcstat-1].status > 0 &&
		 Evnt->evnt[ipart].charge != 0 			// Positive
	        )	 							
	      {
		int scpb_code_pi = SCPB->scpb[Evnt->evnt[ipart].scstat-1].scpdht ;
		Float_t Sect_pi  = ( int )  scpb_code_pi / 10000 ;
		Float_t Strip_pi = ( int ) ( ( scpb_code_pi % 10000 ) / 100 );  
		Float_t TDC_pi = SCPB->scpb[Evnt->evnt[ipart].scstat-1].time * CalConst_TOF;
		Float_t Path_pi = SCPB->scpb[Evnt->evnt[ipart].scstat-1].path ; 
		Float_t p_pi = Evnt->evnt[ipart].pmom ;
		//		float Z_pi = Evnt->evnt[ipart].vert.z ;
		TDC_pi -= T_vtx_el ;                 // To match the vertex position of electron
		if ( Sect_pi > N_SECTS || Strip_pi > NumStrips::Instance().Get_NumStrips() ) 
		  {
		    printf("Strange SCPB code for pion %d\n",scpb_code_pi );
		    return;
		  } 
		
		NTuple->Fill( Sect_e, Strip_e, Sect_pi, Strip_pi, 
			      T0_e, TDC_pi, p_pi, Path_pi, (Float_t)rf_time1*CalConst_RF, n_ph_e );
		
	      }
	}  
    }
}


void TTofCalib::TCalibTest::Do( float TuneConst[][N_MAX_STRIPS], int Status[][N_MAX_STRIPS] )
{
  Float_t	Sect_e,
 		Strip_e,
 		Sect_h,
 		Strip_h,
 		T_st,
 		T_h,
 		P_h, 
 		Path_h ,
 		rf_time,
                n_ph_e ;
 
  std::string ModuleName ("TTofCalib::TCalibTest::Do");
  std::ostringstream Message;
  
  Message << "Filling Histograms for Test";
  PhyAna->PrintMessage (ModuleName, Message.str() );  
  
  NTuple->SetBranchAddress( "Sect_e", &Sect_e ); 		
  NTuple->SetBranchAddress( "Strip_e", &Strip_e ); 		
  NTuple->SetBranchAddress( "Sect_h", &Sect_h ); 		
  NTuple->SetBranchAddress( "Strip_h", &Strip_h ); 		
  NTuple->SetBranchAddress( "T_st", &T_st ); 		
  NTuple->SetBranchAddress( "T_h", &T_h ); 	
  NTuple->SetBranchAddress( "P_h", &P_h ); 	
  NTuple->SetBranchAddress( "Path_h", &Path_h ); 	
  NTuple->SetBranchAddress( "rf_time", &rf_time ); 	
  NTuple->SetBranchAddress( "n_ph_e", &n_ph_e ); 	

  Int_t n_entries = (Int_t) NTuple->GetEntries();
  Int_t nbytes = 0;
  int n_bunches;

  TH2F* B_P[N_SECTS][N_MAX_STRIPS] ;
  for ( int iSc = 1 ; iSc <= N_SECTS; iSc++ )
    for ( int iSt = 1; iSt <= NumStrips::Instance().Get_NumStrips(); iSt++ )
      {
	char Title[15];
	char Name[10] ;
	sprintf( Title, "Sec %d Strip %d ", iSc, iSt );
	sprintf( Name, "_%d_%d", iSc, iSt );
	B_P[iSc-1][iSt-1] = new TH2F( Name, Title, 50, 0., 3.5, 50, 0., 1.3 );
      }
  TH2F* BadStrip_e = new TH2F("BadStrip_e", "Bad electron Strip", 6, 0.5, 6.5, NumStrips::Instance().Get_NumStrips(), 0.5, NumStrips::Instance().Get_NumStrips() + 0.5 );
  TH2F* BadStrip_h = new TH2F("BadStrip_h", "Bad hadron   Strip", 6, 0.5, 6.5, NumStrips::Instance().Get_NumStrips(), 0.5, NumStrips::Instance().Get_NumStrips() + 0.5 );

  for ( Int_t iEvt = 0; iEvt < n_entries; iEvt++ ) 
    {
      nbytes += NTuple->GetEvent( iEvt );
      int iSect_e   = (int) Sect_e ;
      int iStrip_e  = (int) Strip_e;
      int iSect_h   = (int) Sect_h ;
      int iStrip_h  = (int) Strip_h;
      
      if ( ( iSect_e * iStrip_e * iSect_h * iStrip_h ) == 0 ) 
	{
	  printf("***Error, Strip or Sector Number is Zero !" );
	}
      
      //      if ( ( Status[iSect_e-1][iStrip_e-1] != 0 ) && 
      //	   ( Status[iSect_h-1][iStrip_h-1] != 0 ) &&
      //	   ( iStrip_h < 40 ) && ( iStrip_e < 40 )
      //	   ) 
      if ( ( n_ph_e > 15. ) )             //  minimum cutoff on number of photoelectrons in CC
	  
	{     
	  float TOF_h_0 = T_h - 
	    (  T_st - 
	       ( fmod_d( T_st - rf_time + 1000.*RF_Period, RF_Period ) - RF_Period/2. )
	       );
	  float TOF_h_1 = T_h - TuneConst[iSect_h-1][iStrip_h-1] -
	    ( T_st - TuneConst[iSect_e-1][iStrip_e-1] - 
	      ( fmod_d( T_st - TuneConst[iSect_e-1][iStrip_e-1] - rf_time
		      + 1000.*RF_Period, RF_Period ) - RF_Period/2. )
	      );
	  
	  Axis_t Beta_0 = ( Path_h / TOF_h_0 ) / c_light ;
	  Axis_t Beta_1 = ( Path_h / TOF_h_1 ) / c_light ; 

	  if ( Beta_0 > 1.02 && P_h > 0.35 && Beta_0 < 2. && P_h < 1.2 ) 
	    {
	      BadStrip_h->Fill( Sect_h, Strip_h, 1. );
	      BadStrip_e->Fill( Sect_e, Strip_e, 1. );
	    }
	  Axis_t M_2_0 = P_h*P_h * ( 1./( Beta_0 * Beta_0 ) - 1. );
	  Axis_t M_2_1 = P_h*P_h * ( 1./( Beta_1 * Beta_1 ) - 1. );
	  
	  //    if ( 1 )
	  {
	    M2vsStrip[0][iSect_h-1]->Fill( (Axis_t)Strip_h, (Axis_t)M_2_0, 1. );
	    M2vsStrip[1][iSect_h-1]->Fill( (Axis_t)Strip_h, (Axis_t)M_2_1, 1. );
	    
	    BetavsP[0]->Fill( (Axis_t)P_h, (Axis_t)Beta_0, 1. );
	    BetavsP[1]->Fill( (Axis_t)P_h, (Axis_t)Beta_1, 1. );

	    B_P[iSect_h-1][iStrip_h-1]->Fill(  (Axis_t)P_h, (Axis_t)Beta_1, 1. );
	    
	    if ( M_2_0 > 0. ) M_Spec[0]->Fill( (Axis_t)sqrt( M_2_0 ), 1. );
	    if ( M_2_1 > 0. ) M_Spec[1]->Fill( (Axis_t)sqrt( M_2_1 ), 1. );
	  }
	} 
    }   
  Message << "Test Histograms Filled";
  PhyAna->PrintMessage (ModuleName, Message.str());    


}




int PionDeDxCut( float P, float E )
{
  if ( ( 6.0 < E ) && ( E < 17.-6. * P ) )
    return 1;
  else
    return 0; 
}

int RoundOff( float x )
{
  if ( fabs( x - ( (int)x ) )  > 0.5 )
    return (int)( (int)x + (int) ( x/fabs(x) ) );
  else
    return (int)x;  
}


