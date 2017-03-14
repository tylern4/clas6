//Class authored by Hovanes Egiyan, updated and added to clas packages by Paul Mattione

#include "TPhotTiming.h"

Char_t* TPhotTiming::DBHost   = "clasdb.jlab.org";
Char_t* TPhotTiming::UserName = "clasuser";
Char_t* TPhotTiming::DataBase = "calib";
Int_t   TPhotTiming::MinTrkNum = 2;

TString TPhotTiming::PionIdMethod = "time";

TDatabasePDG TPhotTiming::DbPDG ;

Double_t TPhotTiming::cLight = 29.9792458;
Double_t TPhotTiming::RfConst = 2.0039 ;
Double_t TPhotTiming::ConstShift = 500. * TPhotTiming::RfConst;

Int_t TPhotTiming::kObjectNumber = 0;




TPhotTiming::TPhotTiming( const Char_t* FileName,  
			  const Char_t* OutFileName, 
			  const Char_t* runIndex, int runno) {
  kReadOnly   = 0;
  kFitDone = 0;
  kRunNum  = runno;
  kSC_VERSION_FLAG = (kRunNum < 55357) ? 1 : 2;
  kN_STRP = (kSC_VERSION_FLAG == 1) ? 48 : 57;

  fReader.Add( FileName );

  // Open root file
  fOutFileName = OutFileName ;
  fOutFile = new TFile( fOutFileName.Data() , "RECREATE" );

  this->InitConstants();
  this->BookHistos();

  fRunIndex = runIndex;
  InitCalDB();

  kObjNum = ++(TPhotTiming::kObjectNumber) ;
}



TPhotTiming::TPhotTiming( TFile* OutFile, const Char_t* runIndex, int runno) {
  fOutFile = OutFile;

  kReadOnly  = 1;     // This means that this file is not to be closed 
  kFitDone = 0;
  kRunNum  = runno;
  kSC_VERSION_FLAG = (kRunNum < 55357) ? 1 : 2;
  kN_STRP = (kSC_VERSION_FLAG == 1) ? 48 : 57;

  this->InitConstants();
  this->ReadHistos();

  fRunIndex = runIndex;
  InitCalDB();

  kObjNum = ++(TPhotTiming::kObjectNumber) ;
}



TPhotTiming::~TPhotTiming() {
  // If file is open and is not for reading then the object should close it
  if ( ( fOutFile !=0 ) && ( fOutFile->IsOpen() ) && ( !kReadOnly ) ) {
    this->CloseFile();
  }

  for( Int_t iSec = 0; iSec < N_SEC; iSec++ ) {
    if( gDelay[iSec] != 0 ) 
      delete gDelay[iSec] ;
  }
 
  //  TPhotTiming::kObjectNumber -- ;
}


void TPhotTiming::BookHistos() {
  fOutFile->cd();         // Make sure that we are in the right directory
  char Title[20];
  for ( Int_t iSec = 0; iSec < N_SEC; iSec++ ) {
    gDelay[iSec] = 0;
    sprintf( Title, "dt_vs_pd_%d", iSec+1 );
    h_dt_vs_pd[iSec] = new TH2F( Title, Title, kN_STRP, 0.5, kN_STRP + 0.5, 400, -5.0, +5.0 );
    for ( Int_t iPd = 0; iPd < kN_STRP; iPd++ ) {
      sprintf( Title, "dt_%d_%d", iSec+1, iPd+1 );      
      h_dt[iSec][iPd] = new TH1F( Title, Title, 300, -7., +7. );
    }
  }
  h_com_offset = new TH1F( "Common", "Common", 400, -1.0, +1.0 );
  h_rf       = new TH1F( "Tagger1", "Tagger1" ,  400, -1.5, +1.5 );
  h_rf_vs_pd = new TH2F( "Tagger2", "Tagger2" , N_TC, 0.5, N_TC+0.5, 800, -4, +4 );

  h_tof_tag_tc = new TH2F( "TofTag", "TofTag", N_TC, 0.5, N_TC+0.5, 400, -6, +6 );

  gTagDelay = 0;
  for( Int_t iT = 0; iT < N_TC; iT++ ) {
    sprintf( Title, "tag_rf_%d", iT+1 ); 
    h_tg[iT] =  new TH1F( Title, Title, 200, -1.1, +1.1 );
    sprintf( Title, "tof_tag_%d", iT+1 ); 
    h_tof_tag[iT] = new TH1F( Title, Title, 200, -6, +6 );
  }
  return ;
}


Int_t TPhotTiming::ReadHistos() {
  if ( ( fOutFile == 0 ) || ( ! fOutFile->IsOpen() ) ) {
    cout << "The File for reading histos was not opened " << endl;
    return -1;
  }
  fOutFile->cd();
  char Title[20];
  for ( Int_t iSec = 0; iSec < N_SEC; iSec++ ) {
    gDelay[iSec] = 0;
    sprintf( Title, "dt_vs_pd_%d", iSec+1 );
    h_dt_vs_pd[iSec] = dynamic_cast<TH2F*> ( fOutFile->Get( Title ) );
    if ( h_dt_vs_pd[iSec] == 0 ) {
      cout << "Error: Could not find histogram " << Title << endl;
      return -2;
    }
    for ( Int_t iPd = 0; iPd < kN_STRP; iPd++ ) {
      sprintf( Title, "dt_%d_%d", iSec+1, iPd+1 );      
      h_dt[iSec][iPd] = dynamic_cast<TH1F*> ( fOutFile->Get( Title ) );
      if ( h_dt[iSec][iPd] == 0 ) {
	cout << "Error: Could not find histogram " << Title << endl;
	return -2;
      }      
    }    
  }

  for( Int_t iT = 0; iT < N_TC; iT++ ) {
    sprintf( Title, "tag_rf_%d", iT+1 ); 
    h_tg[iT] = dynamic_cast<TH1F*>( fOutFile->Get( Title ) );
    if( h_tg[iT] == 0 ) {
      cerr << "Error: Could not find histogram " << Title << endl;
      return -7 ;
    }

    sprintf( Title, "tof_tag_%d", iT+1 ); 
    h_tof_tag[iT] =  dynamic_cast<TH1F*>( fOutFile->Get( Title ) );
    if( h_tof_tag[iT] == 0 ) {
      cerr << "Error: Could not find histogram " << Title << endl;
      return -8 ;
    }
  }
  sprintf( Title, "TofTag" ); 
  h_tof_tag_tc = dynamic_cast<TH2F*>( fOutFile->Get( Title ) );
  if( h_tof_tag_tc == 0 ) {
    cout << "Error: Could not find Tagger to Tof Histogram " << Title << endl;
    return -9;      
  }

  h_rf       =  dynamic_cast<TH1F*> ( fOutFile->Get( "Tagger1" ) );
  h_rf_vs_pd =  dynamic_cast<TH2F*> ( fOutFile->Get( "Tagger2" ) );
  if ( h_rf == 0 || h_rf_vs_pd == 0 ) {
    cout << "Error: Could not find Tagger histograms " << endl;
      return -3;    
  }
  h_com_offset = dynamic_cast<TH1F*> ( fOutFile->Get( "Common" ) );
  if ( h_com_offset == 0 ) {
    cout << "Error: Could not find Total Offset Histogram " << endl;
    return -5;
  }
  return 0;
}



Int_t TPhotTiming::CloseFile() {
  if ( ( fOutFile == 0 ) || ( ! fOutFile->IsOpen() )  ) {
    cout << "Output file " << fOutFileName << " has already been closed "  << endl;
    fOutFile = 0;
    return -1;
  }
      
  fOutFile->cd();
  if ( ! kReadOnly ) { 
    for ( Int_t iSec = 0; iSec<N_SEC; iSec++ ) {
      h_dt_vs_pd[iSec]->Write();
      for ( Int_t iPd = 0; iPd < kN_STRP; iPd++ ) {
	h_dt[iSec][iPd]->Write();
      }
    }
    h_com_offset->Write();
  
    for( Int_t iTC = 0; iTC < N_TC; iTC++ ) {
      h_tg[iTC]->Write();
      h_tof_tag[iTC]->Write();
    }
    h_rf_vs_pd->Write();
    h_rf->Write();
    h_tof_tag_tc->Write();
  }

  fOutFile->Close();  
  fOutFile = 0;
  return 0;
}



Int_t TPhotTiming::Process( Long_t N_Events ) {
  if ( kReadOnly ) {
    cout << "This object was created to read histograms" << endl;
    cout << "You need to create a new object to process files " << endl;
    return -1;
  }
  Double_t StartTime;
  const Double_t M_pi = DbPDG.GetParticle( +211 )->Mass();
  //  const Double_t M_p  = DbPDG.GetParticle( 2212 )->Mass();

  fReader.SetDebugMode( 0 );
  cout << "Found " << fReader.GetEntries() << " Entries" << endl;

  Long_t N_Evt = N_Events;
  if ( N_Evt > ( fReader.GetEntries() ) || N_Evt < 0 )
    N_Evt = static_cast<Long_t> ( fReader.GetEntries() );
  cout << "Will Analyze " << N_Evt << " Events " << endl;

  TVector3 v3Tgt( 0, 0, 0 );
  Int_t RunNumOld = kRunNum;
  Bool_t newFile = 1;
  for ( Int_t iEvt = 0; iEvt < N_Evt; iEvt++ ) {
    if ( iEvt % 100000 == 0 && iEvt > 0 ) 
      cout << "Analyzing Event " << iEvt << "  ... " << endl ;    
    
    fReader.GetEntry( iEvt );
    
    THEADERClass* header = fReader.GetHEADER();
    if ( kRunNum == -1 ) {
      kRunNum = header->GetRunNum();
      RunNumOld = kRunNum;
      kSC_VERSION_FLAG = (kRunNum < 55357) ? 1 : 2;
      kN_STRP = (kSC_VERSION_FLAG == 1) ? 48 : 57;
      newFile = 1;
    } 
    else if ( header->GetRunNum() != RunNumOld ) {
      cout << "Warning : Analyzing run " << header->GetRunNum() << " instead of " << kRunNum << endl;
      RunNumOld = header->GetRunNum();
      newFile = 1;
    } 


    if( newFile ) {
      const Char_t* tSystem = "GEOMETRY";
      const Char_t* tSubSystem = "target";
      const Char_t* tItem = "position" ;
      TArrayF  ValueArray ;
      cout << "Requesting Target Position for run " << kRunNum << endl;
      fCalDB.Get_Map_Float( tSystem, tSubSystem, tItem, kRunNum,  &ValueArray );
      v3Tgt.SetXYZ( ValueArray[0], ValueArray[1], ValueArray[2] );
      cout << "Target position is :  " ;
      v3Tgt.Print();
      newFile = 0;
    }

    Double_t rfTime = header->GetSTT();
    Double_t tagTime = rfTime;
    Int_t tId = -10;
    Int_t ptrTag;
    Int_t N_phot =  fReader.GetNRows( "TGPB" );
    for ( Int_t iPhot=0; iPhot < N_phot; iPhot++ ) {
      TTGPBClass* tgpb = dynamic_cast<TTGPBClass*> ( fReader.GetBankRow( "TGPB", iPhot ) );
      if ( tgpb != 0  && (ptrTag = -(tgpb->GetPointer())/1000-1) >= 0 ) {
 	TTAGRClass* tagr = dynamic_cast<TTAGRClass*> ( fReader.GetBankRow( "TAGR", ptrTag ) );
	if( tagr != 0 ) {
	  rfTime   = tagr->GetTagRF();
	  tagTime  = tagr->GetTagTime();
	  Double_t id = tagr->GetTid();
	  tId = tagr->GetTid();
	  Double_t dt = tagTime - rfTime;
	  h_tg[tId-1]->Fill( dt );
	  h_rf_vs_pd->Fill( id, dt );
	  h_rf->Fill( dt );
	}
      } 
    }    
    
    StartTime = header->GetSTT();
    Int_t Nrec = fReader.GetNPart(); 
    if ( Nrec >= MinTrkNum ) {
      for ( Int_t iRow = 0; iRow < Nrec; iRow++ ) {
	TEVNTClass* evnt = dynamic_cast<TEVNTClass*>( fReader.GetBankRow( "EVNT", iRow ) );
	if ( evnt != 0 && evnt->GetStat() > 0 && evnt->GetDCStat() > 0  && evnt->GetSCStat() > 0  ) {
	  TDCPBClass* dcpb = dynamic_cast<TDCPBClass*>( fReader.GetBankRow( "DCPB", evnt->GetDCidx() ) );
	  TSCPBClass* scpb = dynamic_cast<TSCPBClass*>( fReader.GetBankRow( "SCPB", evnt->GetSCidx() ) );
	  
	  if( dcpb != 0 && scpb != 0 ) {
	    Double_t P     = evnt->GetMomentum();
	    Double_t E_dep = scpb->GetEdep();
	    TVector3 v3Vtx = evnt->GetVertex();

	    Bool_t PionIdCheck = 1;
	    if( PionIdMethod.Contains( "time" ) ) 
	      PionIdCheck &= ( TMath::Abs( evnt->GetId() ) == 211 ) ;

	    if( PionIdMethod.Contains( "energy" ) ) 
	      PionIdCheck &= ( TPhotTiming::PionDeDxCut( P, E_dep ) ) ;

	    if ( dcpb->GetStatus() > 0 && PionIdCheck ) {
	      Double_t Time_sc = scpb->GetTime() ;
	      Double_t Path =  scpb->GetPath();
	      
	      Int_t    iSec =  scpb->GetSector();
	      Double_t pd   = static_cast<Double_t> ( scpb->GetPaddle() ) ; 
	      Int_t iPd = scpb->GetPaddle() ;
	      
	      Double_t Beta = P / sqrt( P*P + M_pi*M_pi ) ;
	      //	      Double_t Beta = P / sqrt( P*P + M_p*M_p ) ;
	      
	      if( Beta > 1.0e-10 ) {
		Double_t vtxTime = Time_sc - Path/( Beta*cLight ) ; 
		if( tId > 0 ) {
		  StartTime = rfTime + ( ( v3Vtx.Z() - v3Tgt.Z() ) / cLight ) ;
		}

		Double_t dt = vtxTime - StartTime;
		
		if( iSec <= 6 && iPd <= kN_STRP ) {
		  h_dt_vs_pd[iSec-1]->Fill( pd, dt );
		  h_dt[iSec-1][iPd-1]->Fill( dt );
		  h_com_offset->Fill( dt );
		  if( tId > 0 ) {
		    Double_t dTofTag =  tagTime + ( ( v3Vtx.Z() - v3Tgt.Z() ) / cLight ) - vtxTime ;
		    h_tof_tag_tc->Fill( static_cast<Double_t>( tId ), dTofTag );
		    h_tof_tag[tId-1]->Fill( dTofTag );
		  }
		}
	      }
	    }
	  }
	}
      }
    }   
  }  
  return N_Evt;
}



Int_t TPhotTiming::Fit( Int_t Sec, Int_t Pd ) {
  if( kRunNum < 0 ) {
    cerr << "Erro:   Bad run number " << kRunNum << endl;
    cerr << "Error : Cant fit without knowing run number " << endl;
    return -1;
  }
  Int_t SecMin, SecMax, PdMin, PdMax;
  if ( Sec == 0 ) {
    SecMin = 1; SecMax = N_SEC;
  } else {
    SecMin = SecMax = Sec;
  }  
  if ( Pd == 0 ) {
    PdMin = 1; PdMax = kN_STRP;
  } else {
    PdMin = PdMax = Pd;
  }
  
  Double_t xArray[N_STRP_MAX];
  for ( Int_t ix=0; ix<kN_STRP; ix++ ) 
    xArray[ix] = ix+1;
  TF1 gaus_p0( "gaus_p0", "[0]*exp(-((x-[1])*(x-[1])/(2.*[2]*[2])))+[3]", -2., +2. );

  for ( Int_t iSec = SecMin; iSec <= SecMax; iSec++ ) {
    for ( Int_t iPd = PdMin; iPd <= PdMax; iPd++ ) {
      TH1F* hist = h_dt[iSec-1][iPd-1];

      Double_t Mean  = hist->GetBinCenter( hist->GetMaximumBin() ) ;
      Double_t Sigma = hist->GetRMS();
      Double_t Ampl  = 0.8 * hist->GetMaximum();
      Double_t Bkg  = 0.1 * hist->GetMaximum();
      Sigma = TMath::Min( Sigma, 0.3 );

      Double_t LoEdge = Mean - 3*Sigma;
      Double_t UpEdge = Mean + 3*Sigma;
      gaus_p0.SetParameter( 0, Ampl );
      gaus_p0.SetParameter( 1, Mean );
      gaus_p0.SetParameter( 2, Sigma );
      gaus_p0.SetParameter( 3, Bkg );

      hist->Fit( "gaus_p0", "Q0+", "", LoEdge, UpEdge );
      TF1* FitFun = hist->GetFunction( "gaus_p0" );
      FitFun->ResetBit( (1<<9) );

      Mean      = FitFun->GetParameter( 1 );
      Sigma     = fabs( FitFun->GetParameter( 2 ) );
      Double_t ErrMean   = FitFun->GetParError( 1 );
      Double_t ErrSigma  = FitFun->GetParError( 2 );
      if ( Sigma > 0.7 || ErrMean > 0.2 || hist->GetEntries() < 100 ) { 
	fDelay[0][iSec-1][iPd-1] = 0.0; 
	fWidth[0][iSec-1][iPd-1] = Sigma;
	fDelay[1][iSec-1][iPd-1] = 4.0;
	fWidth[1][iSec-1][iPd-1] = ErrSigma; 
	kBadChan[iSec-1][iPd-1] = 1;
      }
      else {
	fDelay[0][iSec-1][iPd-1] = Mean;
	fWidth[0][iSec-1][iPd-1] = Sigma;
	fDelay[1][iSec-1][iPd-1] = ErrMean;
	fWidth[1][iSec-1][iPd-1] = ErrSigma;
      }
    }
    gDelay[iSec-1] = new TGraphErrors(kN_STRP, xArray, (Double_t*)fDelay[0][iSec-1], 
				       0, (Double_t*)fDelay[1][iSec-1] ); 
  }

  {
  // Now fit the TOF offset 
    TF1 gaus_p1( "gaus_p1", "[0]*exp(-((x-[1])*(x-[1])/(2.*[2]*[2])))+[3]+[4]*x", -2., +2. );
    TH1F* hist = h_com_offset;
    
    Double_t Mean  = hist->GetBinCenter( hist->GetMaximumBin() ) ;
    Double_t Sigma = hist->GetRMS();
    Double_t Ampl  = 0.8 * hist->GetMaximum();
    Double_t Bkg  = 0.1 * hist->GetMaximum();
    Sigma = TMath::Min( Sigma, 0.2 );
    
    Double_t LoEdge = Mean - 2*Sigma;
    Double_t UpEdge = Mean + 2*Sigma;
    gaus_p1.SetParameter( 0, Ampl );
    gaus_p1.SetParameter( 1, Mean );
    gaus_p1.SetParameter( 2, Sigma );
    gaus_p1.SetParameter( 3, Bkg );
    gaus_p1.SetParameter( 4, 0.0 );

    
    hist->Fit( "gaus_p1", "Q0+", "", LoEdge, UpEdge );
    TF1* FitFun = hist->GetFunction( "gaus_p1" );
    FitFun->ResetBit( (1<<9) );
    
    Mean      = FitFun->GetParameter( 1 );
    Sigma     = fabs( FitFun->GetParameter( 2 ) );
//     Double_t ErrMean   = FitFun->GetParError( 1 );
//     Double_t ErrSigma  = FitFun->GetParError( 2 );
    fTOFOffset = Mean;
    fTOFWidth  = Sigma;
  }

  // Now fit Tagger RF histo as well   
  //  First Check for RF status 
  const Char_t* tSystem = "RF_OFFSETS";
  const Char_t* tSubSystem = "status";
  const Char_t* tItem = "value" ;
  TArrayI  ValueArray ;
  cout << "Requesting RF status for run " << kRunNum << endl;
  fCalDB.Get_Map_Int( tSystem, tSubSystem, tItem, kRunNum,  &ValueArray );
  Int_t rfStat = ValueArray[0];
  if ( rfStat == 0 ) cout << "Warning: RF dead for run " << kRunNum << endl; 

  if ( rfStat ) {
    FitTagger();
  }
  
  kFitDone = 1;
  return 0;
}



Int_t TPhotTiming::FitTagger() {
  
  TF1 gaus_p0( "gaus_p0", "[0]*exp(-((x-[1])*(x-[1])/(2.*[2]*[2])))+[3]", -2., +2. );
  {
    TH1F* hist = h_rf;
    Double_t Mean  = hist->GetBinCenter( hist->GetMaximumBin() ) ;
    Double_t Sigma = hist->GetRMS();
    Double_t Ampl  = 0.8 * hist->GetMaximum() ;
    Double_t Bkg   = 0.1 * hist->GetMaximum() ;
    Sigma = TMath::Min( Sigma, 0.2 );
    
    Double_t LoEdge = Mean - 2*Sigma;
    Double_t UpEdge = Mean + 2*Sigma;
    gaus_p0.SetParameter( 0, Ampl );
    gaus_p0.SetParameter( 1, Mean );
    gaus_p0.SetParameter( 2, Sigma );
    gaus_p0.SetParameter( 3, Bkg );
    
    hist->Fit( "gaus_p0", "Q0+", "", LoEdge, UpEdge );
    TF1* FitFun = hist->GetFunction( "gaus_p0" );
    FitFun->ResetBit( (1<<9) );
    
    Mean      = FitFun->GetParameter( 1 );
    Sigma     = fabs( FitFun->GetParameter( 2 ) );
//     Double_t ErrMean   = FitFun->GetParError( 1 );
//     Double_t ErrSigma  = FitFun->GetParError( 2 );
    fRfOffset = Mean;
    fRfWidth  = Sigma;
  }
  {
    Double_t xArray[N_TC];
    for ( Int_t ix = 0; ix < N_TC; ix++ ) 
      xArray[ix] = ix+1;

    for ( Int_t iPd = 1; iPd <= N_TC; iPd++ ) {
      {
	TH1F* hist = h_tof_tag[iPd-1];
	
	Double_t Mean  = hist->GetBinCenter( hist->GetMaximumBin() ) ;
	Double_t Sigma = hist->GetRMS();
	Double_t Ampl  = 0.8 * hist->GetMaximum() ;
	Double_t Bkg   = 0.1 * hist->GetMaximum() ;
	Sigma = TMath::Min( Sigma, 0.4 );
	
	Double_t LoEdge = Mean - 3*Sigma;
	Double_t UpEdge = Mean + 3*Sigma;
	gaus_p0.SetParameter( 0, Ampl );
	gaus_p0.SetParameter( 1, Mean );
	gaus_p0.SetParameter( 2, Sigma );
	gaus_p0.SetParameter( 3, Bkg );
	
	hist->Fit( "gaus_p0", "Q0+", "", LoEdge, UpEdge );
	TF1* FitFun = hist->GetFunction( "gaus_p0" );
	FitFun->ResetBit( (1<<9) );
	
	Mean      = FitFun->GetParameter( 1 );
	Sigma     = fabs( FitFun->GetParameter( 2 ) );
	Double_t ErrMean   = FitFun->GetParError( 1 );
	//	Double_t ErrSigma  = FitFun->GetParError( 2 ) ;
	if ( Sigma < 1.5 && ErrMean < 0.5 && hist->GetEntries() > 150 ) { 
	  fTagDelay[0][iPd-1] = ( RfConst * 
				  static_cast<Int_t>
				  ( ( ConstShift + Mean + (RfConst/2.) ) / RfConst ) ) - ConstShift; 
	} else {
	  fTagDelay[0][iPd-1] = 0;
	}
      }
      {
	TH1F* hist = h_tg[iPd-1];
	
	Double_t Mean  = hist->GetBinCenter( hist->GetMaximumBin() ) ;
	Double_t Sigma = hist->GetRMS();
	Double_t Ampl  = 0.8 * hist->GetMaximum() ;
	Double_t Bkg   = 0.1 * hist->GetMaximum() ;
	Sigma = TMath::Min( Sigma, 0.2 );
	
	Double_t LoEdge = Mean - 3*Sigma;
	Double_t UpEdge = Mean + 3*Sigma;
	gaus_p0.SetParameter( 0, Ampl );
	gaus_p0.SetParameter( 1, Mean );
	gaus_p0.SetParameter( 2, Sigma );
	gaus_p0.SetParameter( 3, Bkg );
	
	hist->Fit( "gaus_p0", "Q0+", "", LoEdge, UpEdge );
	TF1* FitFun = hist->GetFunction( "gaus_p0" );
	FitFun->ResetBit( (1<<9) );
	
	Mean      = FitFun->GetParameter( 1 );
	Sigma     = fabs( FitFun->GetParameter( 2 ) );
	Double_t ErrMean   = FitFun->GetParError( 1 );
	Double_t ErrSigma  = FitFun->GetParError( 2 ) ;
	if ( Sigma > 0.6 || ErrMean > 0.2 || hist->GetEntries() < 100 ) { 
	  fTagDelay[0][iPd-1] = 0.0; 
	  fTagWidth[0][iPd-1] = Sigma;
	  fTagDelay[1][iPd-1] = 4.0;
	  fTagWidth[1][iPd-1] = ErrSigma; 
	  kTagBadChan[iPd-1] = 1;
	} else {
	  fTagDelay[0][iPd-1] += Mean;
	  fTagWidth[0][iPd-1]  = Sigma;
	  fTagDelay[1][iPd-1]  = ErrMean;
	  fTagWidth[1][iPd-1]  = ErrSigma;
	}
      }
    }
    gTagDelay = new TGraphErrors( N_TC, xArray, (Double_t*)fTagDelay[0], 
				  0, (Double_t*)fTagDelay[1] ); 
  }
  return 0;
}





void TPhotTiming::Plot1D( Int_t Sec ) { 
  Int_t SecMin, SecMax;
  if ( Sec == 0 ) {
    SecMin = 1;
    SecMax = N_SEC;
  } else {
    SecMin = SecMax = Sec;
  }
  gStyle->SetOptStat(0);
  gStyle->SetOptTitle(1);  
  //  const Int_t nSec = SecMax - SecMin + 1 ;
  TCanvas* canv[N_SEC];
  Char_t Title[64];
  for ( Int_t iSec = SecMin; iSec <= SecMax; iSec++ ) {
    sprintf( Title, "Obj_%d_Sec_%d", kObjNum, iSec );
    canv[iSec-1] = new TCanvas( Title, Title, 10, 70, 600, 800 );
    canv[iSec-1]->SetFillColor( 10 );
    canv[iSec-1]->Divide( 1, 1 );
    canv[iSec-1]->cd(1);
    TPad* MainPad = dynamic_cast<TPad*>( gPad );
    MainPad->Divide( 6, 10, 0.001, 0.001 );
    for ( Int_t iPd = 0; iPd < kN_STRP; iPd++ ) {
      MainPad->cd( iPd+1 );
      h_dt[iSec-1][iPd]->Draw("");
      gPad->Update();
    }
    MainPad->cd();
  }
  return;
}


void TPhotTiming::Plot2D( Double_t minY, Double_t maxY ) {
  Char_t Title[64];
  sprintf( Title, "Obj_%d", kObjNum );
  TCanvas* canv =  new TCanvas( Title, Title, 40, 100, 800, 700 );
  canv->SetFillColor( 10 );
  canv->Divide( 1, 1 );
  canv->cd(1);
  TPad* MainPad = dynamic_cast<TPad*>( gPad );
  MainPad->Divide( 2, 3 );
  gStyle->SetOptStat(0);
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);
  for ( Int_t iSec = 0; iSec < N_SEC; iSec++ ) {
    MainPad->cd( iSec+1 );
    gPad->SetLogz();
    if( minY < h_dt_vs_pd[iSec]->GetYaxis()->GetXmin() ) 
      minY = h_dt_vs_pd[iSec]->GetYaxis()->GetXmin();
    if( minY > h_dt_vs_pd[iSec]->GetYaxis()->GetXmax() ) 
      maxY = h_dt_vs_pd[iSec]->GetYaxis()->GetXmax();
   
    h_dt_vs_pd[iSec]->GetYaxis()->SetRangeUser( minY, maxY );
    h_dt_vs_pd[iSec]->Draw("colz");
    if ( kFitDone && gDelay[iSec] != 0 ) {
      gDelay[iSec]->SetMarkerStyle( 6 );
      gDelay[iSec]->SetMarkerSize(0.1);
      gDelay[iSec]->SetMarkerColor(1);
      gDelay[iSec]->SetLineWidth(4);
      gDelay[iSec]->Draw("P");
    }
    gPad->Update();
  }  
  MainPad->cd();
  return;
}


void TPhotTiming::PlotTagger() {
  PlotTagger1D();
  PlotTagger2D();
  return;
}



void TPhotTiming::PlotTagger1D() {
  gStyle->SetOptStat(0);
  gStyle->SetOptTitle(1);  
  Char_t Title[64];
  {
    sprintf( Title, "Obj_%d_tc", kObjNum  );
    TCanvas* canv = new TCanvas( Title, Title, 10, 70, 1000, 1000 );
    canv->SetFillColor( 10 );
    canv->Divide( 1, 1 );
    canv->cd(1);
    TPad* MainPad = dynamic_cast<TPad*>( gPad );
    
    Int_t nPadX = static_cast<Int_t> ( TMath::Sqrt( static_cast<Double_t> ( N_TC ) ) );
    Int_t nPadY = static_cast<Int_t> ( static_cast<Double_t>(N_TC) / static_cast<Double_t>(nPadX) ) 
      + ( N_TC % nPadX ); 
    MainPad->Divide( nPadX, nPadY, 0.001, 0.001 );
    for ( Int_t iPd = 0; iPd < N_TC; iPd++ ) {
      MainPad->cd( iPd+1 );
      h_tg[iPd]->Draw("");
      gPad->Update();
    }
    MainPad->cd();
    MainPad->Update();
  }
  {
    sprintf( Title, "Obj_%d_tag_tof", kObjNum  );
    TCanvas* canv = new TCanvas( Title, Title, 40, 70, 1000, 1000 );
    canv->SetFillColor( 10 );
    canv->Divide( 1, 1 );
    canv->cd(1);
    TPad* MainPad = dynamic_cast<TPad*>( gPad );
    
    Int_t nPadX = static_cast<Int_t> ( TMath::Sqrt( static_cast<Double_t> ( N_TC ) ) );
    Int_t nPadY = static_cast<Int_t> ( static_cast<Double_t>(N_TC) / static_cast<Double_t>(nPadX) ) 
      + ( N_TC % nPadX ); 
    MainPad->Divide( nPadX, nPadY, 0.001, 0.001 );
    for ( Int_t iPd = 0; iPd < N_TC; iPd++ ) {
      MainPad->cd( iPd+1 );
      h_tof_tag[iPd]->Draw("");
      gPad->Update();
    }
    MainPad->cd();
    MainPad->Update();
  }  
  return;
}


void TPhotTiming::PlotTagger2D( Double_t minY, Double_t maxY ) {
  Char_t Title[64];
  sprintf( Title, "Obj_%d_tagger", kObjNum );
  TCanvas* canv =  new TCanvas( Title, Title, 40, 100, 800, 500 );
  canv->SetFillColor( 10 );
  canv->Divide( 1, 1 );
  canv->cd(1);
  TPad* MainPad = dynamic_cast<TPad*>( gPad );
  MainPad->Divide( 2, 1 );

  gStyle->SetOptStat(0);
  gStyle->SetOptTitle(0);
  gStyle->SetOptFit(1);
  gStyle->SetPalette(1);  
  MainPad->cd(1);
  gPad->SetLogz();
  if( minY < h_rf_vs_pd->GetYaxis()->GetXmin() ) 
    minY = h_rf_vs_pd->GetYaxis()->GetXmin();
  if( minY > h_rf_vs_pd->GetYaxis()->GetXmax() ) 
    maxY = h_rf_vs_pd->GetYaxis()->GetXmax();
  
  h_rf_vs_pd->Draw("colz");
  if ( kFitDone ) {
    if( gTagDelay != 0 ) {
      gTagDelay->SetMarkerStyle( 6 );
      gTagDelay->SetMarkerSize(0.08);
      gTagDelay->SetMarkerColor(1);
      gTagDelay->SetLineWidth(3);
      gTagDelay->Draw("P");
    }
//     Double_t Xmin = h_rf_vs_pd->GetXaxis()->GetXmin();
//     Double_t Xmax = h_rf_vs_pd->GetXaxis()->GetXmax();
//     TLine* line = new TLine( Xmin, fRfOffset, Xmax , fRfOffset );
//     line->SetLineWidth(3);
//     line->Draw("same");
  }
  gPad->Update();

  MainPad->cd(2);  
  //  h_rf->Draw();
  gPad->SetLogz();
  if( minY < h_tof_tag_tc->GetYaxis()->GetXmin() ) 
    minY = h_tof_tag_tc->GetYaxis()->GetXmin();
  if( minY > h_tof_tag_tc->GetYaxis()->GetXmax() ) 
    maxY = h_tof_tag_tc->GetYaxis()->GetXmax();
  h_tof_tag_tc->Draw( "colz" );
  gPad->Update();
  
  MainPad->cd();
  return;
}


void TPhotTiming::InitConstants() {
  fRfOffset = 0; 
  fRfWidth = 4;
  fTOFOffset = 0;
  fTOFWidth = 4;
  for ( Int_t iSec = 0; iSec < N_SEC; iSec++ ) 
    for ( Int_t iPd = 0; iPd < N_STRP_MAX; iPd++ ) 
      for ( Int_t ix = 0; ix < 2 ; ix++ ) {
	fDelay[ix][iSec][iPd] = 0.0;
	fWidth[ix][iSec][iPd] = 4.0;
	fMapDelay[iSec][iPd] = 0.0;
	fDelay4Map[iSec][iPd] = 0.0;
	kBadChan[iSec][iPd] = 0;
      }
}


void TPhotTiming::InitCalDB() {
  fCalDB.SetDebugMode( 0 );
  fCalDB.SetDBParams( DBHost, DataBase, UserName );
  fCalDB.SetIndexTable( fRunIndex.Data() );
}


Int_t TPhotTiming::WriteConstants( const Char_t* tofFileName, const Char_t* tagFileName ) {
  if( kRunNum < 0 ) {
    cerr << "Bad run number " << kRunNum << endl;
    return -1;
  }
  Int_t statTof  = WriteTOFConstants( tofFileName );
  Int_t statTag  = WriteTagConstants( tagFileName );
  return ( statTof | statTag );
}



Int_t TPhotTiming::WriteTOFConstants( const Char_t* FileName ) {
  if( kRunNum < 0 ) {
    cerr << "Bad run number " << kRunNum << endl;
    return -1;
  }
  if ( this->GetNewMapConstants( "tof" ) != 0 ) {
    cout << "Error calculating the new constnts " << endl;
    return -4;
  }  
  fTxtFileName = FileName;
  FILE* outfile = fopen( fTxtFileName.Data(), "w" ) ;
  if ( outfile != 0 ) {
    fprintf( outfile, 
	    "Sector    Strip            Delay           ErrDelay            Sigma            ErrSigma       Delay4Map\n" );
    for ( Int_t iSec = 0; iSec < N_SEC; iSec++ ) {
      for ( Int_t iPd = 0; iPd < kN_STRP; iPd++ ) {
	fprintf( outfile, "%2d        %2d       %12.4f       %12.4f     %12.4f     %12.4f     %12.4f \n", 
		 iSec+1,     iPd+1,  fDelay[0][iSec][iPd],   fDelay[1][iSec][iPd],
		 fWidth[0][iSec][iPd], fWidth[1][iSec][iPd], fDelay4Map[iSec][iPd] );		
      }
    }
    //    fprintf( outfile, "%12.4f       %12.4f \n", fRfOffset, fRfWidth );
    fclose(outfile);
  } else {
    cout << "Could not Open File " << fTxtFileName << endl;
    return -3;
  }
  return 0;
}


Int_t TPhotTiming::WriteTagConstants( const Char_t* FileName ) {
  if( kRunNum < 0 ) {
    cerr << "Bad run number " << kRunNum << endl;
    return -1;
  }
  if ( this->GetNewMapConstants( "tag" ) != 0 ) {
    cout << "Error calculating the new constnts " << endl;
    return -4;
  }  
  fTxtFileName = FileName;
  FILE* outfile = fopen( fTxtFileName.Data(), "w" ) ;
  if ( outfile != 0 ) {

    fprintf( outfile, 
	     "Counter         Delay              ErrDelay         Sigma           ErrSigma       Delay4Map\n" );
    for ( Int_t iPd = 0; iPd < N_TC; iPd++ ) {
      fprintf( outfile, "%2d        %12.4f       %12.4f     %12.4f     %12.4f     %12.4f \n", 
	       iPd+1,  fTagDelay[0][iPd],   fTagDelay[1][iPd],
	       fTagWidth[0][iPd], fTagWidth[1][iPd], fTagDelay4Map[iPd] );		
    }
    fclose(outfile);
  } else {
    cout << "Could not Open File " << fTxtFileName << endl;
    return -3;
  }
  return 0;
}


Int_t  TPhotTiming::GetNewMapConstants( const Char_t* opt ) {
  TString sOpt = opt;
  sOpt.ToLower();
  if ( this->GetMapValues( sOpt.Data() ) != 0 ) {
    cout << "Could not read databse  values for run " << kRunNum << 
      " from RunIndex table" << fRunIndex.Data()  << endl;
    return -1;
  }
 
  if( sOpt.Contains( "tof" ) ) {
    for ( Int_t iSec = 0; iSec < N_SEC; iSec++ ) {
      for ( Int_t iPd = 0; iPd < kN_STRP; iPd++ ) {
	//      fDelay4Map[iSec][iPd] = fMapDelay[iSec][iPd] - ( fDelay[0][iSec][iPd] - fRfOffset );
	fDelay4Map[iSec][iPd] = fMapDelay[iSec][iPd] - fDelay[0][iSec][iPd] ;
      }
    }
  }

  if( sOpt.Contains( "tag" ) ) {
    for ( Int_t iTC = 0; iTC < N_TC; iTC++ ) {
      fTagDelay4Map[iTC] = fTagMapDelay[iTC] + fTagDelay[0][iTC] ;
    }
  }
  return 0;
}



Int_t TPhotTiming::GetMapValues( const Char_t* opt ) {
  TString sOpt = opt;
  sOpt.ToLower();
  if( kRunNum < 0 ) {
    cerr << "Bad run number " << kRunNum << endl;
    return -1;
  }

  if( sOpt.Contains( "tof" ) ) {
    const Char_t* tSystem = (kSC_VERSION_FLAG == 1) ? "SC_CALIBRATIONS" : "SC_CALIBRATIONS_V2";
    const Char_t* tSubSystem = "delta_T";
    const Char_t* tItem = "paddle2paddle" ;
    TArrayF  ValueArray ;
    cout << "Requesting TOF Constants for run " << kRunNum << endl;
    fCalDB.Get_Map_Float( tSystem, tSubSystem, tItem, kRunNum,  &ValueArray );
    Float_t* Array1D = ValueArray.GetArray();
    for ( Int_t iSec = 0; iSec < N_SEC; iSec++ ) {
      for ( Int_t iPd = 0; iPd < kN_STRP; iPd++ ) {
	fMapDelay[iSec][iPd] = static_cast<Double_t> ( *(Array1D + iSec*kN_STRP + iPd) );
	//	cout << iSec+1 << " " << iPd+1 << " " << fMapDelay[iSec][iPd] << endl;
      }
    }
  }
  
  if( sOpt.Contains( "tag" ) ) {
    const Char_t* tSystem = "TAG_CALIB";
    const Char_t* tSubSystem = "tag_t";
    const Char_t* tItem = "ci" ;
    TArrayF  ValueArray ;
    cout << "Requesting TAG Constants for run " << kRunNum << endl;
      fCalDB.Get_Map_Float( tSystem, tSubSystem, tItem, kRunNum,  &ValueArray );
      //      Float_t* Array1D = ValueArray.GetArray();      
      for( Int_t iTC = 0; iTC < N_TC; iTC++ ) {
	fTagMapDelay[iTC] = ValueArray[iTC] ;
      }
  }
  return 0;
}


Int_t TPhotTiming::PionDeDxCut( Double_t P,  Double_t E )
{
  if ( ( 6.0 < E ) && ( E < 17.-6. * P ) )
    return 1;
  else
    return 0; 
}
