#include <iostream>
#include <fstream>
#include <signal.h>
#include <TROOT.h>
#include <TFile.h>
#include <TNtuple.h>
#include <clasEvent.h>

#include "TNtupleUtil.h"

int StartRun(int runNo)
{  
  int static CurrentRun = -1;
  if (CurrentRun != runNo) {
    vertex_brun(runNo);
    CurrentRun = runNo;
  }
  return 0;
}

void PrintUsage(char *processName)
{
  cerr << processName << " <options> <filename>\n";
  cerr << "\toptions are:\n";
  cerr << "\t-o<name>.root\tRoot output file.\n";
  cerr << "\t-h\tprint the above" << endl;
}


static int realexit = 0;
void ctrlCHandle(int x)
{
  signal(SIGINT, ctrlCHandle);
  signal(SIGHUP, ctrlCHandle);
  if(realexit > 0) {
    cerr << endl << endl << "\t\t\t***  INTERRUPTED!!!  Exiting... ***"
	 << endl << endl;
    exit(1);
  }
  realexit++;
  cerr << endl << endl << "\t\t\t***  INTERRUPTED!!! Ending Event processing ***"
	 << endl << endl;
}


TROOT ClasNtuple("ClasNtuple","A CLAS Ntuple");

int main(int argc,char **argv){
  
  signal(SIGINT, ctrlCHandle);
  signal(SIGHUP, ctrlCHandle);
  long EventCount=0;
  int partbank0 = 1;
  
  int ret = 1;
  long Nevents = 0;
  long nSelectedEvents =0;
  char *argptr;
  
  // bos stuff
  int MaxBanks = 1000; 
  
 
  Char_t outputfile[50], ntpTitle[50]; 
  sprintf(outputfile,"ClasNtuple.root");
  sprintf(ntpTitle,"Hall  Ntuple");
 
  for (int i = 1; i < argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {
      case 'o':
	sprintf(outputfile,"%s.root",++argptr);
	cerr<<"Saveing output in file: "<<outputfile<<endl;
	break;
      case 'h':
	PrintUsage(argv[0]);
	exit(0);
	break;
      default:
	cerr << "Unrecognized argument: " << argptr << endl;;
	break;
      }
    }
  }///////////////////////////////////////////////////////////


  //
  //  Open the output file.
  //
  Int_t comp   = 1; // compress levels from 0 to 9 (none to most)
  TFile *ntpfile = new TFile(outputfile,"RECREATE","A CLAS ntuple");
  ntpfile->SetCompressionLevel(comp);

  Bool_t firstEvent=kTRUE;

  //
  // setup a TNtuple
  //
  TNtuple *ntp=0;
  TntpLables *vnames= new TntpLables();
  Int_t n_vectors;
  Float_t values[200];
  Char_t label[50];

  //
  // Initialize BOS
  bnames_(&MaxBanks);
  initbos();
  configure_banks(stderr,0);

  for (int i = 1; i < argc; ++i) {
    argptr = argv[i];
    // process all arguments on command line.
    if (*argptr != '-' && realexit==0 ) {
      // we have a file to process
      EventCount = 0;
      clasEvent event(argptr,&bcs_,partbank0,0);
      
      if (event.status() ) 
	ret = 1;
      
      while ( ret && realexit==0 ) {
	// process every event in the file
	ret = event.read(partbank0);
	//cerr<<"Event status: "<<event.status()<<endl;
	if(DISIO_DATA == ret && event.status() ){
	  Nevents++;
	  int runno = event.run();	
	  ConfigEvent(runno,0); 
	  StartRun(runno);
	  
	  //
	  // ProcessOneEvent
	  n_vectors=0;
	  int nReturnValue; nReturnValue = 0;
	  if(event.load(partbank0)==0) cerr<<"Error: event.load(partbank0)\n";
	  
	  //  clasTAGR_t *tagR = 0;
	  //if(tagR = event.TAGR()) {
	  // }
	  
	  
	  //
	  // Book selected event channel
	  // 
	  if((event.N(PiMinus)==1 && event.N(-1)==1  || event.N(PiMinus)==2 
	      && event.N(-1)==2 ) 
	     && event.N(PiPlus)==2 && event.N(Proton)==1
	     && event.N(1)==3){

	     nSelectedEvents++;

	    fourVec pim, pip1, pip2, proton, beam, miss4v;
	    fourVec target( PROTON_MASS, threeVec(0.0,0.0,0.0) );
	    threeVec vert = event.V();
	    
	    
	    //  Fill 4-vector of each particle.
	    //
	    pim = event.cp(PiMinus,1).p();
	    pip1 = event.cp(PiPlus,1).p();
	    pip2 = event.cp(PiPlus,2).p();
	    proton = event.cp(Proton,1).p();
	    beam = event.getBeam(4.40, 5.7);
	    miss4v = beam + target -proton -pim -pip1 -pip2;
	    
	    //
	    //  Book event info into ntuple
	    //
	    values[n_vectors++] = beam.t(); 
	    if(firstEvent){
	      sprintf(label,"beamE");
	      vnames->Add(label);
	      cout<<"beamE:\t\t  Photon beam energy\n";
	    }
	    values[n_vectors++] = event.vtime()- event.stVtime() ;
	    if(firstEvent){
	      sprintf(label,"stVtimeDiff");
	      vnames->Add(label);
	      cout<<"stVtimeDiff:\t\t  vtime - stVtime x\n";
	    }
	    values[n_vectors++] = vert.x();
	    if(firstEvent){
	      sprintf(label,"x");
	      vnames->Add(label);
	      cout<<"x:\t\t  Vertex x\n";
	    }
	    values[n_vectors++] = vert.y();
	    if(firstEvent){
	      sprintf(label,"y");
	      vnames->Add(label);
	      cout<<"y:\t\t  Vertex y\n";
	    }
	    values[n_vectors++] = vert.z();
	    if(firstEvent){
	      sprintf(label,"z");
	      vnames->Add(label);
	      cout<<"z:\t\t  Vertex z\n";
	    }
	    
	    //
	    // Book particle info
	    //
	    values[n_vectors++] = pim.z();
	    if(firstEvent){
	      sprintf(label,"pim_pz");
	      vnames->Add(label);
	      cout<<"pim_pz:\t\t PiMinus z momentum \n";
	    }
	    values[n_vectors++] = sqrt( pim.x()*pim.x() + pim.y()*pim.y() );
	    if(firstEvent){
	      sprintf(label,"pim_pt");
	      vnames->Add(label);
	      cout<<"pim_pt:\t\t PiMinus transverse momentum \n";
	    }
	    values[n_vectors++] = pim.V().theta();
	    if(firstEvent){
	      sprintf(label,"pim_theta");
	      vnames->Add(label);
	      cout<<"pim_theta:\t\t PiMinus lab theta \n";
	    }
	    values[n_vectors++] = pim.V().phi();
	    if(firstEvent){
	      sprintf(label,"pim_phi");
	      vnames->Add(label);
	      cout<<"pim_phi:\t\t PiMinus lab phi \n";
	    }
	    values[n_vectors++] = pip1.z();
	    if(firstEvent){
	      sprintf(label,"pip1_pz");
	      vnames->Add(label);
	      cout<<"pip1_pz:\t\t PiPlus1 z momentum \n";
	    }
	    values[n_vectors++] = sqrt( pip1.x()*pip1.x() + pip1.y()*pip1.y() );
	    if(firstEvent){
	      sprintf(label,"pip1_pt");
	      vnames->Add(label);
	      cout<<"pip1_pt:\t\t PiPlus1 transverse momentum \n";
	    }
	    values[n_vectors++] = pip1.V().theta();
	    if(firstEvent){
	      sprintf(label,"pip1_theta");
	      vnames->Add(label);
	      cout<<"pip1_theta:\t\t PiPlus1 lab theta \n";
	    }
	    values[n_vectors++] = pip1.V().phi();
	    if(firstEvent){
	      sprintf(label,"pip1_phi");
	      vnames->Add(label);
	      cout<<"pip1_phi:\t\t PiPlus1 lab phi \n";
	    }
	    values[n_vectors++] = pip2.z();
	    if(firstEvent){
	      sprintf(label,"pip2_pz");
	      vnames->Add(label);
	      cout<<"pip2_pz:\t\t PiPlus2 z momentum \n";
	    }
	    values[n_vectors++] = sqrt( pip2.x()*pip2.x() + pip2.y()*pip2.y() );
	    if(firstEvent){
	      sprintf(label,"pip2_pt");
	      vnames->Add(label);
	      cout<<"pip2_pt:\t\t PiPlus2 transverse momentum \n";
	    }
	    values[n_vectors++] = pip2.V().theta();
	    if(firstEvent){
	      sprintf(label,"pip2_theta");
	      vnames->Add(label);
	      cout<<"pip2_theta:\t\t PiPlus2 lab theta \n";
	    }
	    values[n_vectors++] = pip2.V().phi();
	    if(firstEvent){
	      sprintf(label,"pip2_phi");
	      vnames->Add(label);
	      cout<<"pip2_phi:\t\t PiPlus2 lab phi \n";
	    }
	    // proton
	    values[n_vectors++] = proton.z();
	    if(firstEvent){
	      sprintf(label,"proton_pz");
	      vnames->Add(label);
	      cout<<"proton_pz:\t\t Proton z momentum \n";
	    }
	    values[n_vectors++] = sqrt( proton.x()*proton.x() + proton.y()*proton.y() );
	    if(firstEvent){
	      sprintf(label,"proton_pt");
	      vnames->Add(label);
	      cout<<"proton_pt:\t\t Proton transverse momentum \n";
	    }
	    values[n_vectors++] = proton.V().theta();
	    if(firstEvent){
	      sprintf(label,"proton_theta");
	      vnames->Add(label);
	      cout<<"proton_theta:\t\t Proton lab theta \n";
	    }
	    values[n_vectors++] = proton.V().phi();
	    if(firstEvent){
	      sprintf(label,"proton_phi");
	      vnames->Add(label);
	      cout<<"proton_phi:\t\t Proton lab phi \n";
	    }
	    

	    //
	    // particle betas
	    //
	    values[n_vectors++] = event.cp(PiMinus, 1).beta();
	    if(firstEvent){
	      sprintf(label,"betapim_tof");
	      vnames->Add(label);
	      cout<<"betapim:\t\t  Beta via TOF\n";
	    }
	    values[n_vectors++] = event.cp(PiPlus, 1).beta();
	    if(firstEvent){
	      sprintf(label,"betapip1_tof");
	      vnames->Add(label);
	      cout<<"betapip1:\t\t  Beta via TOF\n";
	    }
	    values[n_vectors++] = event.cp(PiPlus, 2).beta();
	    if(firstEvent){
	      sprintf(label,"betapip2_tof");
	      vnames->Add(label);
	      cout<<"betapip2:\t\t  Beta via TOF\n";
	    }
	    values[n_vectors++] = event.cp(Proton, 1).beta();
	    if(firstEvent){
	      sprintf(label,"betaproton_tof");
	      vnames->Add(label);
	      cout<<"betaproton:\t\t  Beta via TOF\n";
	    }
	    values[n_vectors++] = event.cp(PiMinus, 1).Beta();
	    if(firstEvent){
	      sprintf(label,"betapim");
	      vnames->Add(label);
	      cout<<"betapim:\t\t  Beta via p/E\n";
	    }
	    values[n_vectors++] = event.cp(PiPlus, 1).Beta();
	    if(firstEvent){
	      sprintf(label,"betapip1");
	      vnames->Add(label);
	      cout<<"betapip1:\t\t  Beta via p/E\n";
	    }
	    values[n_vectors++] = event.cp(PiPlus, 2).Beta();
	    if(firstEvent){
	      sprintf(label,"betapip2");
	      vnames->Add(label);
	      cout<<"betapip2:\t\t  Beta via p/E\n";
	    }
	    values[n_vectors++] = event.cp(Proton, 1).Beta();
	    if(firstEvent){
	      sprintf(label,"betaproton");
	      vnames->Add(label);
	      cout<<"betaproton:\t\t  Beta via p/E\n";
	    }

	    //
	    //  Book Invariant Masses
	    //
	    values[n_vectors++] = -(~(beam -(pip1+pim+miss4v))); 
	    if(firstEvent){
	      sprintf(label,"t");
	      vnames->Add(label);
	      cout<<"t:\t\t  t(gamma - X) \n";
	    }
	    values[n_vectors++] = ~(pip1 + pim + miss4v); 
	    if(firstEvent){
	      sprintf(label,"mass3pi1");
	      vnames->Add(label);
	      cout<<"mass3pi1:\t\t  Mass(3pi)\n";
	    }
	    values[n_vectors++] = ~(pip2 + pim + miss4v); 
	    if(firstEvent){
	      sprintf(label,"mass3pi2");
	      vnames->Add(label);
	      cout<<"mass3pi2:\t\t  Mass(3pi)\n";
	    }
	    
	    values[n_vectors++] = ~miss4v;
	    if(firstEvent){
	      sprintf(label,"mm");
	      vnames->Add(label);
	      cout<<"mm:\t\t  Missing mass off the 3pi\n";
	    }
	    values[n_vectors++] = miss4v.z();
	    if(firstEvent){
	      sprintf(label,"mm_pz");
	      vnames->Add(label);
	      cout<<"mm_pz:\t\t Missing z momentum \n";
	    }
	    values[n_vectors++] = sqrt( miss4v.x()*miss4v.x() + 
					miss4v.y()*miss4v.y() );
	    if(firstEvent){
	      sprintf(label,"mm_pt");
	      vnames->Add(label);
	      cout<<"mm_pt:\t\t Missing transverse momentum \n";
	    }
	    
	    values[n_vectors++] = ~(pip1+pip2); 
	    if(firstEvent){
	      sprintf(label,"m2pip");
	      vnames->Add(label);
	      cout<<"m2pip:\t\t  Mass(pip pip)\n";
	    }
	    values[n_vectors++] = ~(pip1+pim); 
	    if(firstEvent){
	      sprintf(label,"mpippim1");
	      vnames->Add(label);
	      cout<<"mpip1pim:\t\t  Mass(pip1 pim)\n";
	    }
	    values[n_vectors++] = ~(pip2+pim); 
	    if(firstEvent){
	      sprintf(label,"mpippim2");
	      vnames->Add(label);
	      cout<<"mpip2pim:\t\t  Mass(pip2 pim)\n";
	    }
	    values[n_vectors++] = ~(miss4v + pip1); 
	    if(firstEvent){
	      sprintf(label,"pim2pip1");
	      vnames->Add(label);
	      cout<<"pim2pip1:\t\t  Mass(pip1 pim2)\n";
	    }
	    values[n_vectors++] = ~(miss4v + pip2); 
	    if(firstEvent){
	      sprintf(label,"pim2pip2");
	      vnames->Add(label);
	      cout<<"pim2pip2:\t\t  Mass(pip2 pim2)\n";
	    }
	    values[n_vectors++] = ~(miss4v + pim); 
	    if(firstEvent){
	      sprintf(label,"2pim");
	      vnames->Add(label);
	      cout<<"2pim:\t\t  Mass(2pim )\n";
	    }
	    
	    //Mass( p pi )
	    values[n_vectors++] = ~(proton + pim); 
	    if(firstEvent){
	      sprintf(label,"p_pim1");
	      vnames->Add(label);
	    }
	    values[n_vectors++] = ~(proton + pip1); 
	    if(firstEvent){
	      sprintf(label,"p_pip1");
	      vnames->Add(label);
	    }
	    values[n_vectors++] = ~(proton + pip2); 
	    if(firstEvent){
	      sprintf(label,"p_pip2");
	      vnames->Add(label);
	    }
	    values[n_vectors++] = ~(proton + miss4v); 
	    if(firstEvent){
	      sprintf(label,"p_pim2");
	      vnames->Add(label);
	    }
	    
	    //////////////
	    if(firstEvent){
	      if(vnames->GetNlabels() != n_vectors){
		cerr<<"Error! vnames->GetNlabels()  !=  n_vectors\nExiting\n"
		    <<vnames->GetNlabels()<<"="<<n_vectors<<endl;
		
		exit(-1);
	      }
	      // Now create the ntuple
	      Char_t *vectorNames = vnames->GetLables();
	      cerr<<"Creating Ntuple\n\tTitle: "
		  <<ntpTitle<<"\n\tVectorNames: "
		  <<vectorNames<<endl;
	      ntpfile->cd();// create the ntuple in this file
	      ntp = new TNtuple("ntp",ntpTitle,vectorNames);
	      firstEvent=kFALSE;
	      
	    }
	    ntp->Fill(values);
	    
	    
	  }
	  
	  
	  if(EventCount++%100==0)
	    cerr << "event no: " << Nevents  << "\r";
	  
	  // clear buffers
	  dropAllBanks(&bcs_,"E");
	  cleanBanks(&bcs_);
	  
	  
	} 
	
      }
      
      if(EventCount){
	cerr << "\nEvents processed from file " << argv[i] << ": " 
	     << EventCount << endl;
	cerr << "\tTotal Read: " << Nevents 
	     << "\tTotal Selected: " << nSelectedEvents << endl;
      }

    }// end of processing bos files
  }
  cerr << "\nTotal number of events read:\t" << Nevents << endl;
    
  ntpfile->Write();
  ntpfile->Close();
  
  return (0);
  }


