#include <iostream>
//#include <bos.hh> // not needed due to
#include <seb.hh>
#include <mctk.hh>
#include <mvrt.hh>
#include <part.hh>
#include <string>

#include <TTree.h>
#include <TFile.h>

#include "Options.hh"

// C libraries and external functions:
extern "C" {
#define BOS_IN_ID  10
#include <stdio.h>
#include <string.h>

#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
  //#include <clas_cern.h> // not using the clas_cern libraries
  // #include <bosddl.h> // already included by bostypes.h
#include <utility.h>
#include <makebanks.h>
#include <trip.h>
#include <vertex.h>

  BOSbank bcs_;
  BOSbank wcs_;
  
  void bnames_ (int *);
}

void printhelpmsg () {
  std::cerr << "h10maker is a replacement for nt10maker."
            << std::endl << std::endl
            << "The usual pipeline from BOS to ROOT files is:" << std::endl
            << "BOS -> nt10maker -> h2root, which needs the PAW" << std::endl
            << "library.  Unfortunately I've had problems with PAW" << std::endl
            << "on Centos 7, so I've created this program to avoid PAW."
            << std::endl << std::endl;

  std::cerr << "Usage: h10maker [flags] <infile.bos> <outfile.root>"
            << std::endl << std::endl;

  std::cerr << "flags must be at least one of:"
            << std::endl << std::endl
            << "-p, --part;" << std::endl
            << "-m, --mctk; or" << std::endl
            << "-r, --rec" << std::endl
            << std::endl
            << "-p, --part switches on PART bank output," << std::endl
            << "-m, --mctk switches on the MCTK and MVRT output, and" << std::endl
            << "-r, --rec switches on the reconstructed banks."
            << std::endl << std::endl;
  
  return;
}

int main(int argc, char** argv)
{
  if(argc <= 3) {
    printhelpmsg();
    return 1;
  }

  //Argument parsing:
  OptionSpec options;

  options.AddOption('h',"help",no_argument,"show this help message");
  options.AddOption('p',"part",no_argument,"enable PART bank");
  options.AddOption('m',"mctk",no_argument,"enable MCTK+MVRT banks");
  options.AddOption('r',"rec",no_argument,"enable reconstructed banks");

  Flags flags;

  std::vector<std::string> leftovers;

  parseargs(argc,argv,options,flags,leftovers);

  if(flags.Check('h')) {
    printhelpmsg();
    return 1;
  }
  
  bool partp=false, mctkp=false, sebp=false;
  if(flags.Check('p')) partp=true;
  if(flags.Check('m')) mctkp=true;
  if(flags.Check('r')) sebp=true;

  //Input and output files:
  std::string infile = leftovers[0];
  std::string outfile = leftovers[1];

  //debug
  std::cout << infile << " " << outfile << std::endl;
  //end debug

  //BOS inits:
  int bos_in_id = BOS_IN_ID;
  int MaxBanks = 1000;
  char mess[100];

  bnames_(&MaxBanks);
  initbos();
  configure_banks(stderr,0);

  //Create bos open message
  sprintf(mess, "OPEN BOSINPUT UNIT=%d FILE=\"%s\" READ",
          bos_in_id, infile.c_str());

  //Open bos file
  if(!fparm_c(mess)) {
    fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",
            argv[0],infile.c_str(),strerror(errno));
  }

  //Setup output file and TTree:
  TFile out(outfile.c_str(),"RECREATE");

  TTree tree("h10","h10");
  if(partp) part_branches(&tree);
  if(mctkp) {
    mctk_branches(&tree);
    //mvrt_branches(&tree);
  }
  if (sebp) seb_branches(&tree);

  //Start read loop
  int ierr = 0;
  bool eof = false;
  while(!eof) {
    //Read from BOS file
    ierr = getBOS(&bcs_,bos_in_id,"E");
    //if(ierr == -1) eof = true;
    if(!ierr) eof = true;
    else {
      //NOTE: I'm not sure what this trip function is really doing.  I
      //can try enabling and disabling it in the future.
      if(trip_() == 0) {
        //Extract BOS banks
        if(partp) fill_part_nt_();
        if(mctkp) {
          fill_mc_nt_();
          //fill_mvrt_nt_();
        }
        if (sebp) fill_seb_nt_();
        tree.Fill();
      }

      dropAllBanks(&bcs_,"E");
      cleanBanks(&bcs_);
    }
  }

  //Close input and output farms
  sprintf(mess, "CLOSE BOSINPUT");
  fparm_c(mess);

  tree.Write();
  out.Close();
  return 0;
}
