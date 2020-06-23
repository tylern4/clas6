/******************************************************************************
       nt10maker.cc

       Author:  Kyungseon Joo (Aug. 1999)

       Purpose: This program creates ntuple 10 from BOS file with seb banks

       Usage: nt10maker [options] inputFile 1 [inputFile2] etc.

        options:
        -ofilename      output to filename
        -M[#]   Process  only (#) number of events
        -t      Data Type 1:exp    2:monte carlo (PART bank only)
        -g      Photon run - include TGPB bank (default off)
        -h      This information

       Jun 2002: modified by S. Strauch and S. Niccolai
       The code now can run on two input files
       The ntuple now also contains the MVRT x,y,z vertex and the TGPI latch1
*******************************************************************************/
#include <iostream>
/*------------ Logical unit for input and output bos files    --------- */
#define BOS_IN_ID  10
#define NTID       10
#define NTLUN       1

/*------------ Define C ----------------- */
extern "C" {

#include <stdio.h>
#include <string.h>

#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <clas_cern.h>
#include <bosddl.h>
#include <utility.h>
#include <makebanks.h>
#include <trip.h>
#include <vertex.h>

using std::cout;

BOSbank bcs_;
BOSbank wcs_;

/*---------------  function prototypes ----------------*/
void PrintUsage(char *processName);
void bnames_(int *);
void filter_wrapper(int *, int *, int *, int *, char *, int *, int *, int *, char *);
}
#include <math.h>

main(int argc,char **argv)
{
  int bos_in_id = BOS_IN_ID;
  int MaxBanks = 1000;
  int icycle = 0;
  int data_type = 1;
  char beam_type = 'e';
  int ntid = NTID;
  int ntlun = NTLUN;
  int nfile, nfile_max;
  int i,j;
  char *argptr;
  int nevent_max = 10000000;
  char *ntfile = NULL;
  char mess[100];
  char out[100];
  int run_number = 0;
  int ctrip = 0;
  char *tripFile=NULL;

  if(argc == 1) {
    PrintUsage(argv[0]);
    exit(0);
  }
  for (i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {
      case 'h':
    PrintUsage(argv[0]);
    break;
      case 'k':
    ctrip=1;
        tripFile=++argptr;
        open_trip_file(tripFile);
        break;
      case 'M':
    nevent_max = atoi(++argptr);
    break;
      case 't':
    data_type = atoi(++argptr);
    break;
      case 'g':
    beam_type = 'g';
    break;
      case 'o':
    ntfile =(char *) (*(++argptr) ? argptr : "/dev/fd/1");
    break;
    PrintUsage(argv[0]);
    break;
      }
    }
  }

  bnames_(&MaxBanks);
  initbos();
  configure_banks(stderr,0);

  // get number of bos-input files

  nfile_max = 0;
  for (i = 1;i < argc; ++i) {
    argptr = argv[i];
    if (*argptr != '-') {
      nfile_max++;
    };
  };


  nfile = 0;
  for (i = 1;i < argc; ++i) {
    argptr = argv[i];
    if (*argptr != '-') {
      sprintf(mess,"OPEN BOSINPUT UNIT=%d FILE=\"%s\" READ", bos_in_id, argptr);
      if (!fparm_c(mess)) {
    fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],argptr,strerror(errno));
    nfile_max = nfile_max - 1;
      }
      else {
    nfile++;
        filter_wrapper(&nfile, &nfile_max, &nevent_max, &bos_in_id, ntfile, &ntid, &ntlun, &data_type, &beam_type);
      }
    }
  }
}


void PrintUsage(char *processName)
{
  cout << "Usage: " << processName << " [options] inputFile 1 [inputFile2] etc. \n\n";
  cout << "\toptions:\n";
  cout << "\t-kfilename\ttripfile filename (knock out trips if present)\n";

  cout << "\t-ofilename\toutput to filename\n";
  cout << "\t-M[#]\tProcess  only (#) number of events\n";
  cout << "\t-t\tData Type 1:exp    2:monte carlo (PART bank only) 11:SEB reduced N-tuple\n";
  cout << "\t-g\tProcess as photon run (include TGPB bank)\n";
  cout << "\t-h\tThis information\n";
  exit(0);
}
