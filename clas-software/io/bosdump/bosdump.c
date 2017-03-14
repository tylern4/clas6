/*
 * bosdump.c   Display contents of Bos Banks
 */

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <printBOS.h>

/* declare the bos common */
BOSbank bcs_;
BOSbank wcs_;

/* ----------- Function prototypes ---------------- */
void ctrlCHandle(int);
void PrintUsage(char *processName);
void ProgramEnd();

/* --------------------------------------------------- */


void ProgramEnd()
{
  ;
}

void PrintUsage(char *processName)
{
  fprintf(stderr,"Usage: %s [-M] file1 [file2] etc....\n\n",processName);
  fprintf(stderr,"  Options:\n");
  fprintf(stderr,"\t-M[#]\tPrint out only # number of events\n\n");
  fprintf(stderr,"\t-S[#]\tSkip the first # number of events\n\n");
  fprintf(stderr,"\t-P[#]\tPrint specfic event # number corresponding to HEAD Bank\n\n");
  fprintf(stderr,"\t-p[#]\tPrint event # number from sequential order\n");
  fprintf(stderr,"\t-G<BANKNAME>\tPrint out only these banks (can be used more than once)\n\n");
  fprintf(stderr,"\t-g<BANKNAME>\tDO NOT print these banks (can be used more than once)\n\n");
  fprintf(stderr,"\t-t#\t use number as trigger mask, eg: -t0x4 only dumps trigger bit 3\n");
  fprintf(stderr,"\t-h\tPrint this message.\n\n");
  exit(0);
}

main(int argc,char **argv)
{
  FILE *fp = NULL;
  int i;
  char *argptr;
  int Nevents = 0;
  int max = 0;
  int spf_evt=0,specific_event=0,seq_event=0,sequential_event=0;
  int skip_evt =0;
  char mess[200];
  char *groups[1000];
  int ngroup = 0;
  char *xgroups[1000];
  int nxgroup = 0;
  int maxbanks = 1000;
  clasHEAD_t *HEAD;
  unsigned int triggerMask = 0;

  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);

  for (i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {
      case 'h':
	PrintUsage(argv[0]);
	break;
      case 'M':
	max = atoi(++argptr);
	break;
      case 'S':
	skip_evt = atoi(++argptr);
	break;
      case 'G':
	groups[ngroup++] = ++argptr;
	break; 
      case 'g':
	xgroups[nxgroup++] = ++argptr;
	break;
      case 'p':
	sequential_event=atoi(++argptr);
	seq_event=1;
	break;
      case 'P':
	specific_event=atoi(++argptr);
	spf_evt=1;
	break;
      case 't':
	triggerMask = strtoul(++argptr,NULL,0);
	break;
      default:
	fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
	PrintUsage(argv[0]);
	break;
      }
    }
  }

  if (ngroup) {
    fprintf(stderr,"Groups:\n");
    for (i = 0; i < ngroup; ++i) {
      fprintf(stderr,"%s ",groups[i]);
      if (!(i + 1)%10)
	fputc('\n',stderr);
    }
    fputc('\n',stderr);  
  }

  if (nxgroup) {
    fprintf(stderr,"Exclude Groups:\n");
    for (i = 0; i < nxgroup; ++i) {
      fprintf(stderr,"%s ",xgroups[i]);
      if (!(i + 1)%10)
	fputc('\n',stderr);
    }
    fputc('\n',stderr);  
  }


  bnames_(&maxbanks);
  initbos();
  for (i = 1;i < argc; ++i) {
    argptr = argv[i];
    if (*argptr != '-') {
      sprintf(mess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", argptr);
      if (!fparm_c(mess)) {
	fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],argptr,strerror(errno));
      }
      else {
	while ((max ? Nevents < max : 1) && (getBOS(&bcs_,1,"E") != 0) ) {
	  if(skip_evt>0){
	    skip_evt--;
	    dropAllBanks(&bcs_,"E");
	    cleanBanks(&bcs_);
	    continue;
	  }

	  if (HEAD = (clasHEAD_t *) getBank(&bcs_,"HEAD")) {
	    if (triggerMask ? (triggerMask & HEAD->head[0].trigbits) : 1) {
	      Nevents++;
	      if (Nevents % 100 == 0) {
		fprintf(stderr,"%d\r",Nevents);
		fflush(stderr);
	      }

	      if (spf_evt) {
		if (HEAD->head[0].nevent==specific_event) {
		  PrintEvent(ngroup,groups,nxgroup,xgroups); /*located in c_bos_io*/
		}
	      }
	      else if (seq_event) {
		if (Nevents == sequential_event) {
		  PrintEvent(ngroup,groups,nxgroup,xgroups); /*located in c_bos_io*/
		}
	      }
	      else{
		PrintEvent(ngroup,groups,nxgroup,xgroups); /*located in c_bos_io*/
	      }
	    }
	  }
	  else
	    fprintf(stderr,"UNABLE TO DECIPHER HEAD BANK!\n");
	  dropAllBanks(&bcs_,"E");
	  cleanBanks(&bcs_);
	}
	fprintf(stderr,"#  of events: %d\n",Nevents);
	/*close file*/
	/*sprintf(mess,"CLOSE BOSINPUT", argptr);
	  fprintf(stderr, "err = %d", fparm_c(mess));*/
      }
    
    }

  }
}

void ctrlCHandle(int x)
{
  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  fprintf(stderr,"\n\n\t\t\t***  INTERRUPTED!!!  ***\n\n");
  exit(1);
}

/* end file */







