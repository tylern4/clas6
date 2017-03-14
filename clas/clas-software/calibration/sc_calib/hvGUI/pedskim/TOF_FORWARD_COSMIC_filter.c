/*
 *  Event filter for cosmic data taken for Forward TOF calibration
 *
 * author: Joe Santoro
 * input:  Forward cosmic data taken with Cole Smith's EC
 * output: much smaller, single BOS file with just the TOF banks needed by tofHVcalib
 *
 */


#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <makebanks.h>
#include <map_manager.h>

/* declare the bos common */
BOSbank bcs_;
BOSbank wcs_;

/* -------------- Function prototypes ---------------- */
void ctrlCHandle(int);
void PrintUsage(char *processName);
int clean_up();

/* --------------------------------------------------- */

void PrintUsage(char *processName)
{
  fprintf(stdout,"   \n");
  fprintf(stdout,"  The Forward TOF cosmic Bank filter  \n");
  fprintf(stdout,"   \n");
  fprintf(stdout,"   \n");
  fprintf(stdout,"  Options:\n");
  fprintf(stdout,"\t-o<outfile>\t Output to <outfile>\n");
  fprintf(stdout,"\t-a<outfile>\t Append to <outfile>\n");
  fprintf(stdout,"\t-n[int#]\t Process only # number of events\n");
  fprintf(stdout,"\t-b\t\t Batch mode\n");
  fprintf(stdout,"\t-h\t\t Print this message.\n\n");
  exit(0);
}

main(int argc,char **argv)
{
  FILE *fp = NULL;
  int Nevents = 0;
  int i, firsttime;
  int nwrite = 0, max = 0;
  int current_run = 0, runno;
  int batch_flag = 0;
  char *argptr;
  char *outfile = NULL;
  char out[1000], mess[1000];
  char *dir,map[128];

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
      case 'n':
	max = atoi(++argptr);
	break;
      case 'b':
	batch_flag = 1;
	break;
      case 'o':
        outfile =  *(++argptr) ? argptr : "/dev/fd/1";
        fprintf(stdout,"Output file: %s\n",outfile);
        unlink(outfile);
        sprintf(out, "OPEN BOSOUTPUT UNIT=7 FILE=\"%s\" WRITE STATUS=NEW RECL=3600", outfile);
          if (!fparm_c(out)) {
          fprintf(stdout,"%s: Unable to open file \'%s\': %s\n\n",argv[0],out,strerror(errno));
          exit(1);
          }
        break;
      case 'a':
	outfile =  *(++argptr) ? argptr : "/dev/fd/1";
	fprintf(stdout,"Output file: %s\n",outfile);
	sprintf(out, "OPEN BOSOUTPUT UNIT=7 FILE=\"%s\" ACTION=READWRITE FORM=BINARY STATUS=OLD ACCESS=SEQ RECL=3600", outfile);
	  if (!fparm_c(out)) {
	  fprintf(stdout,"%s: Unable to open file \'%s\': %s\n\n",argv[0],out,strerror(errno));
	  exit(1);
	  }
	break;
      default:
	fprintf(stdout,"Unrecognized argument: [-%s]\n\n",argptr);
	PrintUsage(argv[0]);
	break;
      }
    }
  }

  initbos();
  bankList(&bcs_,"T=","HEADSC  ");

  for (i = 1;i < argc; ++i) {
    argptr = argv[i];
    if (*argptr != '-') {
      sprintf(mess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", argptr);
      if (!fparm_c(mess)) {
       fprintf(stdout,"%s: Unable to open file \'%s\': %s\n\n",argv[0],argptr,strerror(errno));
      }
      else {
      while ((max ? Nevents < max : 1) &&  getBOS(&bcs_,1,"E")) {
       clasHEAD_t *HEAD = getBank(&bcs_, "HEAD");
       Nevents++;
          runno = HEAD->head[0].nrun;

         if((Nevents % 100 == 0) && !batch_flag){
          fprintf(stderr,"Run:%d\tEvent:%d\r",runno,Nevents);
          fflush(stderr);
         }

         if(outfile){
          putBOS(&bcs_, 7, "T");
          nwrite++;
          }

          dropAllBanks(&bcs_,"T");
          cleanBanks(&bcs_);
          }

         clean_up();

      }
    }
  }

}


void ctrlCHandle(int x)
{
  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  fprintf(stdout,"\n\n\t\t\t*** !!! HASTA LA VISTA !!!  ***\n\n");
  clean_up();
  exit(1);
}

int clean_up(){
  char mess[100];
  sprintf(mess, "CLOSE BOSOUTPUT UNIT=7");
  fprintf(stdout, "%s, err = %d\n", mess, fparm_c(mess));
}

/* end file */














