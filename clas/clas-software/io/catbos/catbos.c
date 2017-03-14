/*
 * catbos.c   Reconstitute BOS files into one file
 *                   - M. Klusman
 */

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/types.h>
#include <malloc.h>
#include <time.h>
#include <sys/stat.h>
#include <ntypes.h>
#include <bostypes.h>


/* ----------- Function prototypes ---------------- */
void ctrlCHandle(int);
void PrintUsage(char *processName);

/* ----------- CLAS DEFINES         ---------------- */

#define CTYPE_EVENT 1

/* --------------------------------------------------- */

#define BUFSIZE 700000

int Nevents = 0;
int Tevents = 0;

char *initString = "";
#define MAXFILELEN 2000000

void PrintUsage(char *processName)
{
  fprintf(stderr,"Usage: %s -o<fileoutput> file1 file2 ... \n\n",processName);
  fprintf(stderr,"\t%s <file1>, <file2> .. into one overall file\n\n",processName);
  fprintf(stderr,"\t-o\tOutput of final file name\n");
  fprintf(stderr,"\t-m#\tSet maximum file length to # kbytes, append file # to file name\n");
  fprintf(stderr,"\t-R\tRenumber the events in HEAD bank so that the output has consecutive event numbers\n");
  fprintf(stderr,"\t-h\tPrint this message.\n\n");
  exit(0);
}


main(int argc,char **argv)
{
  FILE *fp = NULL;

  int i,total_size=0,nFileOut=0, batch=0;
  int Nevents = 0, nwrite=0;
  int BOSInputUnitNo = 1,BOSOutputUnitNo=7;

  char *argptr;
  char *outfile = NULL;
  char *template,*extension;
  char *OutList = "E";
  char out[100],mess[200];
  char filename[200];

  int maxFileLength = 0;
  char outf[200];
  int nfile = 0;
  int renumber=0;

  clasHEAD_t *HEAD;

  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);

  for (i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) { 
     case 'm':
        maxFileLength = atoi(++argptr);
        break;
      case 'B':
        batch = 1;
        break;
      case 'h':
        PrintUsage(argv[0]);
        break;
      case 'o':
        outfile = ++argptr;
        break;
      case 'R':
        renumber = 1;
        break;
      default:
        fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
        PrintUsage(argv[0]);
        break;
      }
    }
  }

  if (maxFileLength && outfile)
    sprintf(outf,"%s.%2.2d",outfile,nfile);
  else if (outfile)
    strcpy(outf,outfile);
  
  initbos();

  unlink(outfile);
  sprintf(out, "OPEN BOSOUTPUT UNIT=%d FILE=\"%s\" WRITE STATUS=NEW RECL=3600", BOSOutputUnitNo,outf);
  if (!fparm_c(out)) {
    fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],out,strerror(errno));        
    exit(1);
  }
  
  if (!batch) {
    fprintf(stderr,"Number of\tSize of\t\tCurrent\n");
    fprintf(stderr,"Events\t\tFile (KB)\tFile\n");
    fprintf(stderr,"---------\t---------\t-------\n\n");
  }

  for (i = 1;i < argc; ++i) {
    argptr = argv[i];
    if (*argptr != '-') { 
      sprintf(filename,"%s",argptr);
      sprintf(mess,"OPEN BOSINPUT UNIT=%d FILE=\"%s\" READ", BOSInputUnitNo,argptr);
      if (!fparm_c(mess)) {
        fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],argptr,strerror(errno));
      }

      while (getBOS(&bcs_,BOSInputUnitNo,"E")) {
        Nevents++;
        if (!batch) {
          if (Nevents % 25 == 0) {
            fprintf(stderr,"%d\t\t%4d\t\t%s\r",Nevents,total_size,filename);
            fflush(stderr);
          }
        }

        if(renumber){ /* We need to resequence the event numbers. */
          if( (HEAD = (clasHEAD_t *)getBank(&bcs_,"HEAD")) !=NULL){
            HEAD->head->nevent=Nevents;
          }
        }
        /*now start patching together*/
        
        if (outfile) {
          putBOS(&bcs_, BOSOutputUnitNo, OutList);
          nwrite++;
        }
        total_size = fileLength("BOSOUTPUT");
        dropAllBanks(&bcs_,"E");
        cleanBanks(&bcs_);                    
        if (maxFileLength && (total_size > maxFileLength)) {
                      /*close file*/
                      sprintf(mess,"CLOSE BOSOUTPUT UNIT=7");
                      fparm_c(mess);
                      sprintf(outf,"%s.%2.2d",outfile,++nfile);
                      /*                      fprintf(stderr,"Output file: %s\n",outfile); */
                      unlink(outfile);
                      sprintf(out, "OPEN BOSOUTPUT UNIT=7 FILE=\"%s\" WRITE STATUS=NEW RECL=3600", outf);
                      if (!fparm_c(out)) {
                        fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],out,strerror(errno));         
                        exit(1);
                      }                      
                    }
      }
      putBOS(&bcs_, BOSOutputUnitNo, "0");
      sprintf(mess,"CLOSE BOSINPUT UNIT=%d ",BOSInputUnitNo, argptr);
      fparm_c(mess);

    }
  }

  /* close the input and output files */

  sprintf(mess,"CLOSE BOSOUTPUT UNIT=%d ",BOSOutputUnitNo, outfile); 
  fparm_c(mess);
    
  if (!batch) {
    fprintf(stderr,"%d\t\t%d\n\n",nwrite,total_size);
    fprintf(stderr,"Concatenation successful!\n");
    fflush(stderr);
  }
}

void ctrlCHandle(int x)
{
  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  fprintf(stderr,"\n\n\t\t\t***  INTERRUPTED!!!  ***\n\n");
  exit(1);
}


