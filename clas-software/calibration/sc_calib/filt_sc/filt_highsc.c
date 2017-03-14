/*
 *   event filter (  filter chosen events )
 *
 * author: Luminita Todor
 * May 04, 2001
 * input:  Cooked or Raw data
 * output: events that hit scintillator paddles 40-48
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
#include <printBOS.h>
#include <math.h>
#include <pid.h>
#include <vertex.h>

#define TRUE 1
#define FALSE 0

/* declare the bos common */
BOSbank bcs_;
BOSbank wcs_;

/* -------------- Function prototypes ---------------- */
void open_file(char *outfile,int unit);
static void signalINT(int isig);
static void signalSEGV(int isig);
void ctrlCHandle(int);
int install_fault_handlers();
void PrintUsage();
/* --------------------------------------------------- */

/*--------------- Fault handlers ----------------------*/
static void signalINT(int isig)
{
  char mess[100];
  static int count = 0;

  count++;
  fprintf(stderr,"signalINT: Caught signal %d %d\n",isig,count);
  if (count < 4){
    signal(isig,signalINT);
  } else {
    dropAllBanks(&bcs_,"E"); /*drop the banks we read in*/
    dropAllBanks(&bcs_,"C"); /*drop the cooked banks we made*/
    cleanBanks(&bcs_);
    /*close files*/
    sprintf(mess,"CLOSE BOSOUTPUT UNIT=12");
    fparm_c(mess);
    /*close files*/
    sprintf(mess,"CLOSE BOSINPUT UNIT=1");
    fparm_c(mess);
    exit(0);
  }
}
static void signalSEGV(int isig)
{
  static int icount = 0;
  
  fprintf(stderr,"signalSEGV: Caught signal %d\tcount:\t%d\n",isig,icount++); 
  if (icount < 3)
    signal(isig,signalSEGV);
  else {
    exit(1);
  }    
}

int install_fault_handlers(){
  signal(SIGINT,signalINT);  
  signal(SIGSEGV,signalSEGV);
  signal(SIGABRT,signalSEGV);
  signal(SIGBUS,signalSEGV);
  fprintf(stderr,"Fault handlers installed\n");
}

/* -------End of fault handlers ------------*/

main(int argc,char **argv)
{
  FILE *fp = NULL;
  char outfile[1000],mess[1000];
  char *prefix = NULL;
  int batchmode=FALSE,pad,pm=40,pu=48;
  int max = 100000000,smin=0,smax=6;
  char *argptr;
  clasSC_t *SC = NULL;
  int i,ii,ij,Nevents=0,nbig[6],fsave=0,Nsaved=0;

  /* parse command line parameters */
  for (i=1; i<argc; i++) 
    {
      argptr = argv[i];
      if (*argptr == '-') 
	{
	  argptr++;
	  switch (*argptr) 
	    {
	    case 'h':
	      PrintUsage(argv[0]);
	      break;
	    case 'i':
	      batchmode=TRUE;
	      break;
	    case 'n':
	      max = atoi(++argptr);
	      break;
	    case 's':
	      smax = atoi(++argptr);
              smin=smax-1;
	      break;
	    case 'm':
	      pm = atoi(++argptr);
	      break;
	    case 'M':
	      pu = atoi(++argptr);
	      break;
	    case 'o':
	      prefix =  *(++argptr) ? argptr : "/dev/fd/1";
	      break;
	    default:
	      fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
	      PrintUsage(argv[0]);
	      break;
	    }
	}
    }
  initbos();
  install_fault_handlers();
  if (!prefix) {
    sprintf(prefix,"Bos_highSC.out\n"); 
    fprintf(stderr, "\nYou did not specify BOS output name, use %s\n",prefix);
  }
   open_file(prefix,12);
  fprintf(stderr, "\n After open output\n");
  for (i = 1;i < 6; ++i) nbig[i]=0;

  for (i = 1;i < argc; ++i) {
    argptr = argv[i];
    fprintf(stderr,"%s:\n",argptr);
    if (*argptr != '-') {
      sprintf(mess,"OPEN BOSINPUT UNIT=1 FILE=\"%s\" READ", argptr);
      fprintf(stderr,"%s:\n",mess);
      if (!fparm_c(mess)) {
	fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],argptr,strerror(errno));
      }
      else {
	while ((max ? Nevents < max : 1) &&  getBOS(&bcs_,1,"E")){
          Nevents++;
       	  if (Nevents % 100 == 0&&!batchmode) 
	    /*	  fprintf(stderr,"%d: 1:%d\t 2:%d\t 3:%d\t 4:%d\t 5:%d\t 6:%d\n",Nevents,nbig[0],nbig[1],nbig[2],nbig[3],nbig[4],nbig[5]);*/
	    fprintf(stderr,"%d: %d\n",Nevents,Nsaved);
	  fsave=0;
          for (ii=smin;ii<smax;ii++)              
            if (SC=getGroup(&bcs_,"SC  ",ii+1)) 
              for(ij=0;ij<SC->bank.nrow;ij++) 
                if(SC->sc[ij].id>=pm && SC->sc[ij].id<=pu) 
                   {if((SC->sc[ij].adcl!=0)&&(SC->sc[ij].adcr!=0))fsave++;
                    if(fsave&&((SC->sc[ij].tdcl==0)||(SC->sc[ij].tdcr==0)))fsave=0;
		    /*		   if(fsave)nbig[ii]++;*/
		   }            
          if(fsave) putBOS(&bcs_, 12, "E");
          if(fsave) Nsaved++; 
	  dropAllBanks(&bcs_,"E");
	  cleanBanks(&bcs_);
	}
      }
    }
  }
}
    
  
void open_file(char *outfile,int unit){
  char out[1000];
  fprintf(stderr,"Output file %d: %s\n",unit,outfile);
  sprintf(out, "OPEN BOSOUTPUT%d UNIT=%d FILE=\"%s\" WRITE STATUS=NEW RECL=3600",unit, unit, outfile);
  if (!fparm_c(out)) {
     fprintf(stderr,"Unable to open file \'%s\': %s\n\n",out,strerror(errno));
	    exit(1);
  }
  return;
}

void PrintUsage()
{
  fprintf(stderr,"   \n");
  fprintf(stderr,"  Event-Filter for events with valid hits in 40-48 \n scintillator paddles based on SC bank \n");
  fprintf(stderr,"   \n");
  fprintf(stderr,"  Options:\n");
  fprintf(stderr,"\t-o<prefix>\t Output name.\n");
  fprintf(stderr,"\t-n<#>\t\t read only <#> of events\n"); 
  fprintf(stderr,"\t-i<#>\t\t run in batch mode\n"); 
  fprintf(stderr,"\t-s<#>\t\t only sector designated by <#>\n");
  fprintf(stderr,"\t-m<#>\t\t lowest paddle <#>; default 40\n");
  fprintf(stderr,"\t-M<#>\t\t highest paddle <#>; default 48\n");
  fprintf(stderr,"\t-h\t\t Print this message.\n\n");
  exit(0);
}
