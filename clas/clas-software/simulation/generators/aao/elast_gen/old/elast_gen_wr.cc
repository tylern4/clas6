// File: elast_gen_wr.cc
// Last Modified: July 12, 1999
//
//  This file puts a C++ wrapper on elast_gen.F to allow for
//   command line argument capabilities
//


#include <iostream.h>

extern "C" {

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <bosddl.h>
#include <utility.h>
#include <makebanks.h>


// function prototypes
void PrintUsage(char *processName);
void elast_gen_wrapper(char*,char*,char*,char*,float*,float*,int*,float*,float*,float*,
	float*,float*,float*,float*,float*,float*,int*,int*,float*);
}


main(int argc,char **argv)
{ 
  int i,j,k;
  char *argptr;
  char *o = NULL;
  const int NumFiles = 4;  // number of output files to be created
  char *files[NumFiles];
  float   f = 0.;
  float   d = 0.001;
  int     g = 0;
  float  tl = 0.;
  float  tr = 0.;
  float  vx = 0.;
  float  vy = 0.;
  float  vz = 0.;
  float   E = 0.;
  float emn = 0.;
  float smn = 0.;
  float smx = 0.;
  int     p = 0;
  int     M = 0;
  float   c = 0.;

  char F[NumFiles][132];

  for(i=0; i<NumFiles; ++i) {
    files[i] = "";
  }

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

      case 'f':
        f = atof(++argptr);
        break;
        
      case 'd':
        d = atof(++argptr);
        break;

      case 'g':
        g = atoi(++argptr);
        break;

      case 't':
	switch (*(++argptr)){
	case 'l':
	  tl = atof(++argptr);
	  break;
	case 'r':
	  tr = atof(++argptr);
	  break;
	default:
    	  fprintf(stderr,"Unrecognized argument: [-t%s]\n\n",argptr);
	  PrintUsage(argv[0]);
	  break;
	}
	break;

      case 's':
	switch (*(++argptr)){
	case 'm':
  	  switch (*(++argptr)){
       	  case 'n':
	    smn = atof(++argptr);
	    break;
       	  case 'x':
	    smx = atof(++argptr);
	    break;
	  default:
    	    fprintf(stderr,"Unrecognized argument: [-sm%s]\n\n",argptr);
	    PrintUsage(argv[0]);
	    break;
	  }
	  break;
	default:
    	  fprintf(stderr,"Unrecognized argument: [-s%s]\n\n",argptr);
	  PrintUsage(argv[0]);
	  break;
	}
	break;

      case 'e':
	switch (*(++argptr)){
	case 'm':
  	  switch (*(++argptr)){
       	  case 'n':
	    emn = atof(++argptr);
	    break;
	  default:
    	    fprintf(stderr,"Unrecognized argument: [-em%s]\n\n",argptr);
	    PrintUsage(argv[0]);
	    break;
	  }
	  break;
	default:
    	  fprintf(stderr,"Unrecognized argument: [-e%s]\n\n",argptr);
	  PrintUsage(argv[0]);
	  break;
	}
	break;

      case 'v':
	switch (*(++argptr)){
	case 'x':
	  vx = atof(++argptr);
	  break;
	case 'y':
	  vy = atof(++argptr);
	  break;
	case 'z':
	  vz = atof(++argptr);
	  break;
	default:
    	  fprintf(stderr,"Unrecognized argument: [-v%s]\n\n",argptr);
	  PrintUsage(argv[0]);
	  break;
	}
	break;

      case 'M':
	M = atoi(++argptr);
	break;

      case 'p':
	p = atoi(++argptr);
	break;

      case 'E':
	E = atof(++argptr);
	break;

      case 'c':
	c = atof(++argptr);
	break;

      case 'o':
	o =  *(++argptr) ? argptr : (char*) "/dev/fd/1";
        // creates name for bos file
        strcpy(F[0],o);
        F[0][strlen(o)] = '.';
        F[0][strlen(o)+1] = 'e';
        F[0][strlen(o)+2] = 'v';
        F[0][strlen(o)+3] = 't';
        F[0][strlen(o)+4] = '\0';
        // creates name for output file
        strcpy(F[1],o);
        F[1][strlen(o)] = '.';
        F[1][strlen(o)+1] = 'o';
        F[1][strlen(o)+2] = 'u';
        F[1][strlen(o)+3] = 't';
        F[1][strlen(o)+4] = '\0';
        // creates name for histogram file
        strcpy(F[2],o);
        F[2][strlen(o)] = '.';
        F[2][strlen(o)+1] = 'r';
        F[2][strlen(o)+2] = 'z';
        F[2][strlen(o)+3] = '\0';
        // creates name for summary file
        strcpy(F[3],o);
        F[3][strlen(o)] = '.';
        F[3][strlen(o)+1] = 's';
        F[3][strlen(o)+2] = 'u';
        F[3][strlen(o)+3] = 'm';
        F[3][strlen(o)+4] = '\0';

        // assigns the character arrays to their corresponding char*
        for(k=0; k<NumFiles; ++k) {         
          files[k] = F[k];
        }
	break;

      default:
	fprintf(stderr,"Unrecognized argument: [-%s]\n\n",argptr);
	PrintUsage(argv[0]);
	break;
      }
    }
  }  

  elast_gen_wrapper(files[0],files[1],files[2],files[3],&f,&d,&g,&tl,&tr,&vx,&vy,&vz,
  			&E,&emn,&smn,&smx,&p,&M,&c);  

}
    
 
void PrintUsage(char *processName)
{
  cout << "Usage: " << processName << " [options] \n\n";
  cout << "\toptions:\n";
  cout << "\t-ofilename\toutput to filename\n";
  cout << "\t-f[#]\tFraction of events expected for elastic peak\n";
  cout << "\t-d[#]\tegam < d considered elastic\n";
  cout << "\t-g[#]\tNumber of particles to detect\n";  
  cout << "\t-tl[#]\tLength of target cell (cm)\n";  
  cout << "\t-tr[#]\tRadius of target cell (cm)\n";  
  cout << "\t-vx[#]\tX-coordinate of beam position\n";
  cout << "\t-vy[#]\tY-coordinate of beam position\n";
  cout << "\t-vz[#]\tZ-coordinate of beam position\n";    
  cout << "\t-E[#]\tBeam energy (GeV)\n";
  cout << "\t-emn[#]\tMinimum value of scattered electron energy\n";
  cout << "\t-smn[#]\tMinimum value of electron scattering angle (deg.)\n";  
  cout << "\t-smx[#]\tMaximum value of electron scattering angle (deg.)\n";
  cout << "\t-p[#]\t(#)=0: include elastic peak   (#)=1: exclude elastic peak\n";
  cout << "\t-M[#]\tProcess (#) number of events\n";
  cout << "\t-c[#]\tMaximum cross section\n";
  cout << "\t-h\tThis information\n\n";
  exit(0);
}
