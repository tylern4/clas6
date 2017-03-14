/* -=======================================================- *
$Id: gamp2txt.cc,v 1.6 2008/02/28 02:55:00 eugenio Exp $
$Author: eugenio $
$Revision: 1.6 $
$Date: 2008/02/28 02:55:00 $
* -=======================================================- */
//#include <stdio.h>
#include<iostream>
using namespace std;
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <time.h>
#include <ntypes.h> 
#include <bostypes.h> 
#include <pid.h> 
#include <pputil.h>
#include <pdgutil.h>
#include <scalers.h>
#include <particleType.h>

double randm (double low, double high);
int Q(Particle_t pid);
void PrintUsage(char *processName);
void parseTargetPosition(char *str,double *zmin,double *zmax);
double gaussian(double xmean,double xwidth);
extern "C" {
  int nchar(char *,char);
}

int main(int argc,char *argv[])
{
  
  int NPart;
  char *argptr;

  float Eelectron,Egamma,toffset;
  float t,px,py,pz,x,y,z;
  float p,bx,by,bz,Ebeam;
  float PrimaryBeam = 4.0;
  int id;
  int i;
  int q;
  int beamType = 1;
  double zmin = 0, zmax = 0,zvert = 0.0;
  double xvert = 0.0, yvert = 0.0;
  double beamWidth = 0.0;
  long int RDMseed = 0;
  char *prefix = NULL;
  int nline;

  for(i=1; i < argc; ++i){
    argptr = argv[i];
    if( *(argptr = argv[i])  == '-') {
 
      switch(*(++argptr)) {
      case 'p':
	prefix = ++argptr;
	break;
      case 'z':
	parseTargetPosition(++argptr,&zmin,&zmax);
	break;
      case 'h':
        PrintUsage(argv[0]);
        exit(1);
	break;
       case  'E':
	PrimaryBeam = atof(++argptr);
	break;
      case 'r':
	beamWidth = atof(++argptr);
	break;

      }
    }
  }


    if(!RDMseed){
      srand48((long)time(NULL));
    }else{
      srand48(RDMseed);
    }

  while ( (cin >> NPart)) {
    zvert = randm(zmin,zmax);
    xvert = gaussian(0.0,beamWidth);
    yvert = gaussian(0.0,beamWidth);
    NPart--; 
   if (prefix) {
     nline = 0;
      cout << prefix << nline++ << " ";
    }
    cout << NPart  << endl;

    cin >> beamType >> q >> bx >> by >> bz >> Ebeam;
    p = sqrt(bx * bx + by * by + bz * bz);
    if (prefix) {
      cout << prefix << nline++ << " ";
    }
    cout << PrimaryBeam  << " " << p  << " 0 "  << endl;
    for(i=0; i<NPart; i++){
      cin >> id >> q >> px >> py >> pz >> t;
    if (prefix) {
      cout << prefix << nline++ << " ";
    }
      cout << id  << endl; 
   if (prefix) {
      cout << prefix << nline++ << " ";
    }
      cout << t << " " << px << " " << py << " " << pz << " " << endl;
    if (prefix) {
      cout << prefix << nline++ << " ";
    }
      cout << xvert << " " << yvert << " " << zvert << endl;
    }
  }
 
}
void PrintUsage(char *processName)
{
  cerr << processName << ":  convert txt format to gamp format" << endl;
  cerr << "input:\tstdin text file of particle 4-vectors" << endl;
  cerr << "output (stdout):\tgamp format " << endl;
  cerr << "options: " << endl;
  cerr << "\t-h\tprint this Help message." << endl;
  cerr << "\t-b#\toutput beam type as # (geant code) default=1 (photon)" << endl;
  cerr << "\t-zZMIN,ZMAX\tset target z limits" << endl;
  cerr << "\t-E#\tPrimary beam momentum" << endl;
}








int Q(Particle_t pid)
{
  int ret = 0;
  switch (pid) {
  case PiPlus:
  case Proton:
  case KPlus:
  case Positron:
    ret = 1;
    break;
  case PiMinus:
  case Electron:
  case KMinus:
    ret = -1;
    break;
  case Gamma:
  case Neutron:
  case KLong:
  case KShort:
  case Pi0:
    ret = 0;
    break;
  default:
    break;
  }
  return(ret);
}
void parseTargetPosition(char *str,double *zmin,double *zmax)
{
  double z1 = 0,z2 = 0;
  char *word;
  if (nchar(str,',') == 1) {
    /* user has specified a 2-vector */
    word = strtok(str,",");
    z1 = atof(word);
    word = strtok(NULL,",");
    z2  = atof(word);
  }
  else if (nchar(str,',') == 0) {
    z1 = z2 = atof(str);
  }
  else {
    cerr << "error parsing target position:" << str << endl;
  }
  cerr << "Target position set to: " << z1 << " -> " << z2 << endl;
  *zmin = z1;
  *zmax = z2;
}

double randm (double low, double high)
{
  return ((high - low) * drand48 () + low);
}
double gaussian(double xmean,double xwidth)
{
 double r1,r2=9999,g1=1;

 while (r2>g1)
   {
    r1=randm(-3,3);
    g1=exp(-1*(r1*r1)/2)/sqrt(2.*M_PI);
    r2=randm(0,1);
   }
 return(xwidth * r1 + xmean);
}
