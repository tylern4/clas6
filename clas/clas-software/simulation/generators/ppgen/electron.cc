/*
 * electron.cc
 */

static const char sccsid[] = "@(#)"__FILE__"\t1.69\tCreated 3/26/97 20:31:35, \tcompiled "__DATE__;

#include <stdio.h>
#include <time.h>
#include <string.h>
#include <math.h>
#include <mcgen.h>
#include <particleType.h>

#define BUFSIZE 10000

void pParticle(Particle_t,ThreeVector,FourVector);
float theta(float QSQ,float W,float E);
float eprime(float theta,float Qsq,float E);

void Usage(char *);


int main (int argc, char *argv[])
{
  FourVector e;
  float E = 2.445;
  float wlo = 1.0;
  float whi = 1.3;
  float qlo = 0;
  float qhi = 1.3;
  int max = 1000;
  float w,q;
  float thet,ep,px,py,pz;
  float phi;

  srand48((long)time(NULL));


  for (int iarg = 1; iarg < argc; ++iarg) {
    char *ptr = argv[iarg];
    if (*ptr == '-') {
      ptr++;
      switch (*ptr) {
      case 'h':
	MUsage (argv[0]);
	exit(0);
	break;
      case 'E':
	E = atof(++ptr);
	break;
      case 'w':
	wlo  = atof(++ptr);
	break;
      case 'W':
	whi  = atof(++ptr);
      case 'q':
	qlo  = atof(++ptr);
	break;
      case 'Q':
	qhi  = atof(++ptr);
	break;
      case 'M':
	max = atoi (++ptr);
      }
    }
  }

 
  while (max--) {

    ThreeVector production = ThreeVector (0.0, 0.0, randm(-2.0,2.0));

    /* get a w and a qsq */

    do {
      w = randm(wlo,whi);
      q = randm(qlo,qhi);
      thet = theta(q,w,E);
    } while (thet < -500.0);
    ep = eprime(thet,q,E);
 
    phi = randm(0,2.0 * M_PI);
    pz = cos(thet) * ep;
    px = sin(thet) * cos(phi) * ep;
    py = sin(thet) * sin(phi) * ep;
      
    e = FourVector (px,py,pz, sqrt(ep * ep + pow (BEAM_MASS, 2.0)));
    pParticle(Electron,production,e);
        
  }
  
}

int comp_double (const void* a, const void* b)
{

    double* da = (double*) a;
    double* db = (double*) b;

    if(*da<*db){
	return -1;
    }
    else if(*da>*db){
	return 1;
    }
    else {
	return 0;
    }
}

double randm (double low, double high)
{
    return ((high - low) * drand48 () + low);
}


float gaussian_gen()
{
 double r1,r2=9999,g1=1;

 while (r2>g1)
   {
    r1=randm(-3,3);
    g1=exp(-1*(r1*r1)/2)/sqrt(2.*M_PI);
    r2=randm(0,1);
   }
 return(r1);
}



void MUsage (char *ProcessName)
{
    cerr << ProcessName << " generates phase space monte carlo events\n\n\n";
 }


void pParticle(Particle_t type,ThreeVector v,FourVector p)
{

  cout << (int) type << " ";
  cout <<   v.X() << " " << v.Y() << " " << v.Z() << " ";
  cout << p.T() << " " << p.X() << " " << p.Y() << " " << p.Z() << endl;

}

 
float theta(float QSQ,float W,float E) 
{

  double k;

  k = (2. * PROTON_MASS * QSQ)/((2.0 * PROTON_MASS * E + PROTON_MASS * PROTON_MASS - W * W - QSQ) * (4.0 * E));
  if (fabs(k) <= 1.0 && k > 0.0) {
    return(2.0 * asin(sqrt(k)));
  }
  else
    return(-1000.0);
}

float eprime(float theta,float Qsq,float E)
{

  return(Qsq/(4. *  E * sin(theta/2.0) * sin(theta/2.0)));
}

  
