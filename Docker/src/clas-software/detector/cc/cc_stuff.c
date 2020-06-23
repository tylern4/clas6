#include <stdio.h> 
#include <math.h> 

#define MAXT 100
#define MAXP  60
#define TH_MIN    5.
#define PHY_MIN -30.
#define DTH   0.5
#define DPHY  1.
#define SMALL  0.001
#define POW2(x)  (x)*(x)
#define MAXTN 225
#define MAXPN 120
#define DPHYN  0.5
#define DTHN   0.2

  /*
C-  Changes made by A. Vlassov - May, 2000
C-
C-   1.  :     Special function, described the Cerenkov efficiency
C-             as a function of the Photoelectron number, is included.
C-             function name : cc_eff_nphe
C-
C-   2.  :     Number of point in the table is changed :
C-             #define MAXTN 225 ( angle theta )
C-             #define MAXPN 120 ( angle phy )
C-   
C-   3.  :     Name of the data file was changed to "cc_eff_new.dat"
C-
C-   4.  :     Add error flag (-4) when it is not enough data
C-             to define the efficiency
  */

/* Global variables */

static float ccnphe[12][MAXP][MAXT];
static float ccnpe[12][MAXPN][MAXTN];
 /* Nsect (1-nphe, 2-err); phy, theta. Theta is changed first */

/* Function prototypes */

int cc_vcrpl_(float *r0, float *dir, float *plane_par,
              float *dist, float *cross_point);
int cc_vcrpl(float *r0, float *dir, float *plane_par,
              float *dist, float *cross_point);
int cceffn(int run_number, int sector_number, float *point,
      float *dir, float *Nphe,float *Nphe_err, float *cc_eff);
int cceffn_(int *run_number, int *sector_number, float *point,
      float *dir, float *Nphe,float *Nphe_err, float *cc_eff);
int cceffn1(int run_number, int sector_number, float *point,
      float *dir, float *Nphe,float *Nphe_err, float *cc_eff,
      float *Theta, float *Phy);
int cceffn1_(int *run_number, int *sector_number, float *point,
      float *dir, float *Nphe,float *Nphe_err, float *cc_eff,
      float *Theta, float *Phy);
int cc_n_ini(int run_number);
int cc_n_ini_(int *run_number);
float cc_eff_nphe(float *nphe);
int cceffn_out(int run_number, int sector_number, float *point,
      float *dir, float *Nphe,float *Nphe_err, float *cc_eff,
      float *Theta, float *Phy);
int cceffn_out_(int *run_number, int *sector_number, float *point,
      float *dir, float *Nphe,float *Nphe_err, float *cc_eff,
      float *Theta, float *Phy);
int cc_n_ini_out(int run_number);
int cc_n_ini_out_(int *run_number);
float cc_eff_out(float *nphe);

/* ---------------- */
/*   Nphe calculation function */

int cceffn_(int *run_number, int *sector_number, float *point,
        float *dir, float *Nphe,float *Nphe_err, float *cc_eff)
  {
  return(cceffn( *run_number, *sector_number, point,
     dir, Nphe, Nphe_err, cc_eff));
  }

int cceffn(int run_number, int sector_number, float *point, float *dir,
              float *Nphe, float *Nphe_err, float *cc_eff)
  {
  /*
C-   Purpose and Methods : calculates the  Cerenkov efficiency
C-                         for given track.
C-                         Needed initialization is done at first call.
C-
C-   Inputs  :   run_number - number of a run - to trace possible
C-                            changes in efficiency
C-               sector_number - number of the sector
C-               point(3) - coordinates of the point on the track
C-                          somewhere after CC (where B=0): 
C-                          it could be EC or SC  matching point.
C-               dir(3)   - direction vector at that point.
C-     *** IMPORTANT ! *** Point and direction must be in Sector RS
C-
C-   Outputs :   Nphe - estimated number of photoelectrons
C-                       for the track.
C-               Nphe_err - estimated error in Nphe
C-               cc_eff - estimated Cerenkov detector efficiency
C-
C-   Controls:   Function returnes :
C-            
C-                0 : no errors
C-               -1 : can't read initialization file
C-               -2 : Wrong sector number ( must be from 1 to 6 )
C-               -3 : No crossing with the plane - check coordinates 
C-                    and direction - possible wrong coordinate system
C-               -4 : Not enough data to define efficiency
C-
C-   Created    23-NOV-1998   Alexander V. Vlassov
C-   Modified   15-MAY-2000   Alexander V. Vlassov
C-
C----------------------------------------------------------------------

  */
  #define RAD2DEG  57.29577951
  #define ENOUGH   10
  static int first = 1;
  int nt,np,i,j,k,n_ent;
  float *nphe,*ernphe;
  float xw,w,wn,dl2;
  float theta,phy,x[3],dist,r,s;
  static float cc_pln[3] = { - 0.0007840784063, 0., - 0.001681461571 };

  if(first)
    { nt = cc_n_ini(run_number);
      if(nt) return(nt);
      first = 0; }

  i = sector_number - 1;
  nphe   =  &ccnpe[2*i][0][0];
  ernphe =  &ccnpe[2*i+1][0][0];
  if(i<0 || i>=6) return(-2);  /* test of correct sector number */

  /* theta, phy definition */

  nt = cc_vcrpl(point, dir, cc_pln, &dist, x);
  if(!nt) return(-3);     /* No cross-point with the plane ! */
  r = sqrt(x[0]*x[0] + x[1]*x[1] + x[2]*x[2]); 
  theta = RAD2DEG * acos(x[2]/r);       /*  angle Theta */
  s = sqrt(x[0]*x[0] + x[1]*x[1]);
  phy = RAD2DEG * atan2(x[1]/s,x[0]/s); /*  angle Phy   */

  /* theta, phy defined */

  nt = (theta - TH_MIN)/DTHN;
  np = (phy - PHY_MIN)/DPHYN;  /* nt & np - array indexes [0 - (MAX-1)]*/
  if(nt >= MAXTN) nt = MAXTN - 1; if(np >= MAXPN) np = MAXPN - 1;
  if(nt <= 0) nt = 0; if(np <= 0) np = 0;
  n_ent = POW2((nphe[MAXTN*np + nt]/ernphe[MAXTN*np + nt])) + 1;
    /* Number of entries in the cell */

  if(n_ent >= ENOUGH)
    {
    *Nphe = nphe[MAXTN*np + nt];
	*Nphe_err = ernphe[MAXTN*np + nt];
    }
  else
    {
    dl2 = sqrt( POW2(TH_MIN + DTHN*(nt+0.5) - theta) +
          POW2(PHY_MIN + DPHYN*(np+0.5) - phy) ) ;
    xw = w = 0.; n_ent = 0;
    for(i=np-1; i<=np+1; i++)  /* Cycle on Phy */
      {
      for(j=nt-1; j<=nt+1; j++)  /* Cycle on Theta */
        {
          if(i>=0 && i<MAXPN && j>=0 && j<MAXTN)
            {
            k = POW2((nphe[MAXTN*i + j]/ernphe[MAXTN*i + j])) + 1;
            wn = dl2*k/sqrt(POW2(TH_MIN  +  DTHN*(j+0.5) - theta) + 
                      POW2(PHY_MIN + DPHYN*(i+0.5) - phy)); 
            w += wn; n_ent += k; 
            xw += (wn * nphe[MAXTN*i+j]);
            }
        }
      }
    if(n_ent >= ENOUGH)
      {
      *Nphe = xw/w;
      *Nphe_err = 1./sqrt(w);
	  }
    else
      {
	  *Nphe = 0.;
	  *Nphe_err = 100.;
	  *cc_eff = 0.;
	  return (-4);
	  }
    }
  *cc_eff = cc_eff_nphe(Nphe); 
  return (0);
  }

int cc_n_ini_(int *run_number)
  {
  return(cc_n_ini( *run_number));
  }

int cc_n_ini(int run_number)
  {
 /* Name of the file  with Nphe data */
  FILE *ptr;
  char st[128];
  char *filen ;
  /*  char *filen = "cc_eff_eg1.dat";*/
  char filenm[128];
  int j,n,k,num_str;
  float *pm, tmp[5];
 
	/* test of the run number ! */
  filen = "cc_eff_new.dat";
  if(run_number >= 25487 && run_number <= 28559)
    {filen = "cc_eff_eg1.dat";} 
  sprintf(filenm,"%s/%s",getenv("CLAS_PARMS"),filen);
  if((ptr=fopen(filenm,"r")) == NULL)
    {
    printf(" *** ERROR in file OPENING %s \n",filenm);
    return(-1);
    }
  /* -------  getting data  ----- */

  pm = &ccnpe[0][0][0];
  num_str = MAXPN*MAXTN*12/5 ;   /* Number of strings in a file */
  for(j=0; j<num_str; j++)       /* Cycle on Strings in a file */
    {
    if(n=fgets(st,128,ptr) != NULL)
      {
      n=sscanf(st,"%f%f%f%f%f",
      &tmp[0],&tmp[1],&tmp[2],&tmp[3],&tmp[4]);
      for(k=0;k<5;k++)
        pm[5*j+k]  = tmp[k];
      }
	}
  fclose(ptr);
  return(0);

  /* -------  data are stored ----- */
  }

int cc_vcrpl_(float *r0, float *dir, float *plane_par,
              float *dist, float *cross_point)
  {
  return(cc_vcrpl( r0, dir, plane_par, dist, cross_point));
  }
int cc_vcrpl(float *r0, float *dir, float *plane_par,
                     float *dist, float *cross_point)
  {
/*
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : crossing of the stright line(R0,d)
C-                         with a plane
C-
C-   Inputs  :   r0(3) - initial point of line
C-               dir(3) - vector direction: r = R0 + s*D
C-               plane_par(3) - array of plane parameters:
C-      plane_par(1)*x + plane_par(2)*y + plane_par(3)*z + 1 = 0
C-
C-   Outputs :   cc_vcrpl =  0 - no cross with the plane.
C-                           1 - cross in positive direction
C-                          -1 - cross in negative direction
C-               dist    =  Distance to the cross point
C-               cross_point(3) =  Cross point coordinates.
C-
C-   Created    23-NOV-1998   Alexander V. Vlassov
C-   Modified   
C-
C----------------------------------------------------------------------

*/
  double a,b,t,c,d[3];
  const double un = 1.0000000000;
  const float vsmall = 0.000001;
  int i, ires;

  a = b = c = 0.;
  *dist = 0.;
  for(i=0;i<3;i++)
    c += un*dir[i]*dir[i];
  c = sqrt(c);
  if(c <= vsmall)
    {
	ires = 0;
	return(ires);
    }
  for(i=0;i<3;i++)
    d[i] = un*dir[i]/c;
  for(i=0;i<3;i++)
    { a += un*plane_par[i]*d[i]; b += un*plane_par[i]*r0[i]; }
  b += un;
  if(fabs(b) <= vsmall)
    {
    for(i=0;i<3;i++)
      cross_point[i] = r0[i];
    ires = 1;
    }
  else
    {
    if(fabs(a) <= vsmall)
      {
      for(i=0;i<3;i++)
        cross_point[i] = 0.;
      ires = 0.;
      }
    else
      {
      t = -b/a;
      for(i=0;i<3;i++)
        cross_point[i] = t*dir[i] + r0[i];
      ires = 1;
      *dist = t;
      if(t < 0.)
        { *dist = -t; ires = -1;}  
      }
	}
  return(ires);
  }

float cc_eff_nphe(float *nphe)

	 /*   Electron efficiency as a function of mean
          number of detected photoelectrons. Based on e1
          runs with EC only trigger.
          A. Vlassov, May, 2000
          Modified July,2002 - more realistic
          for the fiducial region
	 */
  {
  #define OFFSET1 2.2 
  #define OFFSET2 6.7 
  #define P1     -0.739159727
  #define P2      0.52947
  #define P3     -0.20349
  #define P4      0.026204
  #define P5     -0.0011208
  #define P6      0.0052893896
  #define P7     -0.8
  float t,eff;

  t = *nphe;
  if(t <  OFFSET1 ) eff = exp(P1*t);
  else if(t >=  OFFSET1 && t <  OFFSET2 )
    eff = P2 + P3*t + P4*t*t + P5*t*t*t;
  else eff = P6*exp(P7*(t - OFFSET2 ));

  eff = 1. - eff;

  if(eff <= 0.) eff = 0.;
  if(eff >= 1.) eff = 1.;

  return (eff);
  }
/*  NEW Nphe calculation function with additional theta, phy angles */

int cceffn1_(int *run_number, int *sector_number, float *point,
        float *dir, float *Nphe,float *Nphe_err, float *cc_eff,
        float *Theta, float *Phy)
  {
  return(cceffn1( *run_number, *sector_number, point,
     dir, Nphe, Nphe_err, cc_eff, Theta, Phy));
  }

int cceffn1(int run_number, int sector_number, float *point, float *dir,
              float *Nphe, float *Nphe_err, float *cc_eff,
              float *Theta, float *Phy)
  {
  /*
C-   Purpose and Methods : calculates the  Cerenkov efficiency
C-                         for given track.
C-                         Needed initialization is done at first call.
C-
C-   Inputs  :   run_number - number of a run - to trace possible
C-                            changes in efficiency
C-               sector_number - number of the sector
C-               point(3) - coordinates of the point on the track
C-                          somewhere after CC (where B=0): 
C-                          it could be EC or SC  matching point.
C-               dir(3)   - direction vector at that point.
C-     *** IMPORTANT ! *** Point and direction must be in Sector RS
C-
C-   Outputs :   Nphe - estimated number of photoelectrons
C-                       for the track.
C-               Nphe_err - estimated error in Nphe
C-               cc_eff - estimated Cerenkov detector efficiency
C-               theta, phy - projective angles from the target
C-               to the track cross-point with "special plane". 
C-
C-   Controls:   Function returnes :
C-            
C-                0 : no errors
C-               -1 : can't read initialization file
C-               -2 : Wrong sector number ( must be from 1 to 6 )
C-               -3 : No crossing with the plane - check coordinates 
C-                    and direction - possible wrong coordinate system
C-               -4 : Not enough data to define efficiency
C-
C-   Created    23-NOV-1998   Alexander V. Vlassov
C-   Modified   15-MAY-2000   Alexander V. Vlassov
C-
C----------------------------------------------------------------------

  */
  #define RAD2DEG  57.29577951
  #define ENOUGH   10
  static int first = 1;
  int nt,np,i,j,k,n_ent;
  float *nphe,*ernphe;
  float xw,w,wn,dl2;
  float theta,phy,x[3],dist,r,s;
  static float cc_pln[3] = { - 0.0007840784063, 0., - 0.001681461571 };

  if(first)
    { nt = cc_n_ini(run_number);
      if(nt) return(nt);
      first = 0; }

  i = sector_number - 1;
  nphe   =  &ccnpe[2*i][0][0];
  ernphe =  &ccnpe[2*i+1][0][0];
  if(i<0 || i>=6) return(-2);  /* test of correct sector number */

  /* theta, phy definition */

  nt = cc_vcrpl(point, dir, cc_pln, &dist, x);
  if(!nt) return(-3);     /* No cross-point with the plane ! */
  r = sqrt(x[0]*x[0] + x[1]*x[1] + x[2]*x[2]); 
  theta = RAD2DEG * acos(x[2]/r);       /*  angle Theta */
  s = sqrt(x[0]*x[0] + x[1]*x[1]);
  phy = RAD2DEG * atan2(x[1]/s,x[0]/s); /*  angle Phy   */
  *Theta = theta;  *Phy = phy;

  /* theta, phy defined */

  nt = (theta - TH_MIN)/DTHN;
  np = (phy - PHY_MIN)/DPHYN;  /* nt & np - array indexes [0 - (MAX-1)]*/
  if(nt >= MAXTN) nt = MAXTN - 1; if(np >= MAXPN) np = MAXPN - 1;
  if(nt <= 0) nt = 0; if(np <= 0) np = 0;
  n_ent = POW2((nphe[MAXTN*np + nt]/ernphe[MAXTN*np + nt])) + 1;
    /* Number of entries in the cell */

  if(n_ent >= ENOUGH)
    {
    *Nphe = nphe[MAXTN*np + nt];
	*Nphe_err = ernphe[MAXTN*np + nt];
    }
  else
    {
    dl2 = sqrt( POW2(TH_MIN + DTHN*(nt+0.5) - theta) +
          POW2(PHY_MIN + DPHYN*(np+0.5) - phy) ) ;
    xw = w = 0.; n_ent = 0;
    for(i=np-1; i<=np+1; i++)  /* Cycle on Phy */
      {
      for(j=nt-1; j<=nt+1; j++)  /* Cycle on Theta */
        {
          if(i>=0 && i<MAXPN && j>=0 && j<MAXTN)
            {
            k = POW2((nphe[MAXTN*i + j]/ernphe[MAXTN*i + j])) + 1;
            wn = dl2*k/sqrt(POW2(TH_MIN  +  DTHN*(j+0.5) - theta) + 
                      POW2(PHY_MIN + DPHYN*(i+0.5) - phy)); 
            w += wn; n_ent += k; 
            xw += (wn * nphe[MAXTN*i+j]);
            }
        }
      }
    if(n_ent >= ENOUGH)
      {
      *Nphe = xw/w;
      *Nphe_err = 1./sqrt(w);
	  }
    else
      {
	  *Nphe = 0.;
	  *Nphe_err = 100.;
	  *cc_eff = 0.;
	  return (-4);
	  }
    }
  *cc_eff = cc_eff_nphe(Nphe); 
  return (0);
  }
/*  NEW Nphe calculation function with additional theta, phy angles
    for OUTBENDING electron runs */

int cceffn_out_(int *run_number, int *sector_number, float *point,
        float *dir, float *Nphe,float *Nphe_err, float *cc_eff,
        float *Theta, float *Phy)
  {
  return(cceffn_out( *run_number, *sector_number, point,
     dir, Nphe, Nphe_err, cc_eff, Theta, Phy));
  }

int cceffn_out(int run_number, int sector_number, float *point, float *dir,
              float *Nphe, float *Nphe_err, float *cc_eff,
              float *Theta, float *Phy)
  {
  /*
C-   Purpose and Methods : calculates the  Cerenkov efficiency
C-                         for given track.
C-                         Needed initialization is done at first call.
C-
C-   Inputs  :   run_number - number of a run - to trace possible
C-                            changes in efficiency
C-               sector_number - number of the sector
C-               point(3) - coordinates of the point on the track
C-                          somewhere after CC (where B=0): 
C-                          it could be EC or SC  matching point.
C-               dir(3)   - direction vector at that point.
C-     *** IMPORTANT ! *** Point and direction must be in Sector RS
C-
C-   Outputs :   Nphe - estimated number of photoelectrons
C-                       for the track.
C-               Nphe_err - estimated error in Nphe
C-               cc_eff - estimated Cerenkov detector efficiency
C-               theta, phy - projective angles from the target
C-               to the track cross-point with "special plane". 
C-
C-   Controls:   Function returnes :
C-            
C-                0 : no errors
C-               -1 : can't read initialization file
C-               -2 : Wrong sector number ( must be from 1 to 6 )
C-               -3 : No crossing with the plane - check coordinates 
C-                    and direction - possible wrong coordinate system
C-               -4 : Not enough data to define efficiency
C-
C-   Created    11-AUG-2003   Alexander V. Vlassov
C-
C----------------------------------------------------------------------

  */
  #define RAD2DEG  57.29577951
  #define ENOUGH   10
  static int first = 1;
  int nt,np,i,j,k,n_ent;
  float *nphe,*ernphe;
  float xw,w,wn,dl2;
  float theta,phy,x[3],dist,r,s;
  static float cc_pln[3] = { - 0.0007840784063, 0., - 0.001681461571 };

  if(first)
    { nt = cc_n_ini_out(run_number);
      if(nt) return(nt);
      first = 0; }

  i = sector_number - 1;
  nphe   =  &ccnpe[2*i][0][0];
  ernphe =  &ccnpe[2*i+1][0][0];
  if(i<0 || i>=6) return(-2);  /* test of correct sector number */

  /* theta, phy definition */

  nt = cc_vcrpl(point, dir, cc_pln, &dist, x);
  if(!nt) return(-3);     /* No cross-point with the plane ! */
  r = sqrt(x[0]*x[0] + x[1]*x[1] + x[2]*x[2]); 
  theta = RAD2DEG * acos(x[2]/r);       /*  angle Theta */
  s = sqrt(x[0]*x[0] + x[1]*x[1]);
  phy = RAD2DEG * atan2(x[1]/s,x[0]/s); /*  angle Phy   */
  *Theta = theta;  *Phy = phy;

  /* theta, phy defined */

  nt = (theta - TH_MIN)/DTHN;
  np = (phy - PHY_MIN)/DPHYN;  /* nt & np - array indexes [0 - (MAX-1)]*/
  if(nt >= MAXTN) nt = MAXTN - 1; if(np >= MAXPN) np = MAXPN - 1;
  if(nt <= 0) nt = 0; if(np <= 0) np = 0;
  n_ent = POW2((nphe[MAXTN*np + nt]/ernphe[MAXTN*np + nt])) + 1;
    /* Number of entries in the cell */

  if(n_ent >= ENOUGH)
    {
    *Nphe = nphe[MAXTN*np + nt];
	*Nphe_err = ernphe[MAXTN*np + nt];
    }
  else
    {
    dl2 = sqrt( POW2(TH_MIN + DTHN*(nt+0.5) - theta) +
          POW2(PHY_MIN + DPHYN*(np+0.5) - phy) ) ;
    xw = w = 0.; n_ent = 0;
    for(i=np-1; i<=np+1; i++)  /* Cycle on Phy */
      {
      for(j=nt-1; j<=nt+1; j++)  /* Cycle on Theta */
        {
          if(i>=0 && i<MAXPN && j>=0 && j<MAXTN)
            {
            k = POW2((nphe[MAXTN*i + j]/ernphe[MAXTN*i + j])) + 1;
            wn = dl2*k/sqrt(POW2(TH_MIN  +  DTHN*(j+0.5) - theta) + 
                      POW2(PHY_MIN + DPHYN*(i+0.5) - phy)); 
            w += wn; n_ent += k; 
            xw += (wn * nphe[MAXTN*i+j]);
            }
        }
      }
    if(n_ent >= ENOUGH)
      {
      *Nphe = xw/w;
      *Nphe_err = 1./sqrt(w);
	  }
    else
      {
	  *Nphe = 0.;
	  *Nphe_err = 100.;
	  *cc_eff = 0.;
	  return (-4);
	  }
    }
  *cc_eff = cc_eff_out(Nphe); 
  return (0);
  }
int cc_n_ini_out_(int *run_number)
  {
  return(cc_n_ini_out( *run_number));
  }

int cc_n_ini_out(int run_number)
  {
 /* Name of the file  with Nphe data */
  FILE *ptr;
  char st[128];
  char *filen ;
  /*  char *filen = "cc_eff_eg1.dat";*/
  char filenm[128];
  int j,n,k,num_str;
  float *pm, tmp[5];
 
	/* test of the run number ! */
  filen = "cc_eff_outbend.dat";
  sprintf(filenm,"%s/%s",getenv("CLAS_PARMS"),filen);
  if((ptr=fopen(filenm,"r")) == NULL)
    {
    printf(" *** ERROR in file OPENING %s \n",filenm);
    return(-1);
    }
  /* -------  getting data  ----- */

  pm = &ccnpe[0][0][0];
  num_str = MAXPN*MAXTN*12/5 ;   /* Number of strings in a file */
  for(j=0; j<num_str; j++)       /* Cycle on Strings in a file */
    {
    if(n=fgets(st,128,ptr) != NULL)
      {
      n=sscanf(st,"%f%f%f%f%f",
      &tmp[0],&tmp[1],&tmp[2],&tmp[3],&tmp[4]);
      for(k=0;k<5;k++)
        pm[5*j+k]  = tmp[k];
      }
	}
  fclose(ptr);
  return(0);

  /* -------  data are stored ----- */
  }

float cc_eff_out(float *nphe)

	 /*   Electron efficiency as a function of mean
          number of detected photoelectrons for outbending
          electron runs.
 
          Based on eg1b outbending runs 1.6 GeV
          with EC only trigger.
          A. Vlassov, August, 2003
	 */
  {
  #define OFF1 2.0 
  #define OFF2 5.0 
  #define OFF3 10.0 
  #define PP1      0.66871
  #define PP2     -0.57001
  #define PP3     -0.20349
  #define PP4     -1.0445
  #define PP5     -0.77377
  #define PP6      0.0043137
  #define AA       0.029132386
  float x,t,eff;

  t = *nphe;
  if(t < 1. ) eff = 1.;
  else if(t <  OFF1 )
    { x = t - OFF1;
    eff = PP1 + PP2*x + PP3*x*x;}
  else if(t >=  OFF1 && t <  OFF2 )
    { x = t - OFF1; 
    eff = PP1 * exp( PP4*x);}
  else if(t >=  OFF2 && t <  OFF3 )
    { x = t - OFF2; 
    eff = (AA - PP6) * exp( PP5*x) + PP6;}
  else
    eff = PP6 + (AA - PP6)*exp(PP5 * (OFF3 - OFF2));

  eff = 1. - eff;

  if(eff <= 0.) eff = 0.;
  if(eff >= 1.) eff = 1.;

  return (eff);
  }







