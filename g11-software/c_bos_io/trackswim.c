/*
 * trackswim.c
 *
 * 17 Apr 1992 Const Olchanski
 * 23 Jul 1997 D. P. Weygand - modified for CLAS/Cebaf
 *
*/

#include <stdio.h>
#include <math.h>
#include <assert.h>

#include <trackswim.h>
#include <g3D.h>


/* field routines */
int field_ffield(const vector3_t *r,vector3_t *b);
int field_isOn(void);      /* check field on/off status. return value: 0 - off, !0 - on */

int Bclas(const vector3_t *V,vector3_t *B);





#ifndef BIT
#define BIT(n) (1<<(n))
#endif

#ifndef PI
#define PI 3.1415927
#endif

#define SIGN(x) ((x)>0?(x):-(x))

#define DELTA (0.001)    /* small number used to compare FP numbers */

typedef float (*dist_f_t)(const vector3_t*,const void*);

#define SWIM_F_DBG_PLANE    BIT(4)   /* debug swimming to a plane */
#define SWIM_F_DBG_SWIMMER  BIT(5)   /* debug swimmer itself */
#define SWIM_F_DBG_POINT    BIT(6)   /* debug swimming to a point */
#define SWIM_F_DBG_SPLANE   BIT(7)   /* debug swimming to a plane + swimmer */
#define SWIM_F_DBG_SPOINT   BIT(8)   /* debug swimming to a point + swimmer */

#define SWIM_F_DBG_SWIMMERP  BIT(9)   /* debug swimmer itself */

typedef struct
{
  int flags;

  int sw; /* tracking methode switch */

  double step_line;   /* step for linear swimmer */
  double step_arc;    /* step for arc swimmer    */
  double step_tracb;  /* step for tracb swimmer    */

  double plane_max_dist; /* maximum approach distance to a plane */
  double point_max_dist; /* maximum approach distance to a point */
  double maxSwim;        /* maximum distance to swim in one trackswim() call */

  double min_x,max_x;
  double min_y,max_y;
  double min_z,max_z;

} trackswim_config_t;

static trackswim_config_t swim;
static int inited = 0;

int trackswim_init(void)
{

  /* default values */
  swim.flags = 0;

  swim.sw = 0;

  swim.step_line   = 0.5;
  swim.step_arc    = 8.0;
  swim.step_tracb  = 2.0;
  swim.plane_max_dist  = 0.0005;
  swim.point_max_dist  = 0.0050;
  swim.maxSwim  = 50.0;

  swim.min_x = -500;
  swim.min_y = -500;
  swim.min_z = -1000;
  swim.max_x = 500;
  swim.max_y = 500;
  swim.max_z = 2000;

  inited = 1;
  return 0;
}

/* Units convention :
      x,maxs,step - centimeters
      d?_ds - centimeteres/centimeters
      d2?_ds2 - centimeters/centimeters^2
      b - kGauss (0.1 Tesla)
      q - multiple of elem. charge (for p+ = +1.0, for e- = -1.0)
      p - GeV/c
*/

#define C_LIGHT (0.299792458/1000.0)   /* velocity of light */

#define SQR(x) ((x)*(x))

/*
 **********************************************************************************
         B E G I N N I N G   O F   L I N E A R   S W I M M E R    C O D E
 **********************************************************************************
*/

static float linemin(const vector3_t *v,const vector3_t *dv,float smin,float smax,
		     dist_f_t dist_f,const void*dist_par)
{
  vector3_t r1;
  float s,s1,s2,s3,s4;
  float d1,d2,d3,d4;
  float gold = 0.618;    /* (sqrt(5)-1)/2 */
  float ss = smax - smin;

  s1 = smin;
  s2 = smax - ss*gold;
  s3 = smin + ss*gold;
  s4 = smax;

  r1.x = v->x + s1 * dv->x;
  r1.y = v->y + s1 * dv->y;
  r1.z = v->z + s1 * dv->z;
  
  d1 = dist_f(&r1,dist_par);
      
  r1.x = v->x + s2 * dv->x;
  r1.y = v->y + s2 * dv->y;
  r1.z = v->z + s2 * dv->z;
  
  d2 = dist_f(&r1,dist_par);

  r1.x = v->x + s3 * dv->x;
  r1.y = v->y + s3 * dv->y;
  r1.z = v->z + s3 * dv->z;
  
  d3 = dist_f(&r1,dist_par);

  r1.x = v->x + s4 * dv->x;
  r1.y = v->y + s4 * dv->y;
  r1.z = v->z + s4 * dv->z;
  
  d4 = dist_f(&r1,dist_par);

  while (fabs(ss) > 0.0005)
    {
      if (swim.flags & SWIM_F_DBG_SWIMMER)
	{
	  fprintf(stderr,"s: %10.4lf %10.4lf %10.4lf %10.4lf",s1,s2,s3,s4);
	  fprintf(stderr,"  d: %10.4lf %10.4lf %10.4lf %10.4lf\n",d1,d2,d3,d4);
	}

      if (d2 > d3)
	{
	  s1 = s2; d1 = d2;
	  s2 = s3; d2 = d3;
	  ss = s4 - s1;

	  s3 = s1 + gold*ss;

	  r1.x = v->x + s3 * dv->x;
	  r1.y = v->y + s3 * dv->y;
	  r1.z = v->z + s3 * dv->z;
	  
	  d3 = dist_f(&r1,dist_par);
	}
      else
	{
	  s4 = s3; d4 = d3;
	  ss = s4 - s1;

	  s3 = s2; d3 = d2;
	  
	  s2 = s4 - gold*ss;

	  r1.x = v->x + s2 * dv->x;
	  r1.y = v->y + s2 * dv->y;
	  r1.z = v->z + s2 * dv->z;
	  
	  d2 = dist_f(&r1,dist_par);
	}
    }

  return (s1 + s4)/2.0;
}

static int trackswim_line(vector3_t *x,vector3_t *dx,float p,float q,
			  float maxdist,float mindist,dist_f_t dist_f,const void*dist_par)
{
  vector3_t b, r, dr, d2r;
  float s,ds, C, bestdist, smax,step;
  int doneFlag = 0;
  int ret;

  r = *x;
  dr = *dx;
  
  C = C_LIGHT*q/p;

  smax = fabs(maxdist);

  if (swim.step_line > smax) step = maxdist;
  else step = copysign(swim.step_line,maxdist);

  ds = step;

  if (dist_f) bestdist = dist_f(&r,dist_par);
  else bestdist = HUGE_VAL;

  s = 0.0;
  while (doneFlag == 0)
    {
      vector3_t r1;

      ds = step;

      ret = Bclas(&r, &b);

      if (!ret) {
	  fprintf(stderr,"trackswim_line: Error: Cannot get field at %10.4lf %10.4lf %10.4lf, error %d\n",
		  r.x,r.y,r.z,ret);
	  return -1;
	}

      r1.x = r.x + ds * dr.x;
      r1.y = r.y + ds * dr.y;
      r1.z = r.z + ds * dr.z;

      if (dist_f)
	{
	  float dist = dist_f(&r1,dist_par);

	  if (swim.flags & SWIM_F_DBG_SWIMMER)
	    fprintf(stderr,"trackswim_line: %10.4lf %10.4lf %10.4lf, dist: %10.4lf\n",
		   r1.x,r1.y,r1.z,dist);
	  
	  if (dist < bestdist) bestdist = dist;
	  else
	    {
	      ds = linemin(&r,&dr,-ds,+ds,dist_f,dist_par);

	      r1.x = r.x + ds * dr.x;
	      r1.y = r.y + ds * dr.y;
	      r1.z = r.z + ds * dr.z;

	      dist = dist_f(&r1,dist_par);

	      if (swim.flags & SWIM_F_DBG_SWIMMER)
		fprintf(stderr,"trackswim_line: %10.4lf %10.4lf %10.4lf, dist: %10.4lf\n",
		       r1.x,r1.y,r1.z,dist);
	  
	      doneFlag = 1;
	    }
	}

      r = r1;

      d2r.x = C * (dr.y * b.z - dr.z * b.y);
      d2r.y = C * (dr.x * b.z - dr.z * b.x);
      d2r.z = C * (dr.x * b.y - dr.y * b.x);

      dr.x += ds * d2r.x;
      dr.y += ds * d2r.y;
      dr.z += ds * d2r.z;

      s+=ds;
      
      if (fabs(s)>smax) break;
    }

  *x = r;
  *dx = dr;
  if (fabs(s)<smax) return 0;
  else return 1;
}

/*
 **********************************************************************************
           E N D   O F   L I N E A R   S W I M M E R    C O D E
 **********************************************************************************
*/


/*
 **********************************************************************************
        B E G I N N I N G   O F   C I R C U L A R   S W I M M E R    C O D E
 **********************************************************************************
*/

/*
     These are 'polynomial' cosine and sine.
     The 'calibrate' function determines
     the maximum angle 'maxangle', for which pcos() and psin()
     do not deviate from sin() and cos() by "too much".
*/

static float pcos(float a)
{
  float a2 = a*a;
  return 1.0 - a2/2.0 + a2*a2/24.0;
}

static float psin(float a)
{
  return a - a*a*a/6.0;
}

static int calibrated = 0;
static float maxangle = 0; /* in radians */
static vector3_t v3inf;

static void calibrate(void)
{
  double a,step,d;
  double s1,s2,s,c1,c2,c;
  double threshold = 0.0001; /* 0.01% accuracy */

  step = 1.0;

  for (a= step; a<90.0; a+=step)
    {
      d = a * PI/180.0;

      s1 = sin(d); s2 = psin(d); s = (s1-s2)/s1;
      c1 = cos(d); c2 = pcos(d); c = (c1-c2)/c1;

      if (fabs(s) > threshold) break;
      if (fabs(c) > threshold) break;
    }

  maxangle = d;

  fflush(stdout);
  fprintf(stderr,"trackswim_arc.calibrate: Sin, Cos accuracy %lf%%, max angle: %.0lf degrees, %lf radians\n",
	  threshold*100.0,maxangle/PI*180.0,maxangle);

  v3inf.x = v3inf.y = v3inf.z = HUGE_VAL;

  calibrated = 1;
}

/*
 *   Returns the cross product of the two vectors
*/

static const vector3_t *cross_product(const vector3_t*v1,const vector3_t *v2)
{
  static vector3_t ret;

  ret.x = v1->y*v2->z - v1->z*v2->y;
  ret.y = -(v1->x*v2->z - v1->z*v2->x);
  ret.z = v1->x*v2->y - v1->y*v2->x;
  
  return &ret;
}

typedef struct
{
  int flags;

  vector3_t v;     /* current position      */
  vector3_t b;     /* B at 'v'              */
  vector3_t bunit; /* B unit vector at 'v'  */
  float  bmag;     /* magnitude of B at 'v' */

  vector3_t dv;    /* unit momentum vector                      */
  vector3_t dvb;   /* component of 'dv': parallel to B      */
  vector3_t dvr;   /*                    perpendicular to B */

  vector3_t runit; /*                    B cross dvr - unit vector */
  vector3_t dvrunit;

  float dvbmag;    /* magnitude of dvb: used to split step into
                                stepb (along dvb) and
                                stepr (along dvr and r) components */
  float dvrmag;    /* sqrt(1-SQR(dcbmag))                          */


  float r1;        /* inverse of bending radius (1/R) */

  float bestdist;  /* best distance so far */
  float dist;      /* distance at 'v'      */

  dist_f_t    dist_f;
  const void* dist_arg;

} arc_state_t;

static float arcStep(vector3_t *v_ret,const arc_state_t *s,float step)
{
  vector3_t v1;
  float l,l2,r12,r1;
  float stepva,stepra;
  float arc;
  float stepv,stepr,stepb;

  l = step;

  r1 = s->r1;
  l2 = SQR(l);
  r12 = SQR(r1);

  stepv = s->dvrmag * l*(1 - l2*r12/6);            /* this is stepv = R*sin(l/R)       */
  stepr = s->dvrmag * l2*r1 * (0.5 - l2*r12/12);   /* this is stepr = R*(1-cos(l/R))   */
  stepb = s->dvbmag * step;

/*  fprintf(stderr,"Arc: %10.4lf, Curv: %10.4lf, StepB: %10.4lf, stepV: %10.4lf, stepR: %10.4lf\n",arc,r1,stepb,stepv,stepr);*/

  v1.x = s->v.x + s->dvrunit.x * stepv + s->runit.x * stepr + s->bunit.x * stepb;
  v1.y = s->v.y + s->dvrunit.y * stepv + s->runit.y * stepr + s->bunit.y * stepb;
  v1.z = s->v.z + s->dvrunit.z * stepv + s->runit.z * stepr + s->bunit.z * stepb;

  if (v_ret) *v_ret = v1;

  if (s->dist_f)   return s->dist_f(&v1,s->dist_arg);
  else             return (-1);
}

static int arcRotate(arc_state_t *s,float step)
{
  float arc = step * s->r1;   /* arc = step/R    */
  float Cos = pcos(arc);
  float Sin = psin(arc);

  s->dv.x = s->dvb.x + s->dvrmag*(Cos * s->dvrunit.x + Sin * s->runit.x);
  s->dv.y = s->dvb.y + s->dvrmag*(Cos * s->dvrunit.y + Sin * s->runit.y);
  s->dv.z = s->dvb.z + s->dvrmag*(Cos * s->dvrunit.z + Sin * s->runit.z);
  
  return 0;
}

static float arcmin(const arc_state_t *state,float smin,float smax,float mindist)
{
  float ds = fabs(smax - smin)/2.0;
  float ss = (smin + smax)/2.0;

  float dd = arcStep(NULL,state,ss);

  while (dd > SQR(mindist))
    {
      float s1 = ss - ds;
      float s2 = ss;
      float s3 = ss + ds;

      float d1 = arcStep(NULL,state,s1);
      float d2 = dd;
      float d3 = arcStep(NULL,state,s3);

      float a = ((d1-d2)*(s2-s3) - (d2-d3)*(s1-s2));
      float b = ((d1-d2)*(SQR(s2)-SQR(s3)) - (d2-d3)*(SQR(s1)-SQR(s2)));

      if (fabs(a) > 0.000001)
	{
	  ss = b/(2.0*a);

	  /*assert(!is_NaN(ss));*/

	  if (ss<s1-ds)
	    ss = s1;
	  
	  if (ss>s3+ds)
	    ss = s3;
	}
      else
	ss = s2;
      
      dd = arcStep(NULL,state,ss);

      if (state->flags & SWIM_DBG_TRACE)
	{
	  fprintf(stderr,"trackswim_arcmin: ds: %10.4lf  s: %10.4lf %10.4lf %10.4lf %10.4lf  ",
		 ds,s1,s2,s3,ss);
	  fprintf(stderr,"  d: %10.4lf %10.4lf %10.4lf %10.4lf  ",d1,d2,d3,dd);

	  if (fabs(dd - d2) < SQR(mindist)) fprintf(stderr,"(dd-d2)->exit ");
	  if (fabs(ss - s2) < mindist) fprintf(stderr,"(ss-s2)->exit ");
	  if (ds < mindist) fprintf(stderr,"ds->exit ");
	  if (dd < SQR(mindist)) fprintf(stderr,"dd->exit ");

	  fprintf(stderr,"\n");
	}

      if (fabs(dd - d2) < SQR(mindist)) break;
      if (fabs(ss - s2) < mindist) break;

      ds /= 2.0;

      if (ds < mindist) break;
    }

  return (ss);
}

static float arcmin_gold(const arc_state_t *state,float smin,float smax,float mindist)
{
  static const float gold = 0.618;    /* (sqrt(5)-1)/2 */
  float s,s1,s2,s3,s4;
  float d1,d2,d3,d4;
  float ss = smax - smin;

  s1 = smin;
  s2 = smax - ss*gold;
  s3 = smin + ss*gold;
  s4 = smax;

  d1 = arcStep(NULL,state,s1);
  d2 = arcStep(NULL,state,s2);
  d3 = arcStep(NULL,state,s3);
  d4 = arcStep(NULL,state,s4);

  while (fabs(ss) > mindist)
    {
      if (state->flags & SWIM_DBG_TRACE)
	{
	  fprintf(stderr,"arcmin_gold: s: %10.4lf %10.4lf %10.4lf %10.4lf",s1,s2,s3,s4);
	  fprintf(stderr,"  d: %10.4lf %10.4lf %10.4lf %10.4lf\n",d1,d2,d3,d4);
	}

      if (d2 > d3)
	{
	  s1 = s2; d1 = d2;
	  s2 = s3; d2 = d3;
	  ss = s4 - s1;

	  s3 = s1 + gold*ss;

	  d3 = arcStep(NULL,state,s3);
	}
      else
	{
	  s4 = s3; d4 = d3;
	  ss = s4 - s1;

	  s3 = s2; d3 = d2;
	  
	  s2 = s4 - gold*ss;

	  d2 = arcStep(NULL,state,s2);
	}
    }

  return (s1 + s4)/2.0;
}

static int trackswim_arc(vector3_t *ret_x,vector3_t *dx,float p,float q,
			 float max,float mindist,dist_f_t dist_f,const void*dist_par,
			 int flags)
{
  const vector3_t v3zero = { 0,0,0 };
  float s;  /* accumulated path */

  int doneFlag = 0;
  float maxs,stepExt;
  float k0;
  float step;
  float zzz;
  int haveB = 0;
  int testSingleStep = 1;
  int ret;

  arc_state_t state;

  if (!calibrated) calibrate();

  if (is_NaN(max))
    {
      fprintf(stderr,"trackswim_arc: Error: max=%10.4lf is NaN\n",max);
      return -1;
    }

  maxs = fabs(max);

  if (swim.step_arc > maxs) stepExt = max;
  else  stepExt = copysign(swim.step_arc,max);

  step = stepExt;                      /* arc length */

  k0 = q/p * C_LIGHT;

  s = 0.0;

/*
fprintf(stderr,"p/q: %10.4lf, dv: %10.4lf, %10.4lf, %10.4lf\n",p/q,dv.x,dv.y,dv.z);
*/

  state.dist_f   = dist_f;
  state.dist_arg = dist_par;

  state.v  = *ret_x;
  state.dv = *dx;

  state.flags = flags;

  { /* normalize the dv vector */

    float dvmag = v3mag(&state.dv);
    
    state.dv.x /= dvmag;
    state.dv.y /= dvmag;
    state.dv.z /= dvmag;
  }

#ifdef UNDEF
  if (is_NaN(state.v.z)||is_NaN(state.v.y)||is_NaN(state.v.x))
    {
      abort();
    }
#endif

  ret = Bclas(&state.v, &state.b);

  if (!ret) {
      fprintf(stderr,"trackswim_arc: Error: Cannot get field at %10.4lf %10.4lf %10.4lf, error %d\n",
	      state.v.x,state.v.y,state.v.z,ret);
      return -1;
    }

  state.bmag = v3mag(&state.b);

  if (state.bmag > 0.0)
    {
      haveB = 1;
  
      state.bunit.x = state.b.x / state.bmag;
      state.bunit.y = state.b.y / state.bmag;
      state.bunit.z = state.b.z / state.bmag;
    }
  else
    {
      haveB = 0;

      state.bunit = v3zero;
    }

  if (state.dist_f) state.bestdist = state.dist_f(&state.v,state.dist_arg);
  else state.bestdist = HUGE_VAL;

  if (state.flags & SWIM_DBG_TRACE)
    fprintf(stderr,"trackswim_arc: step: %10.4lf, position: %10.4lf %10.4lf %10.4lf, dist: %10.4lf (starting point)\n",
	   step,state.v.x,state.v.y,state.v.z,state.bestdist);
  
  while (doneFlag == 0)
    {
      vector3_t bnew;
      vector3_t vnew;
      vector3_t vold;
      float distNew;

      vold = state.v;

      state.runit  = v3zero;
      state.dvb    = v3zero;
      state.dvbmag = 0;
      state.dvrmag = 1;
      state.dvr    = state.dv;
      state.dvrunit = state.dv;
      state.r1     = 0;

      if (haveB)
	{
	  float rmag;

	  /* decompose 'dv' into two components:
	       dvb: parallel to B and
               dvr: orthogonal to B


	     dvb = B * (dv,B)
	     dvr = dv - dvb

	  */

	  state.dvbmag = (state.dv.x*state.bunit.x + state.dv.y*state.bunit.y + state.dv.z*state.bunit.z);

	  state.dvb.x = state.bunit.x * state.dvbmag;   /* component of dv parallel to B */
	  state.dvb.y = state.bunit.y * state.dvbmag;
	  state.dvb.z = state.bunit.z * state.dvbmag;

	  state.dvr.x = state.dv.x - state.dvb.x;       /* component of dv perpendicular to B */
	  state.dvr.y = state.dv.y - state.dvb.y;
	  state.dvr.z = state.dv.z - state.dvb.z;

	  state.dvrmag = v3mag(&state.dvr);

	  state.dvrunit.x = state.dvr.x/state.dvrmag;
	  state.dvrunit.y = state.dvr.y/state.dvrmag;
	  state.dvrunit.z = state.dvr.z/state.dvrmag;

	  state.runit = *cross_product(&state.dv,&state.bunit);

	  rmag = v3mag(&state.runit);

	  state.runit.x /= rmag;
	  state.runit.y /= rmag;
	  state.runit.z /= rmag;

	  state.r1 = k0 * state.bmag;  /*   this is 1/Radius  */

#ifdef UNDEF
	  if (state.flags & SWIM_DBG_TRACE)
	    fprintf(stderr,"trackswim_arc: bbb: %10.4lf dv: %6.3lf %6.3lf %6.3lf, dvb: %6.3lf %6.3lf %6.3lf, dvr: %6.3lf %6.3lf %6.3lf, r1: %10.4lf\n",
		   state.dvbmag,
		   state.dv.x,state.dv.y,state.dv.z,
		   state.dvb.x,state.dvb.y,state.dvb.z,
		   state.dvr.x,state.dvr.y,state.dvr.z,
		   state.r1);
#endif
	} /* endif (haveB) */


      {
	/*
	 * Here we check that the step size does not get bigger than
	 * the maximum arc length 
	 */

	float angle;

	angle = step * state.r1;   /* angle = step/R    */

/*
fprintf(stderr,"b: %10.4lf, %10.4lf, %10.4lf,  r1: %10.4lf, angle: %10.4lf, l: %10.4lf\n",b.x,b.y,b.z,r1,angle,l);
*/
	if (angle > maxangle)
	  {
	    float newStep = step * maxangle/angle;

	    if (state.flags & SWIM_DBG_TRACE)
	      fprintf(stderr,"trackswim_arc: Arc (%10.4lf) is longer than maximum (%10.4lf), reducing step size from %10.4lf to %10.4lf\n",angle,maxangle,step,newStep);

	    step = newStep;  /* decrease step */
	  }
      }

      {
	/*
	 * Try to make a step
	*/

	vector3_t v1;
	float dist;

/*fprintf(stderr,"About to call arcStep with step %10.4lf, curvature: %10.4lf\n",step,state.r1);*/

	dist = arcStep(&v1,&state,step);

	if (state.flags & SWIM_DBG_TRACE)
	  fprintf(stderr,"trackswim_arc: step: %10.4lf, position: %10.4lf %10.4lf %10.4lf, dist: %10.4lf\n",
		 step,v1.x,v1.y,v1.z,dist);
	
	if (state.dist_f)
	  {
	    int doArcmin = 0;

	    if (dist >= state.bestdist)
	      {
		doArcmin = 1;
	      }
	    else
	      {
		state.bestdist = dist;

		if (testSingleStep)
		  {
		    dist = arcStep(NULL,&state,1.1 * step);

		    if (dist > state.bestdist)
		      {
			doArcmin = 1;
		      }

		    testSingleStep = 0;
		  }
	      }

	    if (doArcmin)
	      {
		/* at this point we assume that we reached the boundary */

		if (state.flags & SWIM_DBG_TRACE)
		  fprintf(stderr,"trackswim_arc: Calling arcmin with step %10.4lf and mindist %10.4lf\n",step,mindist);
	
		step = arcmin(&state,-step,+step,mindist);

		dist = arcStep(&v1,&state,step);

		if (state.flags & SWIM_DBG_TRACE)
		  fprintf(stderr,"trackswim_arc: Arcmin returned step %10.4lf, at %10.4lf %10.4lf %10.4lf, dist: %10.4lf\n",
			 step,v1.x,v1.y,v1.z,dist);

		state.v = v1;

		if (haveB) arcRotate(&state,step);

		break;  /* done. exit from 'trackswim_arc' */
	      }
	  }
/*
fprintf(stderr,"Step: %10.4lf, v1: %10.4lf %10.4lf %10.4lf \n",step,v1.x,v1.y,v1.z);
*/
	vnew = v1;
	distNew = dist;
      }

      /* we made one step... */

      {
	/*
	 * now calculate the B field in the new point and check if the step size should be decreased
	 * loop until step size is small enough
	 *
	 */

	float maxstepSize;
	float deltab,deltar;
	float lmax2,lmax;
	float newstep;

	while (1)
	  {
	    

#ifdef UNDEF
	    if (is_NaN(vnew.z)||is_NaN(vnew.y)||is_NaN(vnew.x))
	      {
		abort();
	      }
#endif
	    ret = Bclas(&vnew, &bnew);
	    
	    if (!ret) {
		fprintf(stderr,"trackswim_arc: Error: Cannot get field at %10.4lf %10.4lf %10.4lf, error %d\n",
			vnew.x,vnew.y,vnew.z,ret);
		return -1;
	      }

	    /* calculate the maximum step size */
	      
	    /*
	     * Here: deltab -   magnitude of the magnetic field gradient
	     *       deltar -   required track precision
	     *       lmax   -   maximum step size to keep within required precision
	     */
	    
	    deltab = sqrt(SQR(state.b.x-bnew.x)+SQR(state.b.y-bnew.y)+SQR(state.b.z-bnew.z))/fabs(step);
	    deltar = 0.0001; /* 1 um */

	    if (deltab < 0.0001) /* the magnetic field change is less than 0.1 Gauss, field did not change. */
	      break;
	      
	    lmax2 = fabs(deltar/deltab / k0);   /* Oops, here we potentially divide by zero... CO 1995-02-09 */
	      
	    lmax = sqrt(lmax2);
	      
	    maxstepSize = lmax * 1.0;

	    if (fabs(step) > maxstepSize)
	      {
		/* now decrease the step and calculate the new track position */

		newstep = copysign(maxstepSize * 0.8,step);
	      }
	    else
	      {
		/* the step is small enough. No need to change it, exit the loop */

		break;
	      }

#ifdef UNDEF
	    if (state.flags & SWIM_DBG_TRACE)
	      fprintf(stderr,"trackswim_arc: deltab: %10.4lf, lmax: %10.4lf, step: %10.4lf ---> %10.4lf decreased\n",deltab,lmax,step,newl);
#endif
	    
#ifdef UNDEF
	    fprintf(stderr,"trackswim_arc: At z: %10.4lf: Max step: %10.4lf, Change step size from %10.4lf to %10.4lf (decrement)\n",vold.z,maxstepSize,step,newstep);
#endif
	      
	    step = newstep;

	    distNew = arcStep(&vnew,&state,step);

	  } /* end loop while (step size is bigger than maximum step size) */

#ifdef UNDEF
	fprintf(stderr,"trackswim_arc: At z: %10.4lf: Max step: %10.4lf,                  Step %10.4lf\n",vold.z,maxstepSize,step);
#endif

	/* add correction for non uniform field */
	
	if (haveB)
	{
	  float newbmag = v3mag(&bnew);
	  
	  float newr1 = k0 * newbmag;
	  
	  float rr = 1/state.r1;                /* radius in the previous point   */
	  float newr = 1/newr1;                 /* radius in the new point (vnew) */
	  float rdiff = rr - newr;
	  float rnew = (rr+newr)/2;             /* correction is done here!!!! in this line!!! */

	  /*float deltab = state.bmag - newbmag;*/
	  
/*fprintf(stderr,"deltaB: %10.3lf, r: %10.4lf, %10.4lf , rCorrection: %10.4lf\n",deltab,rr,newr,rdiff);*/

	  state.r1 = 1.0/rnew;

	  distNew = arcStep(&vnew,&state,step); /* recalculate the position of the new point */
	                                        /* note, that we neglect to recalculate the B field
						   in the corrected point (vnew)  */
	}
	    
	/*
	 * Update the position
	*/

	state.v = vnew;
	state.dist = distNew;
	state.bestdist = distNew;

	/*
	 * update the momentum vector 
	*/

	if (haveB) arcRotate(&state,step);

	/*
	 * update the traveled arc length. Do not bother updating the B field, checking step sizes, etc...
	*/

	s += fabs(step);

	if (s + fabs(step)>maxs) break;
	    
	/*
	 * update the B field.
	*/

	state.b = bnew;

	/*
	 * check if the step size can be increased
	*/

	if (fabs(step) < stepExt/2)
	  {
	    float newstep;
	    float newl = fabs(step) * 1.5;
	    
	    if (newl > stepExt) newl = stepExt;
	    
	    if (newl > maxstepSize) newl = maxstepSize * 0.8;

#ifdef UNDEF	    
	    if (state.flags & SWIM_DBG_TRACE)
	      fprintf(stderr,"trackswim_arc: deltab: %10.4lf, lmax: %10.4lf, step: %10.4lf ---> %10.4lf increased\n",deltab,maxstepSize,step,newl);
#endif
	    
	    newstep = copysign(newl,step);

#ifdef UNDEF	    
	    fprintf(stderr,"trackswim_arc: At z: %10.4lf: Max step: %10.4lf, Change step size from %10.4lf to %10.4lf (increment)\n",
		   vold.z,maxstepSize,step,newstep);
#endif
	    
	    step = newstep;
	  }
      }

      /*
       * Prepare for the next step
      */

      state.bmag = v3mag(&state.b);

      if (state.bmag > 0.0)
	{
	  haveB = 1;
	  state.bunit.x = state.b.x / state.bmag;
	  state.bunit.y = state.b.y / state.bmag;
	  state.bunit.z = state.b.z / state.bmag;
	}
      else
	{
	  haveB = 0;
	  state.bunit = v3zero;
	}
    }

  *ret_x  = state.v;
  *dx = state.dv;

  if (fabs(s)<maxs) return 0;
  else return 1;
}

/*
 **********************************************************************************
             E N D    O F    C I R C U L A R    S W I M M E R    C O D E
 **********************************************************************************
*/

static int trackswim_tracb(vector3_t *v,vector3_t *dv,float p,float q,
			   float max,float mindist,dist_f_t dist_f,const void*dist_par,
			   int flags)
{
  float pq = p * q;
  vector3_t vlim1 = { -10000,-10000,-10000};
  vector3_t vlim2 = { +10000,+10000,+10000};
  float arc = 0;

  float step = swim.step_tracb;

  const float *ppp = dist_par;
  float Dnorm;
  vector3_t unv;
  int ret;

  if (is_NaN(v->x)||is_NaN(v->y)||is_NaN(v->z))
    {
      fprintf(stderr,"trackswim_tracb: Error: One of v: %10.4lf %10.4lf %10.4lf is NaN\n",v->x,v->y,v->z);
      return -1;
    }

  if (is_NaN(dv->x)||is_NaN(dv->y)||is_NaN(dv->z))
    {
      fprintf(stderr,"trackswim_tracb: Error: One of dv: %10.4lf %10.4lf %10.4lf is NaN\n",dv->x,dv->y,dv->z);
      return -1;
    }

  if (ppp)
    {
      if (is_NaN(ppp[3]))
	{
	  unv.x = 0;
	  unv.y = 0;
	  unv.z = 1;
	  Dnorm = ppp[2];
	}
      else
	{
	  unv.x = ppp[0];
	  unv.y = ppp[1];
	  unv.z = ppp[2];
	  Dnorm = ppp[3];
	}
    }
  else
    {
      unv.x = 0;
      unv.y = 0;
      unv.z = 1;
      Dnorm = v->z + max;
    }

  if (Dnorm - (unv.x*v->x + unv.y*v->y + unv.z*v->z) < 0)
    {
      step = -step;
    }

  if (Dnorm < 0)
    {
      Dnorm =  - Dnorm;
      unv.x =  - unv.x;
      unv.y =  - unv.y;
      unv.z =  - unv.z;
    }

  dv->x *= p;
  dv->y *= p;
  dv->z *= p;

/*
  fprintf(stderr,"tracb call: v: %10.4lf %10.4lf %10.4lf, dv: %10.3lf %10.3lf %10.3lf\n",
	 v->x,v->y,v->z,dv->x,dv->y,dv->z);
*/
      
  ret = ptracb_(v,dv,&pq,&vlim1,&vlim2,&step,&Dnorm,&unv,&arc);

/*
  fprintf(stderr,"tracb ret.: v: %10.4lf %10.4lf %10.4lf, dv: %10.3lf %10.3lf %10.3lf\n",
	 v->x,v->y,v->z,dv->x,dv->y,dv->z);
*/
      
  dv->x /= p;
  dv->y /= p;
  dv->z /= p;
      
  return (ret!=0);
}

/*
 * x - starting and ending point
 * dx - momentum unit vector
 * p - momentum absolute value
 * q - charge
 * maxs - signed maximum arc length, positive value tells to swim forward,
 *                                   negative tells to swim backward
 * mindist   - how close to come to the boundary
 * dist_f    - boundary conditions: dist_f should be minimized along the track
 * dist_par  - parameters for dist_f
*/

int trackswim(vector3_t *x,vector3_t *dx,float p,float q,
	      float maxs,float mindist,dist_f_t dist_f,const void*dist_par,
	      int flags)
{
  int ret;
  if (!inited)
    {
      trackswim_init();
    }

  switch (swim.sw)
    {
    default:
    case 1: fprintf(stderr,"trackswim: Error: Attempt to call an inexistant swimming routine: %d\n",swim.sw); return -1;
    case 2: ret = trackswim_arc(x,dx,p,q,maxs,mindist,dist_f,dist_par,flags);   break;
    case 3: ret = trackswim_line(x,dx,p,q,maxs,mindist,dist_f,dist_par);        break;
    case 4: ret = trackswim_tracb(x,dx,p,q,maxs,mindist,dist_f,dist_par,flags); break;
    }

  if (flags&SWIM_DBG_TRACE) fprintf(stderr,"trackswim:%d: track at %10.4lf %10.4lf %10.4lf, ret: %d\n",
				   swim.sw,x->x,x->y,x->z,ret);

  return ret;
}

/*
 * here go higher level trk_swim... functions
*/

int trk3p_swimAway(vector3_t *v,vector3_t *punit,float qp,float dist,int flags)
{
  int ret = 0;
  vector3_t vv;

  vv = *v;

  if ((qp==0)||(!field_isOn())) {

      /* this is code for magnet-off */

      v->x += punit->x * dist;
      v->y += punit->y * dist;
      v->z += punit->z * dist;
    }
  else {

      /* this is code for magnet-on */

      ret = trackswim(v,punit,1,qp,dist,0,NULL,NULL,flags);

      if (ret != 0)
	{
	  return -1;
	}
    }
      
  g3D_SetColor("green");
  g3D_Line(vv.x,vv.y,vv.z,v->x,v->y,v->z);
  g3D_flush();

  return 0;
}

int trk3_swimAway(vector3_t *v,vector3_t *p,int charge,float dist,int flags)
{
  int ret = 0;
  float pmag;
  vector3_t dv;

  pmag = v3mag(p);

  dv.x = p->x/pmag;
  dv.y = p->y/pmag;
  dv.z = p->z/pmag;

  ret = trk3p_swimAway(v,&dv,charge/pmag,dist,flags);

  p->x = dv.x * pmag;
  p->y = dv.y * pmag;
  p->z = dv.z * pmag;

  return ret;
}

int trk_swimAway(double trk[],double dist)
{
  int ret;
  vector3_t v,p;
  float qp;

  if (!inited) trackswim_init();

  v.x = trk[0];
  v.y = trk[1];
  v.z = trk[2];
  qp  = trk[3];
  p.x = trk[4];
  p.y = trk[5];
  p.z = trk[6];

  ret = trk3p_swimAway(&v,&p,qp,dist,swim.flags);

  trk[0] = v.x;
  trk[1] = v.y;
  trk[2] = v.z;
  trk[3] = qp;
  trk[4] = p.x;
  trk[5] = p.y;
  trk[6] = p.z;

  return ret;
}

int trk3p_swimToPlane_boff(vector3_t *v,vector3_t *p,float qp,
			   const char planeName[],
			   const vector3_t *dnorm,float dpos,
			   const trk_planeLimits_t *box,
			   int flags)
{
  int ret = 0;
  float vn,pn,pos;
  
  vn = v->x*dnorm->x + v->y*dnorm->y + v->z*dnorm->z;
  pn = p->x*dnorm->x + p->y*dnorm->y + p->z*dnorm->z;

  pos = (dpos - vn)/pn;

  v->x = v->x + pos * p->x;
  v->y = v->y + pos * p->y;
  v->z = v->z + pos * p->z;

  return 0;
}

static float distToPlane(const vector3_t*v,const void *vpar)
{
  const float *par = vpar;
  float ref = par[0]*v->x + par[1]*v->y + par[2]*v->z;
  float dist = SQR(par[3] - ref);
  return dist;
}

int trk3p_swimToPlane_bon(vector3_t *v,vector3_t *p,float qp,
			  const char planeName[],
			  const vector3_t *dnorm,float dpos,
			  const trk_planeLimits_t *box,
			  int flags)
{
  int ret = 0;
  double best_dist = HUGE_VAL;
  double maxs;
  static float par[4];
  int icount = 50;
  vector3_t vv;

  if (swim.flags & SWIM_F_DBG_SPLANE)
    {
      swim.flags |= SWIM_F_DBG_SWIMMER|SWIM_F_DBG_PLANE;
    }

  vv = *v;

  par[0] = dnorm->x;
  par[1] = dnorm->y;
  par[2] = dnorm->z;
  par[3] = dpos;

  if (flags & SWIM_DBG_TRACE)
    fprintf(stderr,"swim to plane: %s\n",planeName);

  while (--icount > 0)
    {
      /* cpos = current position (perpendicular to the plane) */
      double cpos = dnorm->x*v->x + dnorm->y*v->y + dnorm->z*v->z;
      double ppos = dnorm->x*(*p).x + dnorm->y*(*p).y + dnorm->z*(*p).z;
      double uppos = fabs(ppos);
      double dist = (dpos - cpos);
      double udist = fabs(dist);

      if (uppos < 0.0001)
	{
	  if (flags != 0)
	    fprintf(stderr,"trk_swimToPlane: %s\t %10.4lf %10.4lf %10.4lf, dist: %10.4lf, parallel to plane, giving up\n",
		    planeName,v->x,v->y,v->z,dist);

	  ret = (-1);
	  break;
	}

      if (is_NaN(cpos)||is_NaN(dpos))
	{
	  fprintf(stderr,"trk_swimToPlane: %s\t %10.4lf %10.4lf %10.4lf, dist: %10.4lf, cpos=%10.4lf or dpos=%10.4lf are NaN, giving up\n",
		   planeName,v->x,v->y,v->z,dist,cpos,dpos);
	  ret = (-1);
	  break;
	}

      if (udist > best_dist)
	{
	  if (flags != 0)
	    {
	      fflush(stdout);
	      fprintf(stderr,"trk_swimToPlane: %s: \tDoes not converge: Dist: %10.4lf, track: position: %10.4lf %10.4lf %10.4lf, pmag: %10.3lf, punit: %10.3lf %10.3lf %10.3lf\n",
		      planeName,dist,v->x,v->y,v->z,1/qp,(*p).x,(*p).y,(*p).z);
	    }

	  ret = (-1);
	  break;
	}

      best_dist = udist;
      if (udist < swim.plane_max_dist) break;

      maxs = 2.0 * dist / ppos;

      maxs += copysign(0.1,maxs); /* make sure that 'maxs' does not become zero */

      if (fabs(maxs) > swim.maxSwim)
	maxs = copysign(swim.maxSwim,maxs);

      if (flags & SWIM_DBG_TRACE)
	fprintf(stderr,"swim to plane: %s\t %10.4lf %10.4lf %10.4lf, dist: %10.4lf, maxs: %10.4lf\n",
	       planeName,v->x,v->y,v->z,dist,maxs);

      ret = trackswim(v,p,1,qp,maxs,swim.plane_max_dist,distToPlane,&par[0],flags);

      if (ret<0) break;
      ret = 0;

#ifdef UNDEF

      if (box)
	{

	double r = v->x*box->ux[0]+v->y*box->ux[1]+v->z*box->ux[2];
	
	if (r < box->xmin) ret = 1;
	if (r > box->xmax) ret = 1;

	r = v->x*box->uy[0]+v->y*box->uy[1]+v->z*box->uy[2];

	if (r < box->ymin) ret = 1;
	if (r > box->ymax) ret = 1;

	if (ret)
	  {
	    if (flags != 0)
	      {
		fflush(stdout);
		fprintf(stderr,"trk_swimToPlane: %s: \tTrack out of limits: Starting %10.4lf %10.4lf %10.4lf, ending %10.4lf %10.4lf %10.4lf\n",
			planeName,vv.x,vv.y,vv.z,v.x,v->y,v->z);
	      }
	    break;
	  }
      }
#endif

      {
	if ((v->x < swim.min_x)||(v->x > swim.max_x)
	  ||(v->y < swim.min_y)||(v->y > swim.max_y)
	  ||(v->z < swim.min_z)||(v->z > swim.max_z))
	  {
/*
  fflush(stdout);
	    fprintf(stderr,"trk_swimToPlane: %s: \tTrack out of limits: Starting %10.4lf %10.4lf %10.4lf, ending %10.4lf %10.4lf %10.4lf\n",
		    planeName,vv.x,vv.y,vv.z,v->x,v->y,v->z);
*/
	    ret = (-1);
	    break;
	  }
      }

    } /* end while */

  if (icount < 1)
    {
      fprintf(stderr,"trk_swimToPlane: %s: Aborted by the guard counter at %10.4lf %10.4lf %10.4lf\n",
	      planeName,v->x,v->y,v->z);
      ret = (-1);
    }

  g3D_SetColor("green");
  g3D_Line(vv.x,vv.y,vv.z,v->x,v->y,v->z);
  g3D_flush();

  if (swim.flags & SWIM_F_DBG_SPLANE)
    {
      swim.flags &= ~(SWIM_F_DBG_SWIMMER|SWIM_F_DBG_PLANE);
    }

  return ret;
}

int trk3p_swimToPlane(vector3_t *v,vector3_t *punit,float qp,
		      const char planeName[],
		      const vector3_t *dnorm,float dpos,
		      const trk_planeLimits_t *box,
		      int flags)
{
  if (!inited) trackswim_init();

  if (field_isOn()&&(qp!=0))
    return trk3p_swimToPlane_bon(v,punit,qp,planeName,dnorm,dpos,box,flags);
  else
    return trk3p_swimToPlane_boff(v,punit,qp,planeName,dnorm,dpos,box,flags);
}

int trk3_swimToPlane(vector3_t *v,vector3_t *p,int charge,const char planeName[],
		     const vector3_t *dnorm,float dpos,
		     const trk_planeLimits_t *box,
		     int flags)
{
  float qp;
  float pmag;
  int ret;
  vector3_t dv;

  if (charge==0)
    {
      qp = 0;
      pmag = 1;

      dv = *p;
    }
  else
    {
      pmag = v3mag(p);
      qp = charge/pmag;

      dv.x = p->x/pmag;
      dv.y = p->y/pmag;
      dv.z = p->z/pmag;
    }

  ret = trk3p_swimToPlane(v,&dv,qp,planeName,dnorm,dpos,box,flags);

  p->x = dv.x * pmag;
  p->y = dv.y * pmag;
  p->z = dv.z * pmag;

  return ret;
}


int trk_swimToPlane(double trk[7],const char planeName[],
		    const double dnorm[3],double dpos,
		    const trk_planeLimits_t *box)
{
  int ret;
  vector3_t v,p,d3;
  float qp;

  if (!inited) trackswim_init();

  d3.x = dnorm[0]; d3.y = dnorm[1]; d3.z = dnorm[2];

  v.x = trk[0];
  v.y = trk[1];
  v.z = trk[2];
  qp  = trk[3];
  p.x = trk[4];
  p.y = trk[5];
  p.z = trk[6];

  ret = trk3p_swimToPlane(&v,&p,qp,planeName,&d3,dpos,box,swim.flags);

  trk[0] = v.x;
  trk[1] = v.y;
  trk[2] = v.z;
  trk[3] = qp;
  trk[4] = p.x;
  trk[5] = p.y;
  trk[6] = p.z;

  return ret;
}

static float distToPoint(const vector3_t*v,const void *vpar)
{
  const float *par = vpar;
/*
  float dist = sqrt(SQR(par[0] - v->x) + SQR(par[1] - v->y) + SQR(par[2] - v->z));
*/
  float dist = (SQR(par[0] - v->x) + SQR(par[1] - v->y) + SQR(par[2] - v->z));
/*
  if (swim.flags & SWIM_F_DBG_SPOINT)
    fprintf(stderr,"point: %10.4lf %10.4lf %10.4lf, dist: %10.4lf\n",
	   v->x,v->y,v->z,dist);
*/
  return dist;
}

int trk3p_swimToV_bon(vector3_t*v,vector3_t*p,float qp,const vector3_t *sv,int flags)
{
  int ret;
  double s;
  vector3_t d;
  double maxs;
  double newdist,bestdist;
  float par[4];
  int icount = 50;

  if (swim.flags & SWIM_F_DBG_SPOINT)
    {
      swim.flags |= SWIM_F_DBG_SWIMMER|SWIM_F_DBG_POINT;
    }

  if (flags&SWIM_DBG_TRACE)
    {
      fprintf(stderr,"trk3_swimToV_bon: track before: %10.4lf %10.4lf %10.4lf\n",v->x,v->y,v->z);
    }

  par[0] = sv->x;
  par[1] = sv->y;
  par[2] = sv->z;
  par[3] = -HUGE_VAL;

  bestdist = HUGE_VAL;

  if (swim.flags & SWIM_F_DBG_POINT)
    fprintf(stderr,"swim to V: %10.4lf %10.4lf %10.4lf\n",sv->x,sv->y,sv->z);

  while (--icount > 0)
    {
      d.x = sv->x - v->x;
      d.y = sv->y - v->y;
      d.z = sv->z - v->z;

      newdist = sqrt(SQR(d.x) + SQR(d.y) + SQR(d.z));

      if (bestdist - newdist < swim.point_max_dist) break;
      if (newdist >= bestdist)
	{
	  if (flags != 0)
	    {
	      fflush(stdout);
	      fprintf(stderr,"trk_swimToV: %10.4lf %10.4lf %10.4lf: Does not converge: old: %10.4lf, new: %10.4lf, track: position: %10.4lf %10.4lf %10.4lf, pmag: %10.3lf, punit: %10.3lf %10.3lf %10.3lf\n",
		      sv->x,sv->y,sv->z,bestdist,newdist,v->x,v->y,v->z,1/qp,(*p).x,(*p).y,(*p).z);
	    }

	  break;
	}
      
      s = d.x * (*p).x + d.y * (*p).y + d.z * (*p).z;

      bestdist = newdist;

      maxs = 2*s;

      maxs += copysign(0.1,maxs); /* make sure that 'maxs' does not become zero */

      if (fabs(maxs) < newdist) maxs = copysign(newdist,maxs);
      if (fabs(maxs) > swim.maxSwim) maxs = copysign(swim.maxSwim,maxs);

      if (swim.flags & SWIM_F_DBG_POINT)
	fprintf(stderr,"swim to V: %10.4lf %10.4lf %10.4lf,  maxs: %10.4lf, dist: %10.4lf\n",v->x,v->y,v->z,maxs,newdist);

      ret = trackswim(v,&(*p),1,qp,maxs,swim.point_max_dist/2.0,distToPoint,par,flags);

      if (ret<0) break;
    }

  if (icount < 1)
    {
      fprintf(stderr,"trk_swimToV: %10.4lf %10.4lf %10.4lf: Aborted by the guard counter at %10.4lf %10.4lf %10.4lf\n",
	      sv->x,sv->y,sv->z,v->x,v->y,v->z);
      ret = (-1);
    }

  if (flags&SWIM_DBG_TRACE)
    {
      fprintf(stderr,"trk3_swimToV_bon: track  after: %10.4lf %10.4lf %10.4lf\n",v->x,v->y,v->z);
    }

  if (swim.flags & SWIM_F_DBG_SPOINT)
    {
      swim.flags &= ~(SWIM_F_DBG_SWIMMER|SWIM_F_DBG_POINT);
    }

  return 0;
}

int trk3p_swimToV_boff(vector3_t *v,vector3_t *p,float qp,const vector3_t *u,int flags)
{
  vector3_t uv;
  float puv,pp,pos;
  
  if (flags&SWIM_DBG_TRACE)
    {
      fprintf(stderr,"trk3_swimToV_boff: track before: %10.4lf %10.4lf %10.4lf\n",v->x,v->y,v->z);
    }
  uv.x = u->x - v->x;
  uv.y = u->y - v->y;
  uv.z = u->z - v->z;

  puv = p->x*uv.x + p->y*uv.y + p->z*uv.z;
  pp = p->x*p->x + p->y*p->y + p->z*p->z;

  pos = puv/pp;

  v->x = v->x + pos * p->x;
  v->y = v->y + pos * p->y;
  v->z = v->z + pos * p->z;

  if (flags&SWIM_DBG_TRACE)
    {
      fprintf(stderr,"trk3_swimToV_boff: track  after: %10.4lf %10.4lf %10.4lf\n",v->x,v->y,v->z);
    }

  return 0;
}

int trk3p_swimToV(vector3_t* v,vector3_t* p,float qp,const vector3_t* sv,int flags)
{
  if (!inited) trackswim_init();

  if (field_isOn() && (qp!=0))
    return trk3p_swimToV_bon(v,p,qp,sv,flags);
  else
    return trk3p_swimToV_boff(v,p,qp,sv,flags);
}

int trk3_swimToV(vector3_t* v,vector3_t* p,int charge,const vector3_t* sv,int flags)
{
  float qp;
  float pmag;
  int ret;
  vector3_t dv;

  if (charge==0)
    {
      qp = 0;
      pmag = 1;

      dv = *p;
    }
  else
    {
      pmag = v3mag(p);
      qp = charge/pmag;

      dv.x = p->x/pmag;
      dv.y = p->y/pmag;
      dv.z = p->z/pmag;
    }

  ret = trk3p_swimToV(v,&dv,qp,sv,flags);

  p->x = dv.x * pmag;
  p->y = dv.y * pmag;
  p->z = dv.z * pmag;

  return ret;
}

int trk_swimToV(double trk[7],const double u[3])
{
  int ret;
  vector3_t v,p;
  float qp;
  vector3_t sv;
  
  v.x = trk[0];
  v.y = trk[1];
  v.z = trk[2];
  qp  = trk[3];
  p.x = trk[4];
  p.y = trk[5];
  p.z = trk[6];

  sv.x = u[0]; sv.y = u[1]; sv.z = u[2];

  ret = trk3p_swimToV(&v,&p,qp,&sv,swim.flags);

  trk[0] = v.x;
  trk[1] = v.y;
  trk[2] = v.z;
  trk[3] = qp;
  trk[4] = p.x;
  trk[5] = p.y;
  trk[6] = p.z;

  return ret;
}


/*
 *
 * Code for swimming to Cylinders
 *
*/

static float distToCyl(const vector3_t*v,const void *vpar)
{
  const float *par = vpar;
  float dist;
  vector3_t dist3;
  vector3_t perp3;
  float proj;
  float r;

  dist3.x = v->x - par[0];
  dist3.y = v->y - par[1];
  dist3.z = v->z - par[2];

  proj = dist3.x*par[3] + dist3.y*par[4] + dist3.z*par[5];

  perp3.x = dist3.x - proj*par[3];
  perp3.y = dist3.y - proj*par[4];
  perp3.z = dist3.z - proj*par[5];

  r = SQR(perp3.x) + SQR(perp3.y) + SQR(perp3.z);

  dist = SQR(par[6] - r);


  return dist;
}

int trk3p_swimToCyl_bon(vector3_t* v,vector3_t* p,float qp,
			const vector3_t* center,const vector3_t *axis,float radius,float length,
			int flags)
{
  int ret = 0;
  double s;
  vector3_t d;
  double maxs;
  double bestdist;
  float par[3+3+1];
  int icount = 50;
  float axisLength = v3mag(axis);
  float length2 = length/2;

  if (!inited) trackswim_init();

  if (swim.flags & SWIM_F_DBG_SPOINT)
    {
      swim.flags |= SWIM_F_DBG_SWIMMER|SWIM_F_DBG_POINT;
    }

  if (flags&SWIM_DBG_TRACE)
    {
      fprintf(stderr,"trk3_swimToCyl_bon: track before: %10.4lf %10.4lf %10.4lf\n",v->x,v->y,v->z);
    }

  if (flags&SWIM_DBG_TRACE)
    fprintf(stderr,"swim to Cyl: %10.4lf %10.4lf %10.4lf, axis: %10.4lf %10.4lf %10.4lf, radius: %10.4lf\n",
	   center->x,center->y,center->z,
	   axis->x,axis->y,axis->z,
	   radius);

  /*
   * Before we swim to the cylinder, we make sure we intersect with it.
  */

  {
    /* find out if we are inside or outside of the cylinder on the Z axis */

    vector3_t dist3,perp3;
    float proj,r;

    dist3.x = v->x - center->x;
    dist3.y = v->y - center->y;
    dist3.z = v->z - center->z;
    
    proj = dist3.x*axis->x + dist3.y*axis->y + dist3.z*axis->z;

    if (fabs(proj/axisLength) >= length2 - 0.0100)   /* we are outside of the cylinder in Z */
      {
	float dpos;
	float cosa;
	int ret;

	if (proj > 0)
	  dpos = DotProduct3(center,axis) + length2; /* swim to the positive edge of the CYL */
	else
	  dpos = DotProduct3(center,axis) - length2; /* swim to the negative edge of the CYL */

	if (flags&SWIM_DBG_TRACE)
	  fprintf(stderr,"swimming from: %10.4lf %10.4lf %10.4lf, r: %10.4lf, to: %10.4lf, radius: %10.4lf\n",
		 v->x,v->y,v->z,r,dpos,radius);

	ret = trk3p_swimToPlane(v,p,qp,"TCYLbbox",axis,dpos,NULL,0);

	if (flags&SWIM_DBG_TRACE)
	  fprintf(stderr,"result: %10.4lf %10.4lf %10.4lf, %d\n",v->x,v->y,v->z,ret);

	if (ret)
	  {
	    if (flags&SWIM_DBG_TRACE)
	      fprintf(stderr,"swim to Cyl: Failed to swim to the CYL end.\n");

	    return -1;
	  }

	dist3.x = v->x - center->x;
	dist3.y = v->y - center->y;
	dist3.z = v->z - center->z;
	
	proj = dist3.x*axis->x + dist3.y*axis->y + dist3.z*axis->z;
	
	perp3.x = dist3.x - proj*axis->x;
	perp3.y = dist3.y - proj*axis->y;
	perp3.z = dist3.z - proj*axis->z;
	
	r = sqrt(SQR(perp3.x) + SQR(perp3.y) + SQR(perp3.z));

	cosa = DotProduct3(&perp3,p);

	if (r < radius)
	  {
	    /* we are inside the cylinder in R */
	    
	    if (cosa * proj >= 0)
	      {
		if (flags&SWIM_DBG_TRACE)
		  fprintf(stderr,"swim to Cyl: No intersection with the cylinder, r: %10.4lf, cosa: %lf\n",r,cosa);
		
		return -1;
	      }
	  }
	else
	  {
	    /* we are outside the cylinder in R */
	    
	    if (cosa * proj <= 0)
	      {
		if (flags&SWIM_DBG_TRACE)
		  fprintf(stderr,"swim to Cyl: No intersection with the cylinder, r: %10.4lf, cosa: %lf\n",r,cosa);
		
		return -1;
	      }
	  }
      }
  }

  par[0] = center->x;
  par[1] = center->y;
  par[2] = center->z;
  par[3] = axis->x;
  par[4] = axis->y;
  par[5] = axis->z;
  par[6] = SQR(radius);

  bestdist = HUGE_VAL;

  while (--icount > 0)
    {
      float saveStep;
      vector3_t dist3,perp3;
      float proj,r;
      float ppos,uppos;
      float newdist;
      float unewdist;
      float umaxs;

      dist3.x = v->x - center->x;
      dist3.y = v->y - center->y;
      dist3.z = v->z - center->z;
      
      proj = dist3.x*axis->x + dist3.y*axis->y + dist3.z*axis->z;

      perp3.x = dist3.x - proj*axis->x;
      perp3.y = dist3.y - proj*axis->y;
      perp3.z = dist3.z - proj*axis->z;
      
      r = sqrt(SQR(perp3.x) + SQR(perp3.y) + SQR(perp3.z));

      newdist  = r - radius;
      unewdist = fabs(newdist);

      if (fabs(proj/axisLength) > length2 + 0.0100)   /* if we are outside of the cylinder in Z */
	{
	  if (flags&SWIM_DBG_TRACE)
	    fprintf(stderr,"swim to Cyl: Went out of the cylinder: proj: %lf, axisLength: %lf, length2: %lf\n",
		   proj,axisLength,length2);

	  return -1;
	}

      if (unewdist < swim.point_max_dist)
	{
	  /* success- we are at the right radius */

	  ret = 0;
	  break;
	}

      if (unewdist >= bestdist)
	{
	  if (flags&SWIM_DBG_TRACE)
	    {
	      fflush(stdout);
	      fprintf(stderr,"trk_swimToCyl: Does not converge: distance- old: %10.4lf, new: %10.4lf, track: position: %10.4lf %10.4lf %10.4lf, pmag: %10.3lf, punit: %10.3lf %10.3lf %10.3lf\n",
		      bestdist,newdist,v->x,v->y,v->z,1/qp,(*p).x,(*p).y,(*p).z);
	    }

	  return -1;
	}

      ppos = (perp3.x * (*p).x + perp3.y * (*p).y + perp3.z * (*p).z)/r;

      uppos = fabs(ppos);

      if (uppos < 0.0001)
	{
	  if (flags&SWIM_DBG_TRACE)
	    fprintf(stderr,"trk3p_swimToCyl: %10.4lf %10.4lf %10.4lf, rdist: %10.4lf, parallel to plane, giving up\n",
		    v->x,v->y,v->z,newdist);

	  ret = (-1);
	  break;
	}

      maxs  = - 1.5 * newdist/ppos;
      umaxs = fabs(maxs);

      if (flags&SWIM_F_FORWARD) /* we should move only FORWARD along the trajectory */
	if (maxs <= 0)          /* but the trajectory goes away from the cylinder   */
	  {
	    float skip;

	    if (newdist >= 0)
	      {
		if (flags&SWIM_DBG_TRACE)
		  fprintf(stderr,"trk3p_swimToCyl: %10.4lf %10.4lf %10.4lf, FORWARD: no intersection, dist: %lf, ppos: %lf\n",
			  v->x,v->y,v->z,newdist,ppos);

		ret = (-1);
		break;
	      }

	    skip = 0.5 + fabs(DotProduct3(p,&dist3));

	    if (flags&SWIM_DBG_TRACE)
	      fprintf(stderr,"trk3p_swimToCyl: %10.4lf %10.4lf %10.4lf, FORWARD: dist: %lf, ppos: %lf, skip: %10.4lf\n",
			  v->x,v->y,v->z,newdist,ppos,skip);

	    trk3p_swimAway(v,p,qp,skip,0);

	    continue;
	  }
	  
      if (flags&SWIM_F_BACKWARD) /* we should move only BACKWARD along the trajectory */
	if (maxs >= 0)           /* but the trajectory goes away from the cylinder   */
	  {
	    float skip;

	    if (newdist >= 0)
	      {
		if (flags&SWIM_DBG_TRACE)
		  fprintf(stderr,"trk3p_swimToCyl: %10.4lf %10.4lf %10.4lf, BACKWARD: no intersection, dist: %lf, ppos: %lf\n",
			  v->x,v->y,v->z,newdist,ppos);

		ret = (-1);
		break;
	      }

	    skip = 0.5 + fabs(DotProduct3(p,&dist3));

	    if (flags&SWIM_DBG_TRACE)
	      fprintf(stderr,"trk3p_swimToCyl: %10.4lf %10.4lf %10.4lf, BACKWARD: dist: %lf, ppos: %lf, skip: %10.4lf\n",
			  v->x,v->y,v->z,newdist,ppos,skip);

	    trk3p_swimAway(v,p,qp,-skip,0);

	    continue;
	  }
	  
      bestdist = unewdist;

      if (umaxs < unewdist)
	maxs = copysign(unewdist,maxs);

      if (umaxs > swim.maxSwim)
	maxs = copysign(swim.maxSwim,maxs);

      saveStep = swim.step_arc;

      if (swim.step_arc > umaxs)  /* make sure the steps are not TOO large */
	swim.step_arc = umaxs*0.50;

      if (swim.step_arc > radius * 0.5)
	swim.step_arc = radius * 0.25;

      if (flags & SWIM_DBG_TRACE)
	fprintf(stderr,"swim to Cyl: %10.4lf %10.4lf %10.4lf,  maxs: %10.4lf, dist: %10.4lf, ppos: %lf\n",v->x,v->y,v->z,maxs,newdist,ppos);

      ret = trackswim(v,p,1,qp,maxs,swim.point_max_dist/2.0,distToCyl,par,flags);

      swim.step_arc = saveStep;

      if (ret<0)
	{
	  fprintf(stderr,"trk_swimToCyl: trackswim() returned %d. Track at: %10.4lf %10.4lf %10.4lf.\n",ret,v->x,v->y,v->z);
	  return -1;
	}

      if (is_NaN(v->z))
	{
	  fprintf(stderr,"trk_swimToCyl: v->z is NaN. Track at: %10.4lf %10.4lf %10.4lf.\n",v->x,v->y,v->z);
	  return -1;
	}
    }

  if (icount < 1)
    {
      fprintf(stderr,"trk_swimToCyl: Aborted by the guard counter at %10.4lf %10.4lf %10.4lf\n",v->x,v->y,v->z);
      ret = (-1);
    }

  if (flags&SWIM_DBG_TRACE)
    {
      fprintf(stderr,"trk3_swimToCyl_bon: track  after: %10.4lf %10.4lf %10.4lf, icount: %d\n",v->x,v->y,v->z,icount);
    }

  if (swim.flags & SWIM_F_DBG_SPOINT)
    {
      swim.flags &= ~(SWIM_F_DBG_SWIMMER|SWIM_F_DBG_POINT);
    }

  return ret;
}

int trk3p_swimToCyl_boff(vector3_t* v,vector3_t* p,float qp,
			 const vector3_t* center,const vector3_t *axis,float radius,float length,
			 int flags)
{

  return trk3p_swimToCyl_bon(v,p,qp,center,axis,radius,length,flags);
}

int trk3p_swimToCyl(vector3_t* v,vector3_t* p,float qp,
		    const vector3_t* center,const vector3_t *axis,float radius,float length,
		    int flags)
{
  if (field_isOn() && (qp!=0))
    return trk3p_swimToCyl_bon(v,p,qp,center,axis,radius,length,flags);
  else
    return trk3p_swimToCyl_boff(v,p,qp,center,axis,radius,length,flags);
}

int trk3_swimToCyl(vector3_t* v,vector3_t* p,int charge,
		   const vector3_t* center,const vector3_t *axis,float radius,float length,
		   int flags)
{
  float qp;
  float pmag;
  int ret;
  vector3_t dv;

  if (charge==0)
    {
      qp = 0;
      pmag = 1;

      dv = *p;
    }
  else
    {
      pmag = v3mag(p);
      qp = charge/pmag;

      dv.x = p->x/pmag;
      dv.y = p->y/pmag;
      dv.z = p->z/pmag;
    }

  ret = trk3p_swimToCyl(v,&dv,qp,center,axis,radius,length,flags);

  p->x = dv.x * pmag;
  p->y = dv.y * pmag;
  p->z = dv.z * pmag;

  return ret;
}

/* end file */
