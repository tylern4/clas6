/* make_mvrt.c */
/* Author: John McNabb */
/* Purpose: makes mvrt bank from TBER bank */
/* Inputs: TBER bank */
/* Outputs: MVRT bank */

/*_begin_doc
 *  RCS ID string
 *  $Id: make_mvrt.c,v 1.10 2008/03/16 04:33:29 fklein Exp $*/
#define USE(var) static void * use_##var = (void *) &var
static char crcsid[] =
"$Id: make_mvrt.c,v 1.10 2008/03/16 04:33:29 fklein Exp $";
USE(crcsid);   /* make sure it is not optimized away */
/* END of RCS information*/

#include <stdio.h>
#include <ntypes.h>
#include <bostypes.h>
#include <bosddl.h>
#include <vertex.h>
#include <math.h>
#include <kinematics.h>

extern double target_z();

void make_mvrt_()
{
    make_mvrt();
}

void make_mvrt()
{
    clasTBER_t *TBER = NULL;       /*structures from bostypes.h*/
    clasMVRT_t *MVRT = NULL;       /*structures from bostypes.h*/
    int ntrk;                      /* number of tracks*/
    int trklist[10]={1,2,3,4,5,6,7,8,9,10}; /*track array*/
    vector3_t vert;                /*vertex*/
    float Cmat[9];                 /*Covariance matrix*/
    float chi2;                    /*chi2*/
    int iter;                      /*iteration needed to converge*/
    int dof;                       /* degrees of freedom*/
    int vcol = sizeof(mvrt_t)/4;
    int i,vid=0;
    line_t line1={0,0,0,0,1,0}; /*line structures defined in vertex.h*/
    /* note that the direction is a unit vector*/
    line_t zbeam={0,0,0,0,0,1}; /*line defining the beam location*/
    extern vector3_t beam_position;
    vector3_t temp;
    float d,R,x,y,z,cx,cy,cz,z0,d0,lambda,phi,sector,alpha;
    static float Piby3;
    int layinfo2;


    if(MVRT = (clasMVRT_t *)makeBank(&bcs_, "MVRT",1, vcol, 1))
    {
        if(TBER=(clasTBER_t *)getBank(&bcs_,"TBER"))
        {
            ntrk = TBER->bank.nrow;
            iter =0;
            if (ntrk>=2 && ntrk <=10 && (mtv(trklist,ntrk,&vert,Cmat,&chi2,&iter)==0) )
            {
                for(i=1;i<=ntrk;i++)
                {
                    /*all tracks in events are used*/
                    vid += pow(2,i);
                }
                MVRT->mvrt->v_id = vid;
                MVRT->mvrt->ntrk = ntrk;
                MVRT->mvrt->vert = vert;
                MVRT->mvrt->chi2 = chi2;
                MVRT->mvrt->cxx = Cmat[0];
                MVRT->mvrt->cxy = Cmat[1];
                MVRT->mvrt->cxz = Cmat[2];
                MVRT->mvrt->cyy = Cmat[4];
                MVRT->mvrt->cyz = Cmat[5];
                MVRT->mvrt->czz = Cmat[8];
                MVRT->mvrt->stat = 0; /*reserved*/
            }
            else
            {
                /*set vertex to closest approach of first track to beamline*/
                Piby3 = acos(-1)/3;
                lambda=TBER->tber->lambda;
                phi=TBER->tber->phi;
                if(fabs(phi)<1.e-8) phi= phi<0.0 ? -1.e-8 : 1.e-8;
                d0 = TBER->tber->d0;
                z0=TBER->tber->z0;
                layinfo2=TBER->tber->layinfo2;
                sector = layinfo2 / (256*256*256);
                alpha = Piby3*(sector -1);

                /*initialize track starting point*/
                vert.x=-z0*sin(alpha);
                vert.y= z0*cos(alpha);
                vert.z=-d0/sin(phi);
                /*initialize track direction*/
                cx=cos(lambda)*sin(phi)*cos(alpha)-sin(lambda)*sin(alpha);
                cy=cos(lambda)*sin(phi)*sin(alpha)+sin(lambda)*cos(alpha);
                cz=cos(lambda)*cos(phi);

                line1.point = vert;     /*set line1 to the first track */
                line1.dir.x = cx;
                line1.dir.y = cy;
                line1.dir.z = cz;
                /*beam still assumed to be traveling in +Z direction*/
                zbeam.point=beam_position;        /*get beam position*/
                R=v3dot(line1.dir,zbeam.dir);
                vertex_doca(&line1,&zbeam,&d,&R); /*get info of closest approach
                                    between line1 and zbeam */
                temp = v3sub(zbeam.point,line1.point);
                MVRT->mvrt->v_id = -1;
                MVRT->mvrt->ntrk = ntrk;
                MVRT->mvrt->vert=line1.point;
                MVRT->mvrt->chi2 = -1;
                MVRT->mvrt->cxx = temp.x*temp.x;
                MVRT->mvrt->cxy = 0;
                MVRT->mvrt->cxz = 0;
                MVRT->mvrt->cyy = temp.y*temp.y;
                MVRT->mvrt->cyz = 0;
                MVRT->mvrt->czz = temp.z*temp.z;
                MVRT->mvrt->stat = 0;/*reserved*/
            }
        }
        else
        {
            /*if no TBER, use beam position*/
            vert.x = beam_position.x;
            vert.y = beam_position.y;
            vert.z = target_z();

            MVRT->mvrt->v_id = -1;
            MVRT->mvrt->ntrk = 0;
            MVRT->mvrt->vert = vert;
            MVRT->mvrt->chi2 = -1;
            MVRT->mvrt->cxx = 0;
            MVRT->mvrt->cxy = 0;
            MVRT->mvrt->cxz = 0;
            MVRT->mvrt->cyy = 0;
            MVRT->mvrt->cyz = 0;
            MVRT->mvrt->czz = 0;
            MVRT->mvrt->stat = 0;/*reserved*/
        }
    }
    else
    {
        printf("error making MVRT bank");
    }
}

vector3_t get_1part_vert(int tberind){
  // this is just basically copied from john's code above for finding the
  // best vertex when there's only 1 track in tber, except this is meant to
  // find the best vertex using only 1 track specified in tber, regardless
  // of how many tracks there are
  clasTBER_t *TBER = NULL;       /*structures from bostypes.h*/
  vector3_t vert;                /*vertex*/
  line_t line1={0,0,0,0,1,0}; /*line structures defined in vertex.h*/
  /* note that the direction is a unit vector*/
  line_t zbeam={0,0,0,0,0,1}; /*line defining the beam location*/
  extern vector3_t beam_position;
  float d,R,x,y,z,cx,cy,cz,z0,d0,lambda,phi,sector,alpha;
  static float Piby3;
  int layinfo2;
  int ntber;

  vert.x = 0.0;
  vert.y = 0.0;
  vert.z = 0.0;

  /*set vertex to closest approach of first track to beamline*/
  if(TBER=(clasTBER_t *)getBank(&bcs_,"TBER")){
    ntber = TBER->bank.nrow;
    if(tberind <= ntber){
      Piby3 = acos(-1)/3;
      lambda=TBER->tber[tberind].lambda;
      phi=TBER->tber[tberind-1].phi;
      d0 = TBER->tber[tberind-1].d0;
      z0=TBER->tber[tberind-1].z0;
      layinfo2=TBER->tber[tberind-1].layinfo2;
      sector = layinfo2 / (256*256*256);
      alpha = Piby3*(sector -1);

      /*initialize track starting point*/
      vert.x=-z0*sin(alpha);
      vert.y= z0*cos(alpha);
      vert.z=-d0/sin(phi);
      /*initialize track direction*/
      cx=cos(lambda)*sin(phi)*cos(alpha)-sin(lambda)*sin(alpha);
      cy=cos(lambda)*sin(phi)*sin(alpha)+sin(lambda)*cos(alpha);
      cz=cos(lambda)*cos(phi);

      line1.point = vert;     /*set line1 to the first track */
      line1.dir.x = cx;
      line1.dir.y = cy;
      line1.dir.z = cz;
      /*beam still assumed to be traveling in +Z direction*/
      zbeam.point=beam_position;        /*get beam position*/
      R=v3dot(line1.dir,zbeam.dir);
      vertex_doca(&line1,&zbeam,&d,&R); /*get info of closest approach
                                          between line1 and zbeam */

      return line1.point;
    }
  }
  return vert;
}

vector3_t get_1part_vert_nobos(tber_t __tber[]){
  // this is just basically copied from john's code above for finding the
  // best vertex when there's only 1 track in tber, except this is meant to
  // find the best vertex using only 1 track specified in tber, regardless
  // of how many tracks there are
  vector3_t vert;                /*vertex*/
  line_t line1={0,0,0,0,1,0}; /*line structures defined in vertex.h*/
  /* note that the direction is a unit vector*/
  line_t zbeam={0,0,0,0,0,1}; /*line defining the beam location*/
  //extern vector3_t beam_position;
  vector3_t beam_position;
  beam_position.x = 0.0;
  beam_position.y = 0.0;
  beam_position.z = 0.0;
  float d,R,x,y,z,cx,cy,cz,z0,d0,lambda,phi,sector,alpha;
  static float Piby3;
  int layinfo2;

  vert.x = 0.0;
  vert.y = 0.0;
  vert.z = 0.0;

  /*set vertex to closest approach of first track to beamline*/
      Piby3 = acos(-1)/3;
      lambda=__tber[0].lambda;
      phi=__tber[0].phi;
      d0 = __tber[0].d0;
      z0=__tber[0].z0;
      layinfo2=__tber[0].layinfo2;
      sector = layinfo2 / (256*256*256);
      alpha = Piby3*(sector -1);

      /*initialize track starting point*/
      vert.x=-z0*sin(alpha);
      vert.y= z0*cos(alpha);
      vert.z=-d0/sin(phi);
      /*initialize track direction*/
      cx=cos(lambda)*sin(phi)*cos(alpha)-sin(lambda)*sin(alpha);
      cy=cos(lambda)*sin(phi)*sin(alpha)+sin(lambda)*cos(alpha);
      cz=cos(lambda)*cos(phi);

      line1.point = vert;     /*set line1 to the first track */
      line1.dir.x = cx;
      line1.dir.y = cy;
      line1.dir.z = cz;
      /*beam still assumed to be traveling in +Z direction*/
      zbeam.point=beam_position;        /*get beam position*/
      R=v3dot(line1.dir,zbeam.dir);
      vertex_doca(&line1,&zbeam,&d,&R); /*get info of closest approach
                                          between line1 and zbeam */

      return line1.point;
}


