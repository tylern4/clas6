/*-------------------sc_makeSCRC.c-------------------*/
/* Routines for creating the scintillator cluster bank SCRC.  Input: SCR
   ROUTINES:
     make_SCRC_bank:   Loops over the hits in SCR looking for adacent paddles 
                       that fire in a given event.  This routine determines 
 		       whether these pairs should be combined into a single 
                       hit based on the uncertainties in the individual times
                       and positions relative to the differences in times 
                       and positions for the adjacent hits.
     scr2scrc:         Copies SCR values over to SCRC for single hits
     get_cluster:      Combines adjacent hits to form clusters.
     get_cluster_unc:  Calculates the uncertainty in an energy-weighted
                       quantity
*/
/*-------------------------------------------------*/
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ntypes.h>
#include <math.h>
#include <bostypes.h>
#include <sc.h>
#include <scExtern.h>
#include <kinematics.h>
#include <utility.h>
/* -=======================================================- *
$Id: sc_makeSCRC.c,v 1.6 2008/03/16 04:33:22 fklein Exp $
$Author: fklein $
$Revision: 1.6 $
$Date: 2008/03/16 04:33:22 $
* -=======================================================- */


clasSCRC_t *make_SCRC_bank(int sector){
  clasSCR_t *SCR=NULL;
  clasSCRC_t *SCRC=NULL;
  int nSCRC=0;
  scrc_t clusters[SC_NPADDLES_SEC_TOTAL];
  int i=0,j,k,matched=FALSE;
  float tdif,tdif2,ydif2,ydif,tunc=1.0,yunc=20.0;
  float tunc2=1.0,yunc2=20.0;
  
    if((SCR=getGroup(&bcs_,"SCR ",sector))!=NULL){
      for(i=0;i<SCR->bank.nrow;i++){
	if(matched==TRUE) matched=FALSE;
        else{    
	  j=i+1;
	  if(j>=SCR->bank.nrow) j=i;
          if(SCR->scr[j].id!=SCR->scr[i].id+1){
	    clusters[nSCRC]=scr2scrc(SCR->scr[i]);
            matched=FALSE;
            nSCRC++;
          }
	  else{
          /* Check to see that the hits match in time and space */
            tdif=fabs(SCR->scr[i].time-SCR->scr[j].time);
	    tunc=3*sqrt(SCR->scr[i].dtime*SCR->scr[i].dtime
		      + SCR->scr[j].dtime*SCR->scr[j].dtime); 
            ydif=fabs(SCR->scr[i].pos.y-SCR->scr[j].pos.y);
            yunc=3*sqrt(SCR->scr[i].err.y*SCR->scr[i].err.y
		      + SCR->scr[j].err.y*SCR->scr[j].err.y); 
	    if(tdif>tunc||ydif>yunc){ /*No match in space or no match in time*/
	      clusters[nSCRC]=scr2scrc(SCR->scr[i]);
              matched=FALSE;
              nSCRC++;
    	    }
            else{ /* Matched in space and time */
	      /* Check for three contiguous hits */
              k=j+1;
	      if (k>=SCR->bank.nrow) k=j; 
	      if (SCR->scr[k].id==SCR->scr[j].id+1){
		tdif2=fabs(SCR->scr[k].time-SCR->scr[j].time);
		tunc2=3*sqrt(SCR->scr[k].dtime*SCR->scr[k].dtime
			   + SCR->scr[j].dtime*SCR->scr[j].dtime); 
		ydif2=fabs(SCR->scr[k].pos.y-SCR->scr[j].pos.y);
		yunc2=3*sqrt(SCR->scr[k].err.y*SCR->scr[k].err.y
			   + SCR->scr[j].err.y*SCR->scr[j].err.y);
		if (ydif2>yunc2||tdif2>tunc2||tunc2>tunc||yunc2>yunc){
		  /*First pair better match */
		  clusters[nSCRC]=get_cluster(SCR->scr[i],SCR->scr[i+1]);
		  matched=TRUE;
		  nSCRC++;
		}
		else{ /* Second pair better match */
		  clusters[nSCRC]=scr2scrc(SCR->scr[i]);
		  matched=FALSE;
		  nSCRC++;	 
		}
	      }
	      else{ /* No 3rd counter in group; combine counters 1 and 2 */
		clusters[nSCRC]=get_cluster(SCR->scr[i],SCR->scr[i+1]);
                matched=TRUE;
                nSCRC++;
              }
            }
          }
        }
      }
    }
  if (nSCRC){ /* If we have any entries, make the SCRC bank */
    if(SCRC=makeBank(&bcs_,"SCRC",sector,sizeof(scrc_t)/4,nSCRC)){
      for(k=0;k<nSCRC;k++) SCRC->scrc[k]=clusters[k];
    }
  }
  return SCRC;
}
/*-------------------------------------------------------------------------*/
/* SUPPLEMENTARY ROUTINES                                                  */
/*-------------------------------------------------------------------------*/
/* scr2scrc:  Copies data from the SCR bank to the SCRX bank for the case 
   where asingle counter is the "cluster" */
scrc_t scr2scrc(scr_t scr){
 scrc_t scrc;
  
 scrc.status=scr.status;
 scrc.id=scr.id;
 scrc.energy=scr.energy;
 scrc.denergy=scr.denergy;
 scrc.time=scr.time;
 scrc.dtime=scr.dtime;
 scrc.pos = scr.pos;
 scrc.err=scr.err;

 return scrc;
}

/* get_cluster:  combines data from adjacent counters; time and y-quantities 
   are weighted by energy deposition */
scrc_t get_cluster(scr_t one, scr_t two){
  scrc_t cluster;
  float ew1,ew2;  /* Energy weights of the two counters */
  float energy_sum;

  energy_sum=one.energy+two.energy;
  if (energy_sum>0.0)
    ew1=one.energy/(one.energy+two.energy);
  else
    ew1=0.5;
  ew2=1.0-ew1;
  cluster.status=one.status+100*two.status;
  cluster.id=one.id;
  cluster.energy=one.energy + two.energy; 
  cluster.denergy=sqrt(one.denergy*one.denergy + two.denergy*two.denergy); 
  cluster.time=ew1*one.time + ew2*two.time;
  cluster.dtime=get_cluster_unc(one.energy,one.denergy,one.time,one.dtime,
				two.energy,two.denergy,two.time,two.dtime);
  cluster.pos.y=ew1*one.pos.y+ew2*two.pos.y; 
  cluster.pos.x=(one.pos.x+two.pos.x)/2;
  cluster.pos.z=(one.pos.z+two.pos.z)/2;
  cluster.err.x=sqrt(one.err.x*one.err.x+two.err.x*two.err.x)/2;
  cluster.err.y=get_cluster_unc(one.energy,one.denergy,one.pos.y,one.err.y,
			     two.energy,two.denergy,two.pos.y,two.err.y);
  cluster.err.z=sqrt(one.err.z*one.err.z+two.err.z*two.err.z)/2;
 
  return cluster;
}


/* get_cluster_unc:  Get the uncertainty on an energy-weighted quantity q */
float get_cluster_unc(float E1,float dE1,float q1,float dq1,float E2,float dE2,
		      float q2,float dq2){
  float dqdq1,dqdq2,dqdE1,dqdE2;
  float energy_sum;

  energy_sum=E1+E2;
  if (energy_sum>0.0)
    { dqdq1=E1/(E1+E2);
      dqdq2=E2/(E1+E2);
      dqdE1=q1*E2/((E1+E2)*(E1+E2));
      dqdE2=q2*E1/((E1+E2)*(E1+E2));
    }
  else
    { dqdq1=0.5;
      dqdq2=0.5;
      dqdE1=q1*0.5;
      dqdE2=q2*0.5;
    }

  return(sqrt(dqdq1*dqdq1*dq1*dq1 + dqdq2*dqdq2*dq2*dq2 + dqdE1*dqdE1*dE1*dE1
	      + dqdE2*dqdE2*dE2*dE2));
}

