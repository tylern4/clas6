/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include "rtpc.h"

/* stuff for calling fortran routines and linking cernlib */
#define  f2cFortran 
//inline float SQR(float x) { return x*x; }

float distance_between_points (float* v1, float* v2) {
//  return sqrt ( SQR(v1[0]-v2[0]) + SQR(v1[1]-v2[1]) + SQR(v1[2]-v2[2])  );
  return sqrt ( (v1[0]-v2[0])*(v1[0]-v2[0]) + (v1[1]-v2[1])*(v1[1]-v2[1]) + (v1[2]-v2[2])*(v1[2]-v2[2])  );
}


void  VSUB(float* v1, float* v2, float* diff, int n) {
  vsub_(v1, v2, diff, &n);
}

float  VMOD(float* v1, int n) {
  return vmod_(v1, &n);
}


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
int chain_link()
{
     /* find tracks by stringing together links of hits that are close */

  /*
  Hits are stored in a structure that can be used to point into rawCYL:		
  hh_num_hits = number of hits in the RTPC for this event
  chain_hits[chain no.][hit# on chain] contains pointers to the hits in hh_hitlist on each chain
  num_chains = number of chaings found
  num_hits_this_chain[chain no.] = number of hits on each chain
  int   hh_hitlist[hh_num_hits].pad= pad_num -- the pad number (0-3199)
  int   hh_hitlist[hh_num_hits].t  = tic     -- the time of the hit (0-99)
  int   hh_hitlist[hh_num_hits].status       -- the unused/used status of this hit. Initially 0.
                                                HREMOVD means it is an outlier (nkb 11-15-05)
                                                HDISEAR means early
						HDISLAT means late
  float hh_hitlist[hh_num_hits].z            -- z position of the pad center (mm)
  float hh_hitlist[hh_num_hits].r            -- radial position of the hit (mm)
  float hh_hitlist[hh_num_hits].phi          -- phi of the hit (after Lorentz correction)
  float hh_hitlist[hh_num_hits].q            -- signal pulse height for this hit
  */
/* ################################################################ */

  /* variables that should be global */


  /*local variables... */
  int ii,jj;
  int anchor_hit, seed_hit, next_hit, seed_index;
  float pseed[3], pnext[3]; /* place to put position 3-vectors of points */
  /*float diff[3];  to store vector from seed point to next point */
  float separation;
  int chain_debug=FALSE;
  char title[80]; /* histogram titles, etc. */

  /* initialize some stuff */
  /* all hits are marked as unused when data is first unpacked*/
  anchor_hit= 0;
  num_chains=0;
  num_hits_this_chain[0]= 0;

  /* Print out what we started with */
  if(chain_debug)
    {
      printf("***************************** \n");
      printf(" Entering chain_link with %d hits \n",hh_num_hits);
      printf(" HitNum   Pad  Time Sta     r      z     phi      q\n");
      for(jj=0; jj<hh_num_hits; jj++)
	{
	  printf(" %6d %5d %5d %3d %6.1f %6.1f %6.3f %6.0f \n",
		 jj,
		 hh_hitlist[jj].pad,
		 hh_hitlist[jj].t,
		 hh_hitlist[jj].status,
		 hh_hitlist[jj].r,
		 hh_hitlist[jj].z,
		 hh_hitlist[jj].phi,
		 hh_hitlist[jj].q   );
	}
    }

/* Pick the next hit on the list to try as a new anchor */

  for(anchor_hit=0; anchor_hit < hh_num_hits; anchor_hit++)
    {
      if (hh_hitlist[anchor_hit].status == HUNUSED)
	{
	  if( num_chains >= MAX_NUM_CHAINS)
	    {
	      printf(">>> too many chains!. aborting.\n");
	      return -1;
	    }
	  num_hits_this_chain[num_chains]=1;
	  chain_hits[num_chains][0]= anchor_hit;
	  if(chain_debug) printf("Trying new chain (number %d) anchored at hit %d\n",num_chains,anchor_hit);
	  hh_hitlist[anchor_hit].status=HISUSED; /* at least temporarily, mark the anchor hit as used */
	  for(seed_hit=0; seed_hit < num_hits_this_chain[num_chains];seed_hit++)
	    {
	      if(chain_debug) printf(" using seed hit %d\n",chain_hits[num_chains][seed_hit]);
	      /* get (x,y,z) of the seed hit*/
	      seed_index= chain_hits[num_chains][seed_hit];
	      pseed[0]= hh_hitlist[seed_index].r * cos(hh_hitlist[seed_index].phi);
	      pseed[1]= hh_hitlist[seed_index].r * sin(hh_hitlist[seed_index].phi);
	      pseed[2]= hh_hitlist[seed_index].z;

	      /* check this seed hit against ALL unused hits */
	      for(next_hit= 0 ; next_hit < hh_num_hits; next_hit++)
		{
		  if ( (hh_hitlist[next_hit].status == HUNUSED) &&              /* if hit unused */
		       ( (hh_hitlist[next_hit].pad   -1600)*
			 (hh_hitlist[seed_index].pad -1600) >=0 ))       /* same side of RTPC */
		    {
		      /* get (x,y,z) of the next hit */
		      pnext[0]= hh_hitlist[next_hit].r * cos(hh_hitlist[next_hit].phi);
		      pnext[1]= hh_hitlist[next_hit].r * sin(hh_hitlist[next_hit].phi);
		      pnext[2]= hh_hitlist[next_hit].z;

		      /* get the vector from seed to next */

		      /* using our own function now
		      VSUB(pseed,pnext,diff,3);
		      separation= VMOD(diff,3);
		      */
		      
		      /* distance between points */
		      separation = distance_between_points(pseed,pnext);

		      if (GEMDEBUG) {
			fprintf (stdout, "V6 separation = %9.2f  {%6.2f,%6.2f,%6.2f}  {%6.2f,%6.2f,%6.2f}\n", 
				 separation, pnext[0], pnext[1], pnext[2], pseed[0], pseed[1], pseed[2] );
			fflush(stdout);
		      }

		      if(separation < MAX_LINK_SEP)
			{
			  /* add it to the current chain */
			  if(chain_debug) printf("...adding hit %d to this chain\n",next_hit);
			  if (num_hits_this_chain[num_chains] >= MAX_HITS_ON_CHAIN)
			    {
			      //printf("Too many hits for the chain list. Aborting.\n");
			      return -1;
			    }
			  chain_hits[num_chains][num_hits_this_chain[num_chains]]=next_hit;
			  /* mark it as used */
			  hh_hitlist[next_hit].status=HISUSED;
			  num_hits_this_chain[num_chains]++;
			}
		    }
		}
	    }
	  if( num_hits_this_chain[num_chains] > 1) 
	    {
	      if(chain_debug)
		{
		  printf("....KEEPING THIS CHAIN. \n");
		  for (jj=0; jj<num_hits_this_chain[num_chains]; jj++)
		    printf(" %d", chain_hits[num_chains][jj]);
		  printf("\n\n");
		}
	      num_chains++; /* save chain if >1 hit on it */
	    }
	  else
	    {hh_hitlist[anchor_hit].status=HUNUSED;} /* no chain -> release the anchor hit */
	}
    }
  if(GEMDEBUG)
    {
      /* print out a summary of found chains */
      for (ii=0; ii<num_chains; ii++)
	{
	  printf("\n");
	  printf("Hits used on chain %d (%d):",ii,num_hits_this_chain[ii]);
	  for (jj=0; jj<num_hits_this_chain[ii]; jj++)
	    printf(" %d", chain_hits[ii][jj]);
	}
      printf("\n");
     
    }
  return num_chains;
}
