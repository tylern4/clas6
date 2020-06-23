/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include "rtpc.h"

/* stuff for calling fortran routines and linking cernlib */
#define  f2cFortran 

/////////////////////DOCUMENTATION/////////////////////////////
//AUTHOR: Nate Baillie 11-15-05
//this routine will take a list of hits on a chain and charge weight
//those points which occur at the same time and are on neighboring
//pads.  The data points which meet this requirement will be 
//coalesced into a single point with weight = the average charge 
//in all the points

//the status of the weighted data point will be HISUSED if it's alone,
//                                              HUNUSED if it hasn't been looked at
//                                              HABSORB if it has been absorbed already

//inputs:  The original raw hit list and it's length
//outputs: A new hit list which has converged the appropriate data
//         points and weighted them accordingly
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
int weightData(int chainnum)
{
float chargeSum, numAvgPhi, numAvgz, numAvgRad;
 int combine, jj, ii, kk, npt;
 hitlist temp[HH_MAX_NUM_HITS];
 int addedHits;
 int store_hit_location[HH_MAX_NUM_HITS];
 int _wdebug = FALSE;
 float phi_dist; //this will be distance in phi between data points, if this is below 
                 //WW_PHI_CUT at virtually the same radius
 float rad_dist; //this will be distance in radius between data points, if this is below 
                 //WW_RAD_CUT the data will be coalesced
 float z_dist;
 npt=0;

 if(_wdebug) printf("starting weightdata with chainnum = %d, hh_num_hits = %d, num_hits_this_chain = %d\n",
		    chainnum, hh_num_hits, num_hits_this_chain[chainnum]);

 if(num_chains+1 > MAX_NUM_CHAINS)
   {
     //printf("not enough room to add more chains, exiting weightdata\n");
     return (chainnum);
   }
 if(hh_num_hits+1 > HH_MAX_NUM_HITS)
   {
     //printf("not enough room to add more hits to hitlist, exiting weightdata\n");
     return (chainnum);
   }

 for (jj=0; jj< num_hits_this_chain[chainnum]; jj++) //fills the weighted data array with the
                                                     //coordinates of the given chain
    {
      if(hh_hitlist[chain_hits[chainnum][jj]].status & HISUSED)
	{
	  temp[npt].r = hh_hitlist[chain_hits[chainnum][jj]].r;
	  temp[npt].phi = hh_hitlist[chain_hits[chainnum][jj]].phi;
	  temp[npt].z = hh_hitlist[chain_hits[chainnum][jj]].z;
	  temp[npt].q = hh_hitlist[chain_hits[chainnum][jj]].q;
	  temp[npt].pad = hh_hitlist[chain_hits[chainnum][jj]].pad;
	  store_hit_location[npt] = chain_hits[chainnum][jj];
	  temp[npt].status = HUNUSED;
	  if(_wdebug) printf("added point %d(%d) to templist r=%.1f, phi=%.3f, z=%.1f, charge = %.1f\n",
			     chain_hits[chainnum][jj], npt,temp[npt].r, temp[npt].phi, temp[npt].z, temp[npt].q);
	  npt++;
	}
    }

 addedHits = FALSE;
 num_hits_this_chain[num_chains] = 0;
 kk=0;

 for (jj=0; jj<npt; jj++)
   {
     combine = FALSE;
     if(temp[jj].status == HUNUSED)
       {
	 chargeSum = temp[jj].q;
	 numAvgPhi = temp[jj].q*temp[jj].phi;
	 numAvgz = temp[jj].q*temp[jj].z;
	 numAvgRad = temp[jj].q*temp[jj].r;
       
	 for (ii = jj+1; ii<npt; ii++)
	   {
	     if(temp[ii].status == HUNUSED)
	       {
		 phi_dist = fabs(temp[jj].phi - temp[ii].phi);
		 rad_dist = fabs(temp[jj].r - temp[ii].r);
		 z_dist = fabs(temp[jj].z - temp[ii].z);
		 if(phi_dist < WW_PHI_CUT && 
		    rad_dist < WW_RAD_CUT && 
		    z_dist < WW_Z_CUT &&
		    temp[ii].pad != temp[jj].pad )
		   {
		     chargeSum += temp[ii].q;
		     numAvgPhi += temp[ii].q*temp[ii].phi;
		     numAvgRad += temp[ii].q*temp[ii].r;
		     numAvgz += temp[ii].q*temp[ii].z;
		     temp[ii].status |= HABSORB;
		     temp[jj].status |= HABSORB;
		     hh_hitlist[store_hit_location[ii]].status = HUNUSED;//mark the old points as unused
		     hh_hitlist[store_hit_location[jj]].status = HUNUSED;//since they will be combined
		     if(_wdebug) printf("added point %d to point %d \n", ii, jj);
		     if(_wdebug) printf("delrad, delphi, delz = %f %f %f\n", 
					rad_dist, phi_dist, z_dist);
		     combine = TRUE;
		     addedHits = TRUE;
		   }//end of logic to combine points, 
	       }//end of check to see if this point has been checked yet
	   }//end of loop over all points ahead of current point

	 if(combine) //add a new point to the hitlist and add this point to a new chain
                     //also increment the hitlist count and the num_hits_this_chain counter
	   {
	     hh_hitlist[hh_num_hits].phi = numAvgPhi/chargeSum;
	     hh_hitlist[hh_num_hits].z = numAvgz/chargeSum;
	     hh_hitlist[hh_num_hits].r = numAvgRad/chargeSum;
	     //if (!quiet) printf("raw[%d][%d].s = %f", t, 5, raw[t][5].s);
	     hh_hitlist[hh_num_hits].q = chargeSum;
	     hh_hitlist[hh_num_hits].status = HISUSED;
	     chain_hits[num_chains][kk] = hh_num_hits;
	     num_hits_this_chain[num_chains]++;
	     if(num_hits_this_chain[num_chains] > MAX_HITS_ON_CHAIN)
	       {
		 printf("new chain has too many links, exiting weightdata\n");
		 return (chainnum); //this tells the program to ignore the new chain
	       }
	     if(_wdebug) printf("point %d added to new_chainlist(pts = %d) r=%.1f, phi=%.3f, z=%.1f, charge = %.1f\n",
				hh_num_hits, num_hits_this_chain[num_chains], 
				hh_hitlist[hh_num_hits].r, 
				hh_hitlist[hh_num_hits].phi, 
				hh_hitlist[hh_num_hits].z, 
				hh_hitlist[hh_num_hits].q);
	     hh_num_hits++;
	     if(hh_num_hits > HH_MAX_NUM_HITS)
	       {
		 printf("not enough room to add more hits to hitlist, exiting weightdata\n");
		 hh_num_hits--;
		 return (chainnum);//this tells the program to ignore the new chain
	       }
	     kk++;
	   }//add new point to hit list AND include this point in the new chainList
	 else
	   {
	     temp[jj].status = HISUSED;
	     if(_wdebug && store_hit_location[jj] == 0)
	       {
		 printf("adding %d to new chain at %d\n",store_hit_location[jj],jj);
	       }
	     chain_hits[num_chains][kk] = store_hit_location[jj];
	     num_hits_this_chain[num_chains]++;
	     kk++;
	     if(_wdebug) printf("point %d kept on new_chainlist(pts = %d) r=%.1f, phi=%.3f, z=%.1f, charge = %.1f\n",
				store_hit_location[jj], num_hits_this_chain[num_chains], 
				hh_hitlist[store_hit_location[jj]].r, 
				hh_hitlist[store_hit_location[jj]].phi, 
				hh_hitlist[store_hit_location[jj]].z, 
				hh_hitlist[store_hit_location[jj]].q);
	   }//end of add old point to new chain list
       }//end of check to see if this point has been scanned yet
   }//end of loop over all points in the chain

 if(_wdebug)
   {
     for(ii = 0; ii<num_hits_this_chain[num_chains]; ii++)
       {
	 printf("chain %d point %d = %d\n", num_chains, ii, chain_hits[num_chains][ii]);
       }
   }

 if(addedHits)
   { //add the new chain to the list only if we had to combine points!
     num_chains++;
     return(num_chains - 1); //tells fitter which chain to fit right now
   }
 return (chainnum);
}
