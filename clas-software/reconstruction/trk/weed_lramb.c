/*
Author: David Ciarletta

Purpose: weed_lramb compares the successful fits for a segments 
         in a specific track & SL and "weeds" out 
	 fits which are very similar to another fit found with a better chi^2
	
Inputs: trk - track # segment is in
	sl - SL the segement is in
	
Modification: Jorn Langheinrich
              s.known_amb[k][s.nknown[k]] : range check for s.nknown[k]
	      replaced for (j=..) loop with while (j<..) loop 
	         (index j is modified within the loop)

Outputs: none
*/


#include <ntypes.h>
#include <bostypes.h>
#include <trk_common.h>
#include <math.h>
#include "tbt.h"

#define s seg[trk-1][sl-1]

void weed_lramb(int trk, int sl) {
  int i,j,k,l;
  int flg[6];
  int tflgs;
  
  for(i=0;i<s.nfits-1;i++) {
    j = i+1;
    while (j<s.nfits) {
      tflgs = 0;
      for(k=0;k<6;k++) {
	if((s.fit[k][i].wire <= 0 && s.fit[k][j].wire <= 0)
	   || (s.fit[k][i].wire == s.fit[k][j].wire
	       && (s.fit[k][i].doca == s.fit[k][j].doca 
		   || fabs(s.fit[k][j].doca/s.fit[k][j].sigma)<=2))
	   || ((s.fit[k][i].wire == s.fit[k][j].wire+1)
	       && (s.fit[k][i].doca < 0) && (s.fit[k][j].doca > 0))
	   || ((s.fit[k][j].wire == s.fit[k][i].wire+1)
	       && (s.fit[k][j].doca < 0) && (s.fit[k][i].doca > 0))) {
	  flg[k] = 0;
	}
	else {
	  flg[k] = 1;
	}
	tflgs += flg[k];
	  
	/* store known ambiguities (ex. two hits in a layer) */
	if(s.fit[k][i].wire == s.fit[k][j].wire+1 && 
	   s.fit[k][i].doca < 0 && s.fit[k][j].doca > 0) {
	  if (s.nknown[k] + 2 < MAX_FITS*5) {
	    s.known_amb[k][s.nknown[k]] = -s.fit[k][i].wire;
	    s.nknown[k]++;
	    s.known_amb[k][s.nknown[k]] = s.fit[k][j].wire;
	    s.nknown[k]++;
	  }
	}
	else if(s.fit[k][i].wire == s.fit[k][j].wire-1 && 
		s.fit[k][i].doca > 0 && s.fit[k][j].doca < 0) {
	  if (s.nknown[k] + 2 < MAX_FITS*5) {
	    s.known_amb[k][s.nknown[k]] = s.fit[k][i].wire;
	    s.nknown[k]++;
	    s.known_amb[k][s.nknown[k]] = -s.fit[k][j].wire;
	    s.nknown[k]++;
	  }
	}
      }
      
      if(tflgs == 0) {
	if(fabs(s.chi2[i]) <= fabs(s.chi2[j])) {
	  for(k=j;k<s.nfits-1;k++) {
	    for(l=0;l<6;l++) {
	      s.fit[l][k] = s.fit[l][k+1];
	    }
	    s.chi2[k] = s.chi2[k+1];
	  }
	  s.nfits--;
	  j--;
	}
	else {
	  for(k=i;k<s.nfits-1;k++) {
	    for(l=0;l<6;l++) {
	      s.fit[l][k] = s.fit[l][k+1];
	    }
	    s.chi2[k] = s.chi2[k+1];
	  }
	  s.nfits--;
	  j=i;
	}
      }
      j++;
    }
  }

  return;
}
