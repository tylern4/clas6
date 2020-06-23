/*
 * Create TAGM bank.
 * 
 * The TAGM bank is similar to the TAGR bank. It contains E-T coincidences 
 * formed from geometric matching as well as timing. The difference is that
 * the TAGM bank gets all it's T-counter info from the 1877 Multi-hit TDCs
 * whose data is stored in the TAGE bank. This allows T-counter rates to
 * be calculated using many more statistics than the what is possible from
 * the 1872 single hit, high resolution TDC info stored in the TAGT banks.
 * 
 * This code was derived from original code written for PrimEx which was
 * be unable to use the CLAS tagger code. Consequently, the coincidence 
 * matching is independant from that done by the standard CLAS tagger
 * package. 
 *
 * This should be called from tagM_evt().
*/

#include <tagM.h>





/* Find all ET coincidences via geometry and timing */
int tagM_GetETCoincidences(int n_e_channel_hits,
									TAG_HIT_t* e_channel_hits,
									int n_t_channel_hits,
									TAG_HIT_t* t_channel_hits,
									TAG_ET_COINCIDENCE_t* et_coincidences,
									float time_offset)
{

	int i,j;
	int n=0;
	int eid;
	int TCHANmin,TCHANmax;
	static int nerrs=0;

	/* Look for coincidences between E and T channels */
	for(i=0;i<n_e_channel_hits;i++){

		/* The file tagETcoinc_ALL.dat correlates E-counter(1-384) with T-channel(1-121)
		 * We therefore need to convert E-channels back into E-counters and consider the
		 * T-channels which can overlap with one or both E-counters in that E-channel.
		 */
		eid = (e_channel_hits[i].id+1)/2; /* gives n for both eid=2n and eid=2n-1 */
		TCHANmin = tagETgeom[eid].TCHANmin;
		if(e_channel_hits[i].id%2)
			TCHANmax = tagETgeom[eid-1+1].TCHANmax; /* E-channel is overlap of two E-counters */
		else
			TCHANmax = tagETgeom[eid-1].TCHANmax; /* E-channel is from single E-counter */
			
		
		for(j=0;j<n_t_channel_hits;j++){

			/* Check ET Geometry */
			if(t_channel_hits[j].id<TCHANmin || t_channel_hits[j].id>TCHANmax)continue;

			/* Check ET Timing */
			if(fabs(e_channel_hits[i].t - t_channel_hits[j].t + time_offset)<ET_TIME_WINDOW){
				et_coincidences[n].tid = t_channel_hits[j].id;
				et_coincidences[n].t   = t_channel_hits[j].t;
				et_coincidences[n].eid = e_channel_hits[i].id;
				et_coincidences[n].e_t = e_channel_hits[i].t;
				et_coincidences[n].energy = tagE_boundaries[et_coincidences[n].eid-1].kavg*TAGM_EBEAM_ENERGY;
				et_coincidences[n].status = TAGM_ET;
				n++;
			}
			if(n>=MAX_ETHITS){
				if(nerrs++<100)fprintf(stderr,"%s:%d Maximum number of ET coincidences found.\n",__FILE__,__LINE__);
				return n;
			}
		}
	}

	
	return n;
}


