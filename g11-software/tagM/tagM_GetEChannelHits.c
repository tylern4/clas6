/*
 * Deterimine Tagger E-counter fine channel hits
 *
 *
 *
*/
#include <tagM.h>

/*
 * Here we want to loop over the E-counter hits in order to find the E-channel hits.
 * This is a little tricky in that we want each E-counter hit to contribute to exactly
 * one E-channel hit. Since corresponding E-counter hits do not neccessarily have to
 * reside next to each other in the e_hits[] array, we assign a flag to each hit to
 * mark it as having been used or not.
 */


/* Used for sorting. Defined below. */
int tagM_EChannelHitCompare(TAG_HIT_t *hit1, TAG_HIT_t *hit2);



int tagM_GetEChannelHits(int n_e_hits, TAG_HIT_t* e_hits, TAG_HIT_t* e_channel_hits)
{
	int i,j,k;
	int n=0;
	int single_hit;
	int used[MAX_EHITS];
	int err=0;

	/* Reset "used" flags */
	for(i=0;i<n_e_hits;i++)used[i]=0;

	/* First, loop over E-counter hits to look for coincidences with adjacent E-counters */
	for(k=0;k<n_e_hits;k++){

		if(used[k])continue;	/* use each hit only once */
		
		/* The counter hits are already sorted by channel id. Look for an adjacent channel */
		for(j=k+1;j<n_e_hits;j++){

			if(used[j])continue;	/* use each hit only once */
			
			if(e_hits[j].id == e_hits[k].id+1){
				if(fabs(e_hits[j].t-e_hits[k].t)<EE_TIME_WINDOW){

					/* This hit corresponds to one in the adjacent detector */
					e_channel_hits[n].id = 2*e_hits[k].id;
					e_channel_hits[n].t  = (e_hits[k].t+e_hits[j].t)/2.0; /* use average time */
					n++;
					used[k]++;	/* mark both hits as used */
					used[j]++;
					break;
				}
			}else{
				if(e_hits[j].id >  e_hits[k].id+1)break; /* quit loop as soon as we can */
			}
		}
	}

	/* Now, loop over E-counter hits and copy any unused ones into the e_channels_hits */
	for(k=0;k<n_e_hits;k++){

		if(used[k])continue;	/* use each hit only once */

		e_channel_hits[n].id = 2*e_hits[k].id - 1;
		e_channel_hits[n].t  = e_hits[k].t;
		n++;
		used[k]++;
	}

	/* perform a little error check to make sure nothing's screwed up */
	err = 0;
	for(i=0;i<n_e_hits;i++)if(used[i] != 1)err=1;
	if(err)fprintf(stderr,"%s:%d Bug detected. Complain to davidl@jlab.org\n",__FILE__,__LINE__);

	/* Sort entries by channel, then time */
	qsort(e_hits,n,sizeof(TAG_HIT_t),(QSORT_PROC_t*)tagM_EChannelHitCompare);
	
	return n;
}


int tagM_EChannelHitCompare(TAG_HIT_t *hit1, TAG_HIT_t *hit2)
{
	static int n_errs=0;
	
	/* Sort by id first */
	if(hit1->id != hit2->id)return hit1->id - hit2->id;

	/* Sort by time second */
	if(hit1->t != hit2->t)return (int)(2.0*(hit1->t - hit2->t));

	/* Should never get here, but just in case ... */
	if(n_errs++<100)fprintf(stderr,"%s:%d Identical EChannel hits!\n",__FILE__,__LINE__);
	return 0;
}

