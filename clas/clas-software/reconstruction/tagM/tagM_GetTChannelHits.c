/*
 * Deterimine Tagger T-counter fine channel hits
 *
 *
 *
*/
#include <tagM.h>


/*
 * Here we want to loop over the T-counter hits in order to find the T-channel hits.
 * This is done similar to what is done for the E-counters (see comment at top of
 * tagM_GetEChannelHits() ). 
 */


/* Used for sorting. Defined below. */
int tagM_TChannelHitCompare(TAG_HIT_t *hit1, TAG_HIT_t *hit2);



int tagM_GetTChannelHits(int n_t_hits, TAG_HIT_t* t_hits, TAG_HIT_t* t_channel_hits)
{
	int i,j,k;
	int n=0;
	int single_hit;
	int used[MAX_THITS];
	int err=0;

	/* Reset "used" flags */
	for(i=0;i<n_t_hits;i++)used[i]=0;

	/* First, loop over T-counter hits to look for coincidences with adjacent T-counters */
	for(k=0;k<n_t_hits;k++){

		if(used[k])continue;	/* use each hit only once */
		
		/* The counter hits are already sorted by channel id. Look for an adjacent channel */
		for(j=k+1;j<n_t_hits;j++){
			if(t_hits[j].id == t_hits[k].id+1){
				if(fabs(t_hits[j].t-t_hits[k].t)<TT_TIME_WINDOW){

					/* This hit corresponds to one in the adjacent detector */
					t_channel_hits[n].id = 2*t_hits[k].id;
					t_channel_hits[n].t  = t_hits[k].t; /* use time of first hit determined by geometry */
					n++;
					used[k]++;	/* mark both hits as used */
					used[j]++;
					break;
				}
			}else{ /* j loop  */
				if(t_hits[j].id >  t_hits[k].id+1)break; /* quit loop as soon as we can */
			}
		}
	}

	/* Now, loop over T-counter hits and copy any unused ones into the t_channels_hits */
	for(k=0;k<n_t_hits;k++){

		if(used[k])continue;	/* use each hit only once */

		t_channel_hits[n].id = 2*t_hits[k].id - 1;
		t_channel_hits[n].t  = t_hits[k].t;
		n++;
		used[k]++;
	}

	/* perform a little error check to make sure nothing's screwed up */
	for(i=0;i<n_t_hits;i++)if(used[i] != 1)err=1;
	if(err)fprintf(stderr,"%s:%d Bug detected. Complain to davidl@jlab.org\n",__FILE__,__LINE__);

	/* Sort entries by channel, then time */
	qsort(t_hits,n,sizeof(TAG_HIT_t),(QSORT_PROC_t*)tagM_TChannelHitCompare);
	
	return n;
}


int tagM_TChannelHitCompare(TAG_HIT_t *hit1, TAG_HIT_t *hit2)
{
	static int n_errs=0;
	
	/* Sort by id first */
	if(hit1->id != hit2->id)return hit1->id - hit2->id;

	/* Sort by time second */
	if(hit1->t != hit2->t)return (int)(2.0*(hit1->t - hit2->t));

	/* Should never get here, but just in case ... */
	if(n_errs++<100)fprintf(stderr,"%s:%d Identical TChannel hits!\n",__FILE__,__LINE__);
	return 0;
}

