/*
 * Read Tagger E-counter TDC info from TAGE bank
 *
 *
 *
*/
#include <tagM.h>


/* Used for sorting. Defined below. */
int tagM_ECounterHitCompare(TAG_HIT_t *hit1, TAG_HIT_t *hit2);


int tagM_GetECounterHits(TAG_HIT_t* e_hits)
{
	int i,j;
	int id, tdc;
	float t;
	int n=0;
	static int n_errs=0;
	clasTAGE_t *TAGE=getBank(&bcs_, "TAGE");
	clasHEAD_t* HEAD=getBank(&bcs_, "HEAD");

	if(!TAGE)return n;

	for(i=0;i<TAGE->bank.nrow;i++){
		id  = TAGE->tage[i].id;
		if(id<1 || id>384)continue;		/* make sure id corresponds to an E-counter	*/
		e_hits[n].status = 0;
		tdc = TAGE->tage[i].tdc;
		t   = EID2NS*(float)tdc;			/* convert to nanoseconds							*/
		t  += EID_TIME_OFFSETS[id-1];		/* align with other E-counters					*/
		t  += E2T_TIME;						/* align with T-counters							*/
		e_hits[n].id  = id;
		e_hits[n].tdc = tdc;
		e_hits[n++].t = t;	/* nanoseconds  */
		if(n>=MAX_EHITS){
			fprintf(stderr,"%s:%d Too many rows in TAGE bank. Dropping some.\n",__FILE__,__LINE__);
			break;
		}
	}


	/* Sort the hits by counter id, then time. This makes it easier to change these into fine channels later */
	qsort(e_hits,n,sizeof(TAG_HIT_t),(QSORT_PROC_t*)tagM_ECounterHitCompare);


	/* For unknown reasons, there occasionally exists 2 identical rows in the TAGE bank.
	 * Lacking a better approach for now, I discard one to prevent double counting.
	 * This is done after sorting since it places identical hits next to each other
	 * in the list making it quicker and easier to filter.
	 */
	for(i=0;i<n-1;i++){
		if(e_hits[i].id == e_hits[i+1].id){
			if(e_hits[i].tdc == e_hits[i+1].tdc){
				if(n_errs++<20){
					fprintf(stderr,"%s:%d Filtering duplicate hits in TAGE. evt# %d\n"
						,__FILE__,__LINE__,HEAD ? HEAD->head[0].nevent:-1);
				}
				for(j=i+1;j<n-1;j++)e_hits[j]=e_hits[j+1];
				n--;
			}
		}
	}

	return n;
}



int tagM_ECounterHitCompare(TAG_HIT_t *hit1, TAG_HIT_t *hit2)
{
	clasHEAD_t* HEAD;
	
	/* Sort by id first */
	if(hit1->id != hit2->id)return hit1->id - hit2->id;

	/* Sort by time second */
	if(hit1->t != hit2->t)return (int)(2.0*(hit1->t - hit2->t));

	/* Really should never get here, but we do due to what's described in comment above. */
	return 0;
}


