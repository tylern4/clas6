/*
 * Read Tagger E-counter TDC info from TAGE bank
 *
 *
 *
*/
#include <tagM.h>


/* Used for sorting. Defined below. */
int tagM_TCounterHitCompare(TAG_HIT_t *hit1, TAG_HIT_t *hit2);


int tagM_GetTCounterHits(TAG_HIT_t* t_hits)
{
	int i;
	int id, tdc;
	float t;
	int n=0;
	clasTAGE_t *TAGE=getBank(&bcs_, "TAGE");

	if(!TAGE)return n;

	for(i=0;i<TAGE->bank.nrow;i++){		
		id  = TAGE->tage[i].id;
		if((id>>8) != 2)continue;  /* make sure id has sd1 = 2 */
		id&=0xFF; /* mask off channel number only */
		t_hits[n].status = 0;
		tdc = TAGE->tage[i].tdc;
		t   = TID2NS*(float)tdc;	/* convert to nanoseconds  */
		t  += TID_TIME_OFFSETS[id-1];
		t_hits[n].id  = id;
		t_hits[n].tdc = tdc;
		t_hits[n++].t = t;	/* nanoseconds  */
		if(n>=MAX_THITS){
			fprintf(stderr,"%s:%d Too many t-counter rows in TAGE bank. Dropping some.\n",__FILE__,__LINE__);
			break;
		}
	}

	/* Sort the hits by counter id, then time. This makes it easier to change these into fine channels later */
	qsort(t_hits,n,sizeof(TAG_HIT_t),(QSORT_PROC_t*)tagM_TCounterHitCompare);

	return n;
}



int tagM_TCounterHitCompare(TAG_HIT_t *hit1, TAG_HIT_t *hit2)
{
	static int n_errs=0;
	
	/* Sort by id first */
	if(hit1->id != hit2->id)return hit1->id - hit2->id;

	/* Sort by time second */
	if(hit1->t != hit2->t)return (int)(2.0*(hit1->t - hit2->t));

	/* Should never get here, but just in case ... */
	if(n_errs++<100)fprintf(stderr,"%s:%d Identical hits in TAGE bank!\n",__FILE__,__LINE__);
	return 0;
}



