
/*
 * Make TAGM banks
 * 
 */


#include <tagM.h>

/* Create TAGM banks based on ET coincidences made with multi-hit TDCs in TAGE crate.
 * Group=0 is for real coincidences while group=1 is for accidentals
 */
int tagM_make_TAGM(int n_et_coincidences, TAG_ET_COINCIDENCE_t* et_coincidences, int group)
{
	int i;
	clasTAGM_t *TAGM;

	if(TAGM = makeBank(&bcs_, "TAGM", group, sizeof(tagm_t)/sizeof(float), n_et_coincidences)){
		for(i=0;i<n_et_coincidences;i++){
			/* since the tagm_t and TAG_ET_COINCIDENCE_t data structures are defined independantly,
			 * I copy the fields individually rather than by memcpy in case one of these structures
			 * is changed some time in the future.
			 */
			 TAGM->tagm[i].energy = et_coincidences[i].energy;
			 TAGM->tagm[i].t      = et_coincidences[i].t;
			 TAGM->tagm[i].e_t    = et_coincidences[i].e_t;
			 TAGM->tagm[i].status = et_coincidences[i].status;
			 TAGM->tagm[i].tid    = et_coincidences[i].tid;
			 TAGM->tagm[i].eid    = et_coincidences[i].eid;
		}
	}

	return 0;

}

