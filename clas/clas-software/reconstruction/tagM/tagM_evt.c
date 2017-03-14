

/* 
 * Tagger event reconstruction from Multi-hit T-counter TDCs
 * This is to be called during the main event loop.
 */

#include <tagM.h>



int tagM_evt(void)
{
	TAG_HIT_t e_hits[MAX_EHITS];				/* info from the 384 physical detectors */
	TAG_HIT_t t_hits[MAX_THITS];				/* info from the 61 physical detectors using LRS1877 MH-TDC */
	TAG_HIT_t e_channel_hits[MAX_EHITS];	/* info from the 767 channels from detector overlap */
	TAG_HIT_t t_channel_hits[MAX_EHITS];	/* info from the 121 channels from detector overlap */
	TAG_ET_COINCIDENCE_t et_coincidences[MAX_ETHITS];
	TAG_ET_COINCIDENCE_t et_accidentals[MAX_ETHITS];
	int n_e_hits=0;
	int n_t_hits=0;
	int n_e_channel_hits=0;
	int n_t_channel_hits=0;
	int n_et_coincidences=0;
	int n_et_accidentals=0;

	/* Get TDC information for E and T counters */
	n_e_hits				= tagM_GetECounterHits(e_hits);
	n_t_hits				= tagM_GetTCounterHits(t_hits);

	/* Convert detector info into fine channel info */
	n_e_channel_hits	= tagM_GetEChannelHits(n_e_hits, e_hits, e_channel_hits);
	n_t_channel_hits	= tagM_GetTChannelHits(n_t_hits, t_hits, t_channel_hits);

	/* Find all real E-T Coincidences */
	n_et_coincidences = tagM_GetETCoincidences(
									n_e_channel_hits,
									e_channel_hits,
									n_t_channel_hits,
									t_channel_hits,
									et_coincidences,
									0.0);

	/* Find all accidental E-T Coincidences */
	n_et_accidentals = tagM_GetETCoincidences(
									n_e_channel_hits,
									e_channel_hits,
									n_t_channel_hits,
									t_channel_hits,
									et_accidentals,
									100.0);

	/* Make the TAGM banks		group=0  real coindcidences
	 * 								group=1	accidental coincidences
	 */
	 tagM_make_TAGM(n_et_coincidences, et_coincidences, 0);
	 tagM_make_TAGM(n_et_accidentals , et_accidentals , 1);

	return 0;	
}





