/* clas_common.h */

#define NBCS 700000
 
typedef struct bcs_common {
        int junk[5];
	int iw[NBCS];
	} BCScommon;

#define NWCS 700000
 
typedef struct wcs_common {
        int junk[5];
	int jw[NWCS];
	} WCScommon;

extern BCScommon bcs_;
extern WCScommon wcs_;
