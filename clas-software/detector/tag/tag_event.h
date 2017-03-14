//$Id: tag_event.h,v 1.3 2006/03/15 14:47:59 pasyuk Exp $
// image of common blocks defined in event.CMN 

#define Tag_max_hit 2000
#define MAXTC 2000
#define MAXEC 2000
#define MAXECTC 2000
#define MAXTAGR 2000
#define MAXHITS 2048 

// status bits
#define TL_PRESENT    0x01
#define TR_PRESENT    0x02
#define TLR_PRESENT   0x03
#define E_PRESENT     0x04
#define MULTIPLE_HITS 0x08
#define TRIPPLE_T     0x10
#define MULTIPLE_T    0x20    //E-hit matched to more than one T-hit
#define MULTIPLE_E    0x40    //T-hit matched to more than one E-hit



typedef struct {
  int Tag_T_Nbhits;
  int tc_id[MAXTC];
  int tc_val_left[MAXTC];
  int tc_val_right[MAXTC];
  int MOR_tdc;
} TagTCraw_t;

extern TagTCraw_t tagtcraw_; // common/TagTCraw/ 

typedef struct {
  int Tgtl_nbhits;
  int itc_id_left[MAXHITS];
  int itc_val_left[MAXHITS];
  int tcl_used[MAXHITS];
} TgtLraw_t;

extern TgtLraw_t tgtlraw_; // common/tgtlraw/

typedef struct {
  int Tgtr_nbhits;
  int itc_id_right[MAXHITS];
  int itc_val_right[MAXHITS];
  int tcr_used[MAXHITS];
} TgtRraw_t;

extern TgtRraw_t tgtrraw_; // common/tgtrraw/

typedef struct {
  int Tag_E_Nbhits;
  int ec_id[MAXEC];
  int ec_val[MAXEC];
} TagECraw_t;

extern TagECraw_t tageraw_; // common/TagECraw/ 

typedef struct {
  int NBgoodT;
  int Good_T_id[MAXTC];
  float Good_T_L_val[MAXTC];
  float Good_T_R_val[MAXTC];
} TagTCfiltered_t;

extern TagTCfiltered_t tagtcfiltered_; // common/tagtcfiltered/

typedef struct {
  int NBgoodE;
  int Good_E_id[MAXEC];
  float Good_E_val[MAXEC];
} TagECfiltered_t;

extern TagECfiltered_t tagecfiltered_; // common/tagecfiltered/

typedef struct {
  int NbGoodTL;
  int NbGoodTR;
  int Good_TL_id[MAXTC];
  float Good_TL_val[MAXTC];
  int Good_TR_id[MAXTC];
  float Good_TR_val[MAXTC];
} TGTLRconverted_t;

extern TGTLRconverted_t tgtlrconverted_;

typedef struct {
  int Nb_matched_Hits;
  int Nb_matched_Ts;
  int Nb_matched_Es[MAXTC];
  int Matched_T_Id[MAXTC];
  float Matched_T_L_val[MAXTC];
  float Matched_T_R_val[MAXTC];
  int Matched_E_Id[MAXECTC][MAXTC];
  float Matched_E_val[MAXECTC][MAXTC];
} TagETmatch_t;

extern  TagETmatch_t tagetmatch_;

typedef struct {
  int Nb_coinc_Hits;
  int NB_coinc_Ts;
  int NB_coinc_Es[MAXTC];
  int coinc_T_Id[MAXTC];
  float coinc_T_L_val[MAXTC];
  float coinc_T_R_val[MAXTC];
  int coinc_E_Id[MAXTC][MAXECTC];
  float coinc_E_val[MAXTC][MAXECTC];
} TagETcoinc_t;

extern TagETcoinc_t tagetcoinc_;

typedef struct {
  int NB_bin_hits;
  int NB_bin_Ts;
  int NB_bin_Es[MAXTC];
  int bin_T_Id[MAXTC];
  float bin_T_L_val[2][MAXTC];
  float bin_T_R_val[2][MAXTC];
  int bin_E_Id[MAXTC][MAXECTC];
  float bin_E_val[2][MAXTC][MAXECTC];
} TagETbin_t;

extern TagETbin_t tagetbin_;

typedef struct {
  float Mean_T_val[2][MAXTC];
  float RF_T_diff[2][MAXTC];
  float Diff_T_val[2][MAXTC];
  float T_time[2][MAXTC];
} TagTcorr_t;

extern TagTcorr_t tagtcorr_;

typedef struct {
  float PhotonEnergy[MAXTAGR];
  float TimeEvent[MAXTAGR];
  float TimeEventcor[MAXTAGR];
  int TAGR_TID[MAXTAGR];
  int TAGR_EID[MAXTAGR];
  int HitStatus[MAXTAGR];
  int NB_TAGR_hits;
} TagTAGRBOS_t;

extern TagTAGRBOS_t tagtagrbos_;

