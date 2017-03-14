//$Id: tagtcl.h,v 1.2 2006/03/15 14:43:54 pasyuk Exp $
// image of tagtcl.CMN

typedef struct {
  int Tagger_NO_LR;
  int Tagger_Raw_Format; 
  int Tagger_NO_E;
  int Tagger_debug_begin;
  int Tagger_debug_end;
  int Tagger_DSD_debug; 
  int Tagger_parms_input;
  int Tagger_PC_TDC;
  int Tagger_PS_TDC;
  int Tagger_TAC_TDC;
  int Tagger_warning;
  int Tagger_ETmatch;
  int Tagger_convert;
  int Tagger_correct;
  int Tagger_energy_correct;
  char Tagger_tpeaks_file_name[128];
  char Tagger_ecorr_name[128];
} tagtcl_t;
extern tagtcl_t tagtcl_;
  
