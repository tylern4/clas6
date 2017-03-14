//$Id: tag_cpp_wrapper.cc,v 1.11 2006/03/15 14:46:20 pasyuk Exp $
#include "tag_cpp_wrapper.h"

//  extern "C" {

//    typedef struct {
//      int ectcmapsMM,tmin[384],tmax[384],tbinmin[767];
//    } ectcmaps_t;
//    extern ectcmaps_t ectcmaps_;
  
//    typedef struct {
//      float Tboundary[122],Eboundary[768],Eaverage[767],Edeviation[767];
//      int tagEboundsMM,tagTboundsMM;
//    } tagenergyboundaries_t;
//    extern tagenergyboundaries_t tagenergyboundaries_;
  
//    typedef struct{
//      int Tagger_NO_LR, Tagger_Raw_Format, Tagger_NO_E;
//      int Tagger_debug_begin,Tagger_debug_end;
//      int Tagger_DSD_debug,Tagger_parms_input;
//      int Tagger_PC_TDC,Tagger_PS_TDC,Tagger_TAC_TDC;
//      int Tagger_warning;
//      int Tagger_ETmatch;
//      int Tagger_convert;
//    } tagtcl_t;
//    extern tagtcl_t tagtcl_;
  
//    typedef struct {
//      double ADJ_T_COINC,ADJ_E_COINC,ET_window,E_TDC_MIN,E_TDC_MAX,TAG_DSD_window;
//      double TAG_ST_window,TAG_DSD_Xtrawindow,TAG_ST_Xtrawindow,PS_COINC_WINDOW;
//    } TagParam_t;
//    extern TagParam_t tagparam_;
  
//    typedef struct {  
//      float posTpeakL[61],posTpeakR[61],posEpeak[384];
//      int tagTDCbaseEPeakMM,tagTDCbaseTPeakMM;    
//    } TagTDCbasePeak_t;
//    extern TagTDCbasePeak_t tagtdcbasepeak_;
  
//    typedef struct {  
//      float TagTCi[121];
//      int TagCivalMM;
//    }  TagCival_t;
//    extern TagCival_t tagcival_;
  
//    typedef struct {
//      int tac_ped_lt,tac_ped_rt,tac_ped_lb,tac_ped_rb;
//      int tac_ped_sum1,tac_ped_sum2,tac_ped_sum3;
//      float tac_sca_lt,tac_sca_rt,tac_sca_lb,tac_sca_rb;
//      float tac_sca_sum1,tac_sca_sum2,tac_sca_sum3;
//      float tac_tdc_w,posTACpeak;
//      int pc_ped_ve,pc_ped_mn,pc_ped_lt,pc_ped_rb,pc_ped_lb,pc_ped_rt;
//      float pc_TDC_w,posPCpeak;
//      int ps_ped[8];
//      float ps_tdc_w[8],posPSpeak[8];
//      float timwk1,timwk2,timwk3,PSthreshold;
//      int TagDSDCalMM;
//    } TagDSDCal_t;
//    extern TagDSDCal_t tagdsdcal_;
  
//    typedef struct {
//      float tag2tof;
//    } Tag2Tof_t;
//    extern Tag2Tof_t tagtotof_;
  
//    typedef struct {
//      float ECchWidth,TCchWidthL[61],TCchWidthR[61];
//      int TagTDCCalTMM, TagTDCCalEMM;
//    } TagTDCCal_t;
//    extern TagTDCCal_t tagtdccal_;  

//    typedef struct {
//      int E_hw_status[384];
//      int TL_hw_status[61];
//      int TR_hw_status[61];
//    } TagHWstat_t;
//    extern TagHWstat_t hwstatus_;
//  }

//______________________________________________________________________________
int tag_brun(int nrun) {
  tag_brun_(&nrun);
  return 1;
}

//______________________________________________________________________________
int tag_brun_scaler() {
  char ok;
  tag_brun_scaler_(&ok);
  return 1;
}

//______________________________________________________________________________
int tag_evnt() {
  tag_evnt_();
  return 1;
}

//______________________________________________________________________________
int tag_erun() {
  tag_erun_();
  return 1;
}

//______________________________________________________________________________
int tag_init() {
  tag_init_();
  return 1;
}

//______________________________________________________________________________
int tag_scaler() {
  tag_scaler_();
  return 1;
}

//______________________________________________________________________________
int tag_bin(int ecounter,int nbcoutner) {
  return tag_bin_(&ecounter,&nbcoutner);
}

//______________________________________________________________________________
int tag_unbin(int bin) {
  return tag_unbin_(&bin);
}

//______________________________________________________________________________
float tag_deltae(int E_bin) {
  return tag_deltae_(&E_bin);
}

//______________________________________________________________________________
int tag_nbc_bin(int bin) {
  return tag_nbc_bin_(&bin);
}

//______________________________________________________________________________
int tagtcl_set_def() {
  tagtcl_set_def_();
  return 1;
}

//______________________________________________________________________________
float TTagSwitches::SetAdjacentT_TimeCoinc(float win) {
  tagparam_.ADJ_T_COINC = win;
  return tagparam_.ADJ_T_COINC;
}

//______________________________________________________________________________
float TTagSwitches::GetAdjacentT_TimeCoinc() {
  return tagparam_.ADJ_T_COINC;
}
//______________________________________________________________________________
float TTagSwitches::SetAdjacentE_TimeCoinc(float win) {
  tagparam_.ADJ_E_COINC = win;
  return tagparam_.ADJ_E_COINC;
}
//______________________________________________________________________________
float TTagSwitches::GetAdjacentE_TimeCoinc() {
  return tagparam_.ADJ_E_COINC;
}
//______________________________________________________________________________
float TTagSwitches::SetE_T_Coinc(float win) {
  tagparam_.ET_window = win;
  return tagparam_.ET_window;
}
//______________________________________________________________________________
float TTagSwitches::GetE_T_Coinc() {
  return tagparam_.ET_window;
}
//______________________________________________________________________________
float TTagSwitches::SetE_TDC_Min(float win) {
  tagparam_.E_TDC_MIN = win;
  return tagparam_.E_TDC_MIN;
}
//______________________________________________________________________________
float TTagSwitches::GetE_TDC_Min() {
  return tagparam_.E_TDC_MIN;
}
//______________________________________________________________________________
float TTagSwitches::SetE_TDC_Max(float win) {
  tagparam_.E_TDC_MAX = win;
  return tagparam_.E_TDC_MAX;
}
//______________________________________________________________________________
float TTagSwitches::GetE_TDC_Max() {
  return tagparam_.E_TDC_MAX;
}
//______________________________________________________________________________
float TTagSwitches::SetT_DSD_Win(float win) {
  tagparam_.TAG_DSD_window = win;
  return tagparam_.TAG_DSD_window;
}
//______________________________________________________________________________
float TTagSwitches::GetT_DSD_Win() {
  return tagparam_.TAG_DSD_window;
}
//______________________________________________________________________________
float TTagSwitches::SetT_ST_Win(float win) {
  tagparam_.TAG_ST_window = win;
  return tagparam_.TAG_ST_window;
}
//______________________________________________________________________________
float TTagSwitches::GetT_ST_Win() {
  return tagparam_.TAG_ST_window;
}
//______________________________________________________________________________
float TTagSwitches::SetPS_PaddleCoinc(float win) {
  tagparam_.PS_COINC_WINDOW = win;
  return tagparam_.PS_COINC_WINDOW;
}
//______________________________________________________________________________
float TTagSwitches::GetPS_PaddleCoinc() {
  return tagparam_.PS_COINC_WINDOW;
}
//______________________________________________________________________________
char TTagSwitches::SetDisregardE() {
  tagtcl_.Tagger_NO_E = 1;
  return GetUseE();
}
//______________________________________________________________________________
char TTagSwitches::SetRequireE() {
  tagtcl_.Tagger_NO_E = 0;
  return GetUseE();
}
//______________________________________________________________________________
char TTagSwitches::GetUseE() {
  if (tagtcl_.Tagger_NO_E==0) return 1;
  return 0;
}
//______________________________________________________________________________
char TTagSwitches::SetNOLR() {
  tagtcl_.Tagger_NO_LR = 1;
  return GetUseLR();
}
//______________________________________________________________________________
char TTagSwitches::SetUseLR() {
  tagtcl_.Tagger_NO_LR = 0;
  return GetUseLR();
}
//______________________________________________________________________________
char TTagSwitches::GetUseLR() {
  if (tagtcl_.Tagger_NO_LR==0) return 1;
  return 0;
}

//______________________________________________________________________________
char TTagSwitches::SetTaggerDebug(int begin,int end) {
  tagtcl_.Tagger_debug_begin = begin;
  tagtcl_.Tagger_debug_end = end;
  if (begin<=end) return 1;
  return 0;
}
//______________________________________________________________________________
char TTagSwitches::GetDebug() {
  if (tagtcl_.Tagger_debug_begin<=tagtcl_.Tagger_debug_end) return 1;
  return 0;
}
//______________________________________________________________________________
char TTagSwitches::SetDSD_Debug() {
  tagtcl_.Tagger_DSD_debug = 1;
  return GetDSD_Debug();
}
//______________________________________________________________________________
char TTagSwitches::SetDSD_NoDebug() {
  tagtcl_.Tagger_DSD_debug = 0;
  return GetDSD_Debug();
}
//______________________________________________________________________________
char TTagSwitches::GetDSD_Debug() {
  if (tagtcl_.Tagger_DSD_debug==0) return 0;
  return 1;
}
//______________________________________________________________________________
char TTagSwitches::SetPC_Require_TDC() {
  tagtcl_.Tagger_PC_TDC = 1;
  return GetPC_Use_TDC();
}
//______________________________________________________________________________
char TTagSwitches::SetPC_Disregard_TDC() {
  tagtcl_.Tagger_PC_TDC = 0;
  return GetPC_Use_TDC();
}
//______________________________________________________________________________
char TTagSwitches::GetPC_Use_TDC() {
  if (tagtcl_.Tagger_PC_TDC==0) return 0;
  return 1;
}
//______________________________________________________________________________
char TTagSwitches::SetPS_Require_TDC() {
  tagtcl_.Tagger_PS_TDC = 1;
  return GetPS_Use_TDC();
}
//______________________________________________________________________________
char TTagSwitches::SetPS_Disregard_TDC() {
  tagtcl_.Tagger_PS_TDC = 0;
  return GetPS_Use_TDC();
}
//______________________________________________________________________________
char TTagSwitches::GetPS_Use_TDC() {
  if (tagtcl_.Tagger_PS_TDC==0) return 0;
  return 1;
}

//______________________________________________________________________________
char TTagSwitches::SetTAC_Require_TDC() {
  tagtcl_.Tagger_TAC_TDC = 1;
  return GetTAC_Use_TDC();
}
//______________________________________________________________________________
char TTagSwitches::SetTAC_Disregard_TDC() {
  tagtcl_.Tagger_TAC_TDC = 0;
  return GetTAC_Use_TDC();
}
//______________________________________________________________________________
char TTagSwitches::GetTAC_Use_TDC() {
  if (tagtcl_.Tagger_TAC_TDC==0) return 0;
  return 1;
}
//______________________________________________________________________________
int TTagSwitches::SetTaggerWarning(int mod) {
  tagtcl_.Tagger_warning = mod;
  return GetTaggerWarning();
}
//______________________________________________________________________________
int TTagSwitches::GetTaggerWarning() {
  return tagtcl_.Tagger_warning;
}
//______________________________________________________________________________
int TTagSwitches::GetETMapTMin(int ec) {
  return ectcmaps_.tmin[ec-1];
}
//______________________________________________________________________________
int TTagSwitches::GetETMapTMax(int ec) {
  return ectcmaps_.tmax[ec-1];
}
//______________________________________________________________________________
int TTagSwitches::SetETMapTMin(int ec,int tmin) {
  if (ec>0 && ec<385) ectcmaps_.tmin[ec-1] = tmin;
  return ectcmaps_.tmin[ec-1];
}
//______________________________________________________________________________
int TTagSwitches::SetETMapTMax(int ec,int tmax) {
  if (ec>0 && ec<385) ectcmaps_.tmax[ec-1] = tmax;
  return ectcmaps_.tmax[ec-1];
}
//______________________________________________________________________________
int TTagSwitches::WidenETMap(int NTc) {
  for (int i=0;i<384;i++) {
    ectcmaps_.tmin[i] -= NTc;
    ectcmaps_.tmax[i] += NTc;
    if (ectcmaps_.tmin[i]<1) ectcmaps_.tmin[i] = 1;
    if (ectcmaps_.tmax[i]>61) ectcmaps_.tmin[i] = 61;
    if (ectcmaps_.tmin[i] > ectcmaps_.tmax[i]) {
      int mean = (ectcmaps_.tmin[i] + ectcmaps_.tmax[i])/2;
      ectcmaps_.tmin[i] = mean;
      ectcmaps_.tmax[i] = mean;
    }
  }
  return 1;
}
//______________________________________________________________________________
float TTagSwitches::GetEaverage(int eb) {
  return tagenergyboundaries_.Eaverage[eb];
}
//______________________________________________________________________________
float TTagSwitches::SetEaverage(int eb,float eav) {
  if (eb>0 && eb<768) tagenergyboundaries_.Eaverage[eb-1] = eav;
  return tagenergyboundaries_.Eaverage[eb-1];
}


