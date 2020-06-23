        subroutine tagcal_read_map(ok,irun,ttranstable)
c
c_begin_doc
c  RCS ID string
c  $Id: tagcal_read_map.F,v 1.14 2006/11/16 22:23:26 pasyuk Exp $
c
c  Documentation for subroutine tagcal_read_map
c
c  Purpose: 
c  --------
c       Attempts to read calibration data 
c
c  Calling Sequence: 
c  ----------------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters:  NONE
c  -----------------
c
c  Called from:  tag_brun
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Claude MARCHAND      Created:  
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "bcs.inc"
#include "tag_general.PAR"
#include "tagtcl.CMN"
#include "tagcal.CMN"
#include "tagRunControl.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      logical ok,ttranstable
      integer irun
c     
c     Local pre-defined variables:
c     ---------------------------
c     RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: tagcal_read_map.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.14 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 2006/11/16 22:23:26 $')
      PARAMETER (CAUTHO= '$Author: pasyuk $')
c     Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tagcal_read_map')
      PARAMETER (CRAUTH='Francois Roudot')
c     
c     Local tag defined variables:
c     ----------------------------
      
      INTEGER irec,run0,run1
      character*60 fname
      real pc_ped(6),ps_walk(3),tac_ped(7),tac_sca(7)
      integer i
      integer tmp_int
      
c     _end_var
c     
c     executable code for routine tagcal_read_file:
c     ------------------------------------
c     
c     
      crmess = '  '
      
      ok= .true.
      
      call revinm('CLAS_PARMS','Maps/TAG_CALIB.map',fname)
      
c     
c     ----------------------- T Translation Table
c     
      
      if (ttranstable.and.TagTTranslateMM.eq.0) then
         TagTTranslateMM = 1
         TAGTTranslOK = 1
         
         call map_get_int(fname, 'tag_t', 'transl_to_table', 122,
     +        TTranslateTo, irun, run0)
         CRMESS='Reading T translation table from the Map Manager'
         CALL RECMES(CRNAME,'I',CRMESS)
         if (run0.lt.0) then
            CRMESS='Problem while reading T translation table from Map'
            CALL RECMES(CRNAME,'W',CRMESS)
            ok=.false.
            use_tgtlr = 0
            CRMESS='Reading T translation table from the Map Manager'
         endif
      endif        
    
c     
c     -------------------- old/new T-counter TDC flag  
c
      
      call map_get_int(fname, 'tag_t', 'use_tgtlr', 1,
     +     use_tgtlr, irun, run0)
      CRMESS='Reading T old/new TDC flag from the Map Manager'
      CALL RECMES(CRNAME,'I',CRMESS)
      if (run0.lt.0) then
         CRMESS='Problem while reading T old/new TDC flag from Map'
         CALL RECMES(CRNAME,'W',CRMESS)
         ok=.false.
      endif
      if(use_tgtlr .eq. 0) then
         CRMESS='Will use TDCs from TAGT bank'
      else
         CRMESS='Will use TDCs from TGTL/TGTR banks'
      endif
      CALL RECMES(CRNAME,'I',CRMESS)

c     
c     -------------------- ET geometry map source
c

c old version of the code with single hit TDC for T-counters 
c reads ET map from file. This will not change
c For new version of the code with pipeline TDC for T-counters we 
c will have two options: read from file as the old code does or read map from 
c calibration database.  
      et_map_from_db = 0
      if (use_tgtlr .eq. 1) then
c for old TDC we would not even try to that
         call map_get_int(fname, 'tag_t', 'et_map_from_db', 1,
     +        tmp_int, irun, run0)
         CRMESS='Reading ET map source flag from caldb'
         CALL RECMES(CRNAME,'I',CRMESS)
         if (run0.lt.0) then
            CRMESS='Problem while reading ET map source flag from caldb'
            CALL RECMES(CRNAME,'W',CRMESS)
            ok=.false.
         endif
         et_map_from_db = tmp_int
         if(et_map_from_db .eq. 0) then
            CRMESS='Will read ET map from file'
         else
            CRMESS='Will read ET map from caldb'
         endif
         CALL RECMES(CRNAME,'I',CRMESS)
      endif
      
c     
c     -------------------- T-counter time shift correction flag  
c     
      
      do_correct = 0
      do i=1,61
         tl_ref(i)=0
         tr_ref(i)=0
      enddo      
         call map_get_int(fname, 'tag_t', 'do_tcorr', 1,
     +     do_correct, irun, run0)
      CRMESS='Reading T do_tcorr from the Map Manager'
      CALL RECMES(CRNAME,'I',CRMESS)
      if (run0.lt.0) then
         CRMESS='Problem while reading do_tcorr from Map'
         CALL RECMES(CRNAME,'W',CRMESS)
         ok=.false.
      endif
      if(do_correct .eq. 0) then
         CRMESS='Will not do time shift corrections'
         CALL RECMES(CRNAME,'I',CRMESS)
      else
         CRMESS='do_tcorr is set '
         CALL RECMES(CRNAME,'I',CRMESS)
         call map_get_int(fname, 'tag_t', 'tl_ref', 61,
     +        tl_ref, irun, run0)
         if (run0.lt.0) then
            CRMESS='Problem while reading TL_ref from Map'
            CALL RECMES(CRNAME,'F',CRMESS)
            ok=.false.
         endif
         call map_get_int(fname, 'tag_t', 'tr_ref', 61,
     +        tr_ref, irun, run0)
         if (run0.lt.0) then
            CRMESS='Problem while reading TR_ref from Map'
            CALL RECMES(CRNAME,'F',CRMESS)
            ok=.false.
         endif         
      endif
            
c     
c     ------------------------  T TDC constants:
c     
      if (TagTDCCalTMM.eq.0) then
         TagTDCCalTMM = 1
         
         call map_get_float(fname, 'tag_t', 'slope_left', 61,
     +        TCchWidthL, irun, run0)
         CRMESS='Reading left T TDC constants from the Map Manager'
         CALL RECMES(CRNAME,'I',CRMESS)
         if (run0.lt.0) then
            CRMESS='Problem while reading left TDC constants from Map'
            CALL RECMES(CRNAME,'F',CRMESS)
            ok=.false.
            TagTDCCalTMM = 0
         endif
         
         call map_get_float(fname, 'tag_t', 'slope_right', 61,
     +        TCchWidthR, irun, run0)
         CRMESS='Reading right T TDC constants from the Map Manager'
         CALL RECMES(CRNAME,'I',CRMESS)
         if (run0.lt.0) then
            CRMESS='Problem while reading right TDC constants from Map'
            CALL RECMES(CRNAME,'F',CRMESS)
            ok=.false.
            TagTDCCalTMM = 0
         endif
      endif        
c     
c     ------------------------ E TDC constant:
c     
      
      if (TagTDCCalEMM.eq.0) then
         TagTDCCalEMM = 1
         
         call map_get_float(fname, 'tag_e', 'slope', 1, 
     +        ECchWidth, irun, run0)
         CRMESS='Reading E TDC constant from the Map Manager'
         CALL RECMES(CRNAME,'I',CRMESS)
         if (run0.lt.0) then
            CRMESS='Problem while reading E TDC constant from Map'
            CALL RECMES(CRNAME,'F',CRMESS)
            ok=.false.
            TagTDCCalEMM = 0
         endif
      endif        
      
c
c     ------------------------ Ci constants:
c     
      
      if (TagCivalMM.eq.0) then
         call map_get_float(fname, 'tag_t', 'ci', 121, 
     +        TagTCi, irun, run0)
         CRMESS='Reading Ci constants from the Map Manager'
         CALL RECMES(CRNAME,'I',CRMESS)
         if (run0.lt.0) then
            CRMESS='Problem while reading Ci constants from Map'
            CALL RECMES(CRNAME,'F',CRMESS)
            ok=.false.
            TagCivalMM = 0
         else
            TagCivalMM = 1
         endif
      endif
      
c     
c     ------------------------ T peaks positions constants:
c     

      if (tagTDCbaseTPeakMM.eq.0) then
         CRMESS='Reading T base peak constants from the Map Manager'
         CALL RECMES(CRNAME,'I',CRMESS)
         tagTDCbaseTPeakMM = 1
         call map_get_float(fname, 'tag_t', 'dt_left', 61,
     +        posTpeakL, irun, run0)
         if (run0.lt.0) then
            CRMESS=
     +      'Problem while reading left T base peak constants from Map'

            CALL RECMES(CRNAME, 'F' ,CRMESS)
            ok=.false.
            tagTDCbaseTPeakMM = 0
         endif

         call map_get_float(fname,'tag_t','dt_right',61,
     +        posTpeakR,irun,run0)
         if (run0.lt.0) then
            CRMESS=
     +      'Problem while reading right T base peak constants from Map'
            CALL RECMES(CRNAME,'F',CRMESS)
            ok=.false.
            tagTDCbaseTPeakMM = 0
         endif
      endif
      
c     
c     ------------------------ E peaks position constants:
c     !
      
      if (tagTDCbaseEPeakMM.eq.0) then
         CRMESS='Reading E base peak constants from the Map Manager'
         CALL RECMES(CRNAME,'I',CRMESS)
         tagTDCbaseEPeakMM = 1
         call map_get_float(fname, 'tag_e', 'dt', 384, 
     +        posEpeak, irun, run0)
         if (run0.lt.0) then
            CRMESS=
     +      'Problem while reading E base peak constants from Map'
            CALL RECMES(CRNAME,'F',CRMESS)
            ok=.false.
            tagTDCbaseEPeakMM = 0
         endif
      endif
      
      
c     
c     ------------------------ Hardware status:
c     !
      
      call map_get_int(fname, 'status', 'tage', 384, 
     +     E_hw_status, irun, run0)
      if (run0.lt.0) then
         CRMESS='Problem while reading E_hw status from Map'
         CALL RECMES(CRNAME,'F',CRMESS)
         ok=.false.
      endif
      call map_get_int(fname, 'status', 'tagt_left', 61,
     +     TL_hw_status, irun, run0)
      if (run0.lt.0) then
         CRMESS='Problem while reading TL_hw status from Map'
         CALL RECMES(CRNAME,'F',CRMESS)
         ok=.false.
      endif
      call map_get_int(fname, 'status', 'tagt_right', 61, 
     +     TR_hw_status, irun, run0)
      if (run0.lt.0) then
         CRMESS='Problem while reading TR_hw status from Map'
         CALL RECMES(CRNAME,'F',CRMESS)
         ok=.false.
      endif
      
      
c     call RecMes('TAG_BRUN','I',' read Map: '//fname)
      
c     
c     ------------------------ DSD calib. constants:
c     
      
      if (TagDSDCalMM.eq.0) then
         CRMESS='Reading DSD calibration constants from the Map Manager'
         CALL RECMES(CRNAME,'I',CRMESS)
         TagDSDCalMM  = 1
         run1 = 0
         call map_get_float(fname,'pc','ped',6,pc_ped,irun,run0)
         if (run0.lt.0) run1=-1
         call map_get_float(fname,'pc','tdc_w',1,pc_TDC_w,irun,run0)
         if (run0.lt.0) run1=-1
         call map_get_float(fname,'pc','peak',1,posPCpeak,irun,run0)
         if (run0.lt.0) run1=-1
         call map_get_int(fname,'ps','ped',8,ps_ped,irun,run0)
         if (run0.lt.0) run1=-1
         call map_get_float(fname,'ps','tdc_w',8,ps_tdc_w,irun,run0)
         if (run0.lt.0) run1=-1
         call map_get_float(fname,'ps','peak',8,posPSpeak,irun,run0)
         if (run0.lt.0) run1=-1
         call map_get_float(fname,'ps','Ethresh',1,PSthreshold,irun,run0)
         if (run0.lt.0) run1=-1
         call map_get_float(fname,'ps','walk',3,ps_walk,irun,run0)
         if (run0.lt.0) run1=-1
         call map_get_float(fname,'tac','ped',7,tac_ped,irun,run0)
         if (run0.lt.0) run1=-1
         call map_get_float(fname,'tac','tdc_w',1,tac_tdc_w,irun,run0)
         if (run0.lt.0) run1=-1
         call map_get_float(fname,'tac','peak',1,posTACpeak,irun,run0)
         if (run0.lt.0) run1=-1
         call map_get_float(fname,'tac','Escale',7,tac_sca,irun,run0)
         if (run0.lt.0) run1=-1
         if (run1.lt.0) then
            CRMESS=
     +      'Problem while reading DSD calibration constants from Map'
            CALL RECMES(CRNAME,'W',CRMESS)
            ok=.false.
            TagDSDCalMM = 0
         endif
         
         
c     ---- PC
         
         pc_ped_ve = pc_ped(1)
         pc_ped_mn = pc_ped(2)
         pc_ped_lt = pc_ped(3)
         pc_ped_rb = pc_ped(4)
         pc_ped_lb = pc_ped(5)
         pc_ped_rt = pc_ped(6)
         
c     ----- PS
         
         timwk1 = ps_walk(1)
         timwk2 = ps_walk(2)
         timwk3 = ps_walk(3)	
         
c     ---- TAC
         
         
         tac_ped_lt = tac_ped(1)
         tac_ped_rt = tac_ped(2)
         tac_ped_lb = tac_ped(3)	
         tac_ped_rb = tac_ped(4)	
         tac_ped_sum1 = tac_ped(5)	
         tac_ped_sum2 = tac_ped(6)	
         tac_ped_sum3 = tac_ped(7)
         
         
         tac_sca_lt = tac_sca(1)
         tac_sca_rt = tac_sca(2)
         tac_sca_lb = tac_sca(3)
         tac_sca_rb = tac_sca(4)
         tac_sca_sum1 = tac_sca(5)
         tac_sca_sum2 = tac_sca(6)
         tac_sca_sum3 = tac_sca(7)
         
      endif
c     
c     ---- overall alignment of the tagger by respect to the TOF
c     
      
      call map_get_float(fname,'tag2tof','value',1,tag2tof,irun,run0)
      
c
c ---- get energy correction table if requested
c
      do i=1,767
         ecor(i)=1.
      enddo
      if(Tagger_energy_correct .eq. 1) then
         call map_get_float(fname,'tag_e','ecor',767,ecor,irun,run0)
         if (run0.lt.0) then
            CRMESS=
     +      'Problem while reading Energy corrections from Map'
            CALL RECMES(CRNAME,'F',CRMESS)
            ok=.false.
         endif 
      endif

c     
c     ------------------------ E & T Energy boundaries
c     
      
c     not implemented yet
      
      RETURN
      END
c     
c------------------------------------------------------------------------------

