      SUBROUTINE RECEVT
c
c_begin_doc
c  RCS ID string
c  $Id: recevt.F,v 1.44 2009/03/31 19:42:26 myurov Exp $
c
c  Documentation for subroutine RECEVT
c
c  Purpose:
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Tue Mar 14 16:27:03 EST 1995
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
#include "bcs.inc"
#include "run_control.inc"
#include "rhist.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: recevt.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.44 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2009/03/31 19:42:26 $')
      PARAMETER ( CAUTHO = '$Author: myurov $')
      DATA CRCSID/
     1'$Id: recevt.F,v 1.44 2009/03/31 19:42:26 myurov Exp $
     3'/
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RECEVT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
c
      LOGICAL L_TRIG, L_TBT
      INTEGER ind, nami
      INTEGER BEAM_SAVE
      INTEGER MLINK
      INTEGER MAMIND


c_end_var
c
c  executable code for routine RECEVT:
c  -------------------------------------

      l_trig = .false.
      l_tbt  = .false.

      if(LPIPLTDC) then
          CALL COPY_PIPELINE_TDC
      endif

      IF (LCC_DO)  CALL CC_BEVT
	if(lst_do) call st_bevt
c
c  ALLOW the USER to get the FIRST crack at the DATA
c
      IF (lusr0_do) call user_bevt

c
c  Call bank RF analysis -generates the RF for use with tagger, st, seb.
c

      if (LRF_DO) call rf_evnt

c
c  call bank analysis - CL01 for use with tagger, tof, and st packages
c
      CALL make_cl01_bank
c
c  Perform Calorimeter reconstruction
c
      IF (LEGN_DO) CALL EC_EVNT
c
c  Perform Large Angle Calorimeter reconstruction
c
      IF (LEC1_DO) CALL EC1_EVNT
c
c  Perform Inner Calorimeter reconstruction
c
      IF (LIC_DO) CALL IC_EVNT
c
c  Perform Scintillation Hodoscope reconstruction
c
      IF (LSH_DO) CALL SH_EVNT
c
c  perform tagger reconstruction
c
      IF (LTAGGER_DO) CALL TAG_EVNT
c
c  perform tracking reconstruction HIT BASED TRACKING
c
      IF (LTRK_DO) CALL TRK_EVNT_HBT
c
c  Perform Time of Flight reconstruction
c
      IF (LTOF_DO) CALL make_sc_banks
c
c  Perform Start Counter reconstruction
c
      if (lst_do) then
         if(start_tb)then
            call st_evnt(1)
         else
            call st_evnt(0)
         endif
      endif
c
c  Perform Cerenkov reconstruction
c
      IF (LCC_DO) CALL CC_EVNT
c
c  perform the track matching
c
      IF (LSEB_DO) CALL RUN_BUILDER(1,L_TRIG)
c
c     alternate HBID - hit based id do seb or this
c 
      IF (LHBID_DO) THEN
         call bdrop(iw,'HBIDTBIDMVRTVERTCC01')
         call make_CC01_banks()
         call make_hbid_bank()
         if(lpid_make_trks)then
	   call make_TRKS_banks()
	   nami=mamind(iw,'TRKS')
           if(IW(nami).gt.0)L_TRIG = .TRUE.
         endif
      ENDIF
c
c  perform tracking reconstruction TIME BASED TRACKING
c
      IF(L_TRIG)THEN
         IF (LTRK_DO.and.L_TRIG) CALL TRK_EVNT_TBT(L_TBT)

c Reconstruct tracks that hit only region 1
         if (LREG1_DO) THEN
            call trk_region1
            if (LST_DO) call st_evnt(2)            
         endif
            
c
c Do BONUS GEM reconstruction
c (here we're tied to having time based recon)

         if (LGEM_DO) call gem_evnt

c
c  alternate pid banks
c
         if (LTOF_DO) THEN
            call make_sc_banks_tb()
         endif
         if (LST_DO) THEN
            call st_evnt(1)            
         endif

c
c  perform the event builder with TBT and new TOF 
c
         L_TBT=L_TBT.or.Start_tb
         IF (LSEB_DO)   CALL RUN_BUILDER(2,L_TBT) 

         IF (LTBID_DO) THEN
            call make_tbid_group(1)
            call make_vert()
            call make_mvrt()
            call make_part_group(1)
         ENDIF
c do GPID 
         IF (LGPID_DO) THEN
            call makegpid(1,0)
         ENDIF

c make PART/TBID without start counter
         IF(LTBID_NOST_DO) THEN
            CALL get_beam_type(BEAM_SAVE) ! save beam type
            CALL put_beam_type(3) ! set "g2b" beam type
            CALL make_tbid_group(2)
            CALL make_part_group(2)
            CALL put_beam_type(BEAM_SAVE) ! restore beam type
         ENDIF

c     
c  now perform users routine
c
      ENDIF
      IF (LUSR1_DO) CALL USER_EVNT
c
c  Done with reconstruction call event display
c
      IF (LDISPLAY .OR. LDISPLAY_ALL) THEN
        CALL RDISPLAY
        LDISPLAY = .FALSE.
      ENDIF
c
      call refill
1776  continue
c
      RETURN
      END
c
c------------------------------------------------------------------------------




